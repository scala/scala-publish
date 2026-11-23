package scala.publish

import coursier.core.{Configuration, MinimizedExclusions, ModuleName, Organization}

import scala.xml.{Elem, XML}

class PomTests extends ScalaPublishTestSuite {

  private def pomWithDependency(
    scope: Option[Configuration] = None,
    exclusions: MinimizedExclusions = MinimizedExclusions.zero
  ): String =
    Pom.create(
      organization = Organization("org.example"),
      moduleName = ModuleName("lib_3"),
      version = "1.0.0",
      dependencies = Seq((
        Organization("org.dep"),
        ModuleName("dep_3"),
        "2.0.0",
        scope,
        exclusions
      ))
    )

  private def parse(pom: String): coursier.core.Project =
    coursier.core.compatibility.xmlParseSax(pom, new coursier.maven.PomParser).project
      .fold(err => fail(s"Error parsing POM: $err\n$pom"), identity)

  private def exclusionsOf(pom: String): Seq[(String, String)] =
    (dependencyElem(pom) \ "exclusions" \ "exclusion").map { e =>
      ((e \ "groupId").text, (e \ "artifactId").text)
    }

  private def dependencyElem(pom: String): Elem = {
    val deps = XML.loadString(pom) \ "dependencies" \ "dependency"
    assertEquals(deps.length, 1, clue = pom)
    deps.head.asInstanceOf[Elem]
  }

  // Scala CLI passes `Some(Configuration(scope.name))` for non-main scopes (e.g. `publish --test`)
  for (scope <- Seq(Configuration.test, Configuration.provided, Configuration.compile))
    test(s"dependency scope ${scope.value} is rendered as <scope>${scope.value}</scope>") {
      val pom = pomWithDependency(Some(scope))
      assertEquals((dependencyElem(pom) \ "scope").map(_.text), Seq(scope.value), clue = pom)
    }

  test("no <scope> element when no configuration is passed") {
    val pom = pomWithDependency(None)
    assertEquals((dependencyElem(pom) \ "scope").length, 0, clue = pom)
  }

  test("dependency scope round-trips through coursier's POM parser") {
    val pom = pomWithDependency(Some(Configuration.test))
    assertEquals(
      parse(pom).dependencies0.map { case (variant, dep) =>
        (variant.asConfiguration, dep.module.name.value)
      },
      Seq((Some(Configuration.test), "dep_3"))
    )
  }

  private val specificExclusions = MinimizedExclusions(Set(
    (Organization("org.scala-lang"), ModuleName("scala3-library_3")),
    (Organization("com.example"), ModuleName("excluded")),
    (Organization("com.example"), ModuleName("another"))
  ))

  test("dependency exclusions are rendered, sorted by organization and name") {
    val pom = pomWithDependency(exclusions = specificExclusions)
    assertEquals(
      exclusionsOf(pom),
      Seq(
        "com.example"    -> "another",
        "com.example"    -> "excluded",
        "org.scala-lang" -> "scala3-library_3"
      ),
      clue = pom
    )
  }

  test("excluding all transitive dependencies is rendered as a * / * exclusion") {
    val pom = pomWithDependency(exclusions = MinimizedExclusions.one)
    assertEquals(exclusionsOf(pom), Seq("*" -> "*"), clue = pom)
  }

  test("no <exclusions> element when there are no exclusions") {
    val pom = pomWithDependency()
    assertEquals((dependencyElem(pom) \ "exclusions").length, 0, clue = pom)
  }

  test("dependency exclusions round-trip through coursier's POM parser") {
    for (exclusions <- Seq(specificExclusions, MinimizedExclusions.one)) {
      val pom = pomWithDependency(exclusions = exclusions)
      assertEquals(
        parse(pom).dependencies0.map(_._2.minimizedExclusions),
        Seq(exclusions),
        clue = pom
      )
    }
  }
}
