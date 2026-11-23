package scala.publish

import coursier.core.{Configuration, MinimizedExclusions, ModuleName, Organization}

import scala.xml.{Elem, XML}

class PomTests extends ScalaPublishTestSuite {

  private def pomWithDependency(scope: Option[Configuration]): String =
    Pom.create(
      organization = Organization("org.example"),
      moduleName = ModuleName("lib_3"),
      version = "1.0.0",
      dependencies = Seq((
        Organization("org.dep"),
        ModuleName("dep_3"),
        "2.0.0",
        scope,
        MinimizedExclusions.zero
      ))
    )

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
    val pom    = pomWithDependency(Some(Configuration.test))
    val parsed =
      coursier.core.compatibility.xmlParseSax(pom, new coursier.maven.PomParser).project
    val project = parsed.fold(err => fail(s"Error parsing POM: $err\n$pom"), identity)
    assertEquals(
      project.dependencies0.map { case (variant, dep) =>
        (variant.asConfiguration, dep.module.name.value)
      },
      Seq((Some(Configuration.test), "dep_3"))
    )
  }
}
