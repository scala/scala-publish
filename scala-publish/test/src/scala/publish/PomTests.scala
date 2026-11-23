package scala.publish

import coursier.core.{Configuration, MinimizedExclusions, ModuleName, Organization, Type}

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

  private def developersOf(developers: Seq[Pom.Developer]): Seq[Elem] = {
    val pom = Pom.create(
      organization = Organization("org.example"),
      moduleName = ModuleName("lib_3"),
      version = "1.0.0",
      developers = developers
    )
    (XML.loadString(pom) \ "developers" \ "developer").collect { case e: Elem => e }
  }

  test("developer email is rendered when provided, in Maven schema order") {
    val devs = developersOf(Seq(
      Pom.Developer("jdoe", "Jane Doe", "https://example.com/jdoe", Some("jane@example.com"))
    ))
    assertEquals(devs.length, 1)
    val dev = devs.head
    assertEquals(dev.child.collect { case e: Elem => e.label }, Seq("id", "name", "email", "url"))
    assertEquals((dev \ "email").text, "jane@example.com")
    assertEquals((dev \ "id").text, "jdoe")
    assertEquals((dev \ "name").text, "Jane Doe")
    assertEquals((dev \ "url").text, "https://example.com/jdoe")
  }

  test("no <email> element when the developer has no email") {
    val devs =
      developersOf(Seq(Pom.Developer("jdoe", "Jane Doe", "https://example.com/jdoe", None)))
    assertEquals(devs.length, 1)
    assertEquals(devs.head.child.collect { case e: Elem => e.label }, Seq("id", "name", "url"))
  }

  test("several developers keep their order and their own emails") {
    val devs = developersOf(Seq(
      Pom.Developer("a", "A", "https://example.com/a", Some("a@example.com")),
      Pom.Developer("b", "B", "https://example.com/b", None),
      Pom.Developer("c", "C", "https://example.com/c", Some("c@example.com"))
    ))
    assertEquals(
      devs.map(d => ((d \ "id").text, (d \ "email").map(_.text).headOption)),
      Seq("a" -> Some("a@example.com"), "b" -> None, "c" -> Some("c@example.com"))
    )
  }

  /** A POM with every argument Scala CLI passes to `Pom.create` (see `Publish.scala` there) */
  private lazy val fullPom: String = Pom.create(
    organization = Organization("org.example"),
    moduleName = ModuleName("lib_3"),
    version = "1.0.0",
    packaging = Some(Type.jar),
    description = Some("A library & <more>"),
    url = Some("https://github.com/example/lib"),
    name = Some("lib"),
    dependencies = Seq(
      (
        Organization("org.scala-lang"),
        ModuleName("scala3-library_3"),
        "3.3.6",
        None,
        MinimizedExclusions.zero
      ),
      (
        Organization("org.dep"),
        ModuleName("dep_3"),
        "2.0.0",
        Some(Configuration.test),
        MinimizedExclusions(Set((Organization("org.x"), ModuleName("y"))))
      )
    ),
    license = Some(Pom.License.apache2),
    scm = Some(Pom.Scm.gitHub("example", "lib")),
    developers = Seq(
      Pom.Developer("jdoe", "Jane Doe", "https://github.com/jdoe", Some("jane@example.com"))
    )
  )

  test("full POM (golden)") {
    val expected =
      """<?xml version="1.0" encoding="UTF-8"?>
        |<project xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd" xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
        |  <modelVersion>4.0.0</modelVersion>
        |  <groupId>org.example</groupId>
        |  <artifactId>lib_3</artifactId>
        |  <version>1.0.0</version>
        |  <packaging>jar</packaging>
        |  <url>https://github.com/example/lib</url>
        |  <description>A library &amp; &lt;more&gt;</description>
        |  <name>lib</name>
        |  <organization>
        |    <name>org.example</name>
        |    <url>https://github.com/example/lib</url>
        |  </organization>
        |  <licenses>
        |    <license>
        |      <name>Apache-2.0</name>
        |      <url>https://spdx.org/licenses/Apache-2.0.html</url>
        |      <distribution>repo</distribution>
        |    </license>
        |  </licenses>
        |  <scm>
        |    <url>https://github.com/example/lib.git</url>
        |    <connection>scm:git:github.com/example/lib.git</connection>
        |    <developerConnection>scm:git:git@github.com:example/lib.git</developerConnection>
        |  </scm>
        |  <developers>
        |    <developer>
        |      <id>jdoe</id>
        |      <name>Jane Doe</name>
        |      <email>jane@example.com</email>
        |      <url>https://github.com/jdoe</url>
        |    </developer>
        |  </developers>
        |  <dependencies>
        |    <dependency>
        |      <groupId>org.scala-lang</groupId>
        |      <artifactId>scala3-library_3</artifactId>
        |      <version>3.3.6</version>
        |    </dependency>
        |    <dependency>
        |      <groupId>org.dep</groupId>
        |      <artifactId>dep_3</artifactId>
        |      <version>2.0.0</version>
        |      <scope>test</scope>
        |      <exclusions>
        |        <exclusion>
        |          <groupId>org.x</groupId>
        |          <artifactId>y</artifactId>
        |        </exclusion>
        |      </exclusions>
        |    </dependency>
        |  </dependencies>
        |</project>""".stripMargin
    assertNoDiff(fullPom, expected)
  }

  test("full POM round-trips through coursier's POM parser") {
    val project = parse(fullPom)
    assertEquals(project.module.organization.value, "org.example")
    assertEquals(project.module.name.value, "lib_3")
    assertEquals(project.version0.asString, "1.0.0")
    assertEquals(project.packagingOpt, Some(Type.jar))
    assertEquals(project.info.description, "A library & <more>")
    assertEquals(project.info.homePage, "https://github.com/example/lib")
    assertEquals(
      project.info.licenseInfo.map(l => (l.name, l.url)),
      Seq(("Apache-2.0", Some("https://spdx.org/licenses/Apache-2.0.html")))
    )
    assertEquals(
      project.info.scm.map(s => (s.url, s.connection, s.developerConnection)),
      Some((
        Some("https://github.com/example/lib.git"),
        Some("scm:git:github.com/example/lib.git"),
        Some("scm:git:git@github.com:example/lib.git")
      ))
    )
    // coursier's SAX POM parser doesn't read developers (`developers = Nil // TODO` there),
    // they're checked at the XML level above
    assertEquals(
      project.dependencies0.map { case (v, d) => (d.module.toString, v.asConfiguration) },
      Seq(
        ("org.scala-lang:scala3-library_3", Some(Configuration.empty)),
        ("org.dep:dep_3", Some(Configuration.test))
      )
    )
  }

  test("minimal POM only has the mandatory elements and an organization") {
    val pom  = Pom.create(Organization("org.example"), ModuleName("lib_3"), "1.0.0")
    val root = XML.loadString(pom)
    assertEquals(
      root.child.collect { case e: Elem => e.label },
      Seq("modelVersion", "groupId", "artifactId", "version", "organization")
    )
  }

  // Scala CLI uses Pom.print to serialize its ivy.xml files
  test("Pom.print adds an XML declaration and indents with 2 spaces") {
    assertEquals(
      Pom.print(<a><b>c</b></a>),
      """<?xml version="1.0" encoding="UTF-8"?>
        |<a>
        |  <b>c</b>
        |</a>""".stripMargin
    )
  }
}
