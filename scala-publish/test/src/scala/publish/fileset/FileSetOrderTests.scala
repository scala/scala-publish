package scala.publish.fileset

import coursier.core.{MinimizedExclusions, ModuleName, Organization}

import java.nio.charset.StandardCharsets
import java.time.Instant
import java.util.concurrent.Executors

import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService}
import scala.publish.{Content, Pom, ScalaPublishTestSuite}

/** [[FileSet.order]] / [[Group.split]] on file sets laid out like Scala CLI's (see `Publish.scala`
  * there)
  */
class FileSetOrderTests extends ScalaPublishTestSuite {

  private var ec: ExecutionContextExecutorService = scala.compiletime.uninitialized

  override def beforeAll(): Unit =
    ec = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(2))
  override def afterAll(): Unit =
    ec.shutdownNow()

  private val now = Instant.EPOCH
  private val org = "org.example"

  private def pom(name: String, dependsOn: Seq[String]): Array[Byte] =
    Pom.create(
      organization = Organization(org),
      moduleName = ModuleName(name),
      version = "1.0.0",
      dependencies = dependsOn.map { dep =>
        (Organization(org), ModuleName(dep), "1.0.0", None, MinimizedExclusions.zero)
      }
    ).getBytes(StandardCharsets.UTF_8)

  /** POM, JAR, sources and javadoc JARs, each with a signature and checksums */
  private def module(name: String, dependsOn: Seq[String] = Nil): FileSet = {
    val dir   = Path(org.split('.').toSeq ++ Seq(name, "1.0.0"))
    val base  = s"$name-1.0.0"
    val files = Seq(s"$base.pom", s"$base.jar", s"$base-sources.jar", s"$base-javadoc.jar")
    FileSet(
      for {
        f      <- files
        suffix <- Seq("", ".asc", ".md5", ".sha1")
      } yield {
        val content =
          if f.endsWith(".pom") && suffix.isEmpty then pom(name, dependsOn) else Array[Byte](1)
        (dir / (f + suffix)) -> Content.InMemory(now, content)
      }
    )
  }

  private def order(fs: FileSet): Seq[Path] =
    fs.order(ec).unsafeRun()(using ec).elements.map(_._1)

  test("ordering keeps all files") {
    val fs = module("core_3") ++ module("app_3", Seq("core_3"))
    assertEquals(order(fs).toSet, fs.elements.map(_._1).toSet)
  }

  test("modules come after the modules they depend on") {
    // app_3 sorts before core_3 alphabetically, but depends on it
    val fs      = module("app_3", Seq("core_3")) ++ module("core_3")
    val modules = order(fs).map(_.elements.dropRight(2).last).distinct
    assertEquals(modules, Seq("core_3", "app_3"))
  }

  test("within a module, the POM comes last, and signatures and checksums before their file") {
    val ordered = order(module("core_3")).map(_.elements.last)
    assertEquals(ordered.last, "core_3-1.0.0.pom")
    for ((name, idx) <- ordered.zipWithIndex)
      for (base <- SideFiles.bases(name).drop(1))
        assert(
          idx < ordered.indexOf(base),
          clue = s"$name should come before $base in $ordered"
        )
  }

  test("a dependency cycle between modules is reported") {
    val fs = module("a_3", Seq("b_3")) ++ module("b_3", Seq("a_3"))
    val e  = intercept[Exception](order(fs))
    assert(e.getMessage.startsWith("Found cycle in input modules"), clue = e.getMessage)
  }

  test("unrecognized files in a module directory are reported") {
    val dir = Path(Seq("org", "example", "core_3", "1.0.0"))
    val fs = module("core_3") ++ FileSet(Seq((dir / "README.md") -> Content.InMemory(now, Array())))
    val e  = intercept[Exception](Group.split(fs))
    assertEquals(e.getMessage, "Unrecognized files: org/example/core_3/1.0.0/README.md")
  }
}
