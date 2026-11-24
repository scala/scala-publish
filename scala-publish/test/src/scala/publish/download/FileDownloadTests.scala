package scala.publish.download

import coursier.maven.MavenRepository

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path as NioPath}
import java.time.Instant
import java.util.concurrent.Executors

import scala.concurrent.ExecutionContext
import scala.publish.download.logger.DownloadLogger
import scala.publish.fileset.{FileSet, Path}
import scala.publish.{Content, MavenMetadata, PublishTasks, ScalaPublishTestSuite}
import scala.xml.XML

class FileDownloadTests extends ScalaPublishTestSuite {

  private val tmpDirs = FunFixture[NioPath](
    setup = _ => Files.createTempDirectory("scala-publish-file-download"),
    teardown = dir =>
      if Files.exists(dir) then
        Files.walk(dir)
          .sorted(java.util.Comparator.reverseOrder())
          .forEach(p => Files.deleteIfExists(p))
  )

  private val nop = new DownloadLogger {}

  private def write(base: NioPath, rel: String, content: String): NioPath = {
    val p = base.resolve(rel)
    Files.createDirectories(p.getParent)
    Files.write(p, content.getBytes(StandardCharsets.UTF_8))
  }

  private def asString(res: Option[(Option[Instant], Array[Byte])]): Option[String] =
    res.map((_, b) => new String(b, StandardCharsets.UTF_8))

  tmpDirs.test("downloads existing files from file: URLs, as built from repository roots") {
    base =>
      val f   = write(base, "org/example/lib_3/maven-metadata.xml", "<metadata/>")
      val res = FileDownload(base).downloadIfExists(f.toUri.toString, None, nop)
      assertEquals(asString(res), Some("<metadata/>"))
      assertEquals(res.flatMap(_._1), Some(Files.getLastModifiedTime(f).toInstant))
  }

  tmpDirs.test("missing files don't exist") { base =>
    val url = base.resolve("org/example/lib_3/maven-metadata.xml").toUri.toString
    assertEquals(FileDownload(base).downloadIfExists(url, None, nop), None)
  }

  tmpDirs.test("relative paths are resolved against the base directory") { base =>
    write(base, "org/example/lib_3/maven-metadata.xml", "<metadata/>")
    val res = FileDownload(base).downloadIfExists("org/example/lib_3/maven-metadata.xml", None, nop)
    assertEquals(asString(res), Some("<metadata/>"))
  }

  tmpDirs.test("relative paths with spaces are resolved against the base directory") { base =>
    write(base, "with space/maven-metadata.xml", "<metadata/>")
    val res = FileDownload(base).downloadIfExists("with space/maven-metadata.xml", None, nop)
    assertEquals(asString(res), Some("<metadata/>"))
  }

  tmpDirs.test("URLs outside of the base directory are rejected") { base =>
    val outside = base.resolveSibling(s"${base.getFileName}-outside/maven-metadata.xml")
    val e       = intercept[Exception] {
      FileDownload(base).downloadIfExists(outside.toUri.toString, None, nop)
    }
    assert(e.getMessage.startsWith("Invalid path: "), clue = e.getMessage)
  }

  tmpDirs.test("updateMavenMetadata merges with the maven-metadata.xml of a local repository") {
    base =>
      val existing = MavenMetadata.print(MavenMetadata.create(
        coursier.core.Organization("org.example"),
        coursier.core.ModuleName("lib_3"),
        Some("1.0.0"),
        Some("1.0.0"),
        Seq("1.0.0"),
        Instant.EPOCH
      ))
      write(base, "org/example/lib_3/maven-metadata.xml", existing)
      val dir = Path(Seq("org", "example", "lib_3", "1.1.0"))
      val fs  = FileSet(Seq(
        dir / "lib_3-1.1.0.pom" -> Content.InMemory(Instant.EPOCH, "<project/>".getBytes),
        dir / "lib_3-1.1.0.jar" -> Content.InMemory(Instant.EPOCH, Array[Byte](1))
      ))
      val pool = Executors.newSingleThreadExecutor()
      try {
        val res = PublishTasks.updateMavenMetadata(
          fs,
          Instant.parse("2024-01-02T03:04:05Z"),
          FileDownload(base),
          MavenRepository(base.toUri.toString.stripSuffix("/")),
          nop,
          withMavenSnapshotVersioning = false,
          pool
        ).unsafeRun()(using ExecutionContext.fromExecutorService(pool))
        val md = res.elements
          .collectFirst { case (p, c) if p.repr == "org/example/lib_3/maven-metadata.xml" => c }
          .getOrElse(fail("No maven-metadata.xml"))
        val info =
          MavenMetadata.info(XML.loadString(new String(md.content(), StandardCharsets.UTF_8)))
        assertEquals(info.versions, Seq("1.0.0", "1.1.0"))
      }
      finally pool.shutdown()
  }
}
