package scala.publish

import coursier.core.{Authentication, ModuleName, Organization}
import coursier.maven.MavenRepository

import java.nio.charset.StandardCharsets
import java.time.{Instant, LocalDateTime}
import java.util.concurrent.Executors

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService}
import scala.publish.download.Download
import scala.publish.download.logger.DownloadLogger
import scala.publish.fileset.{FileSet, Path}
import scala.xml.XML

class MavenMetadataTests extends ScalaPublishTestSuite {

  private var ec: ExecutionContextExecutorService = scala.compiletime.uninitialized

  override def beforeAll(): Unit =
    ec = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(2))
  override def afterAll(): Unit =
    ec.shutdownNow()

  private val now   = Instant.parse("2024-01-02T03:04:05Z")
  private val org   = Organization("org.example")
  private val name  = ModuleName("lib_3")
  private val repo  = MavenRepository("https://repo.example.test/maven")
  private val mdUrl = "https://repo.example.test/maven/org/example/lib_3/maven-metadata.xml"

  private def metadata(versions: String*): String =
    MavenMetadata.print(MavenMetadata.create(
      org,
      name,
      versions.lastOption,
      versions.filter(MavenMetadata.isReleaseVersion).lastOption,
      versions,
      Instant.parse("2023-06-07T08:09:10Z")
    ))

  test("create and info round-trip") {
    val info = MavenMetadata.info(XML.loadString(metadata("0.9.0", "1.0.0-RC1", "1.0.0")))
    assertEquals(
      info,
      MavenMetadata.Info(
        Some("1.0.0"),
        Some("1.0.0"),
        Seq("0.9.0", "1.0.0-RC1", "1.0.0"),
        Some(LocalDateTime.parse("2023-06-07T08:09:10"))
      )
    )
  }

  test("update adds new versions only once, and updates latest, release and lastUpdated") {
    val updated = MavenMetadata.update(
      XML.loadString(metadata("0.9.0", "1.0.0")),
      None,
      None,
      Some("1.1.0"),
      Some("1.1.0"),
      Seq("1.0.0", "1.1.0"),
      Some(LocalDateTime.parse("2024-01-02T03:04:05"))
    )
    assertEquals(
      MavenMetadata.info(updated),
      MavenMetadata.Info(
        Some("1.1.0"),
        Some("1.1.0"),
        Seq("0.9.0", "1.0.0", "1.1.0"),
        Some(LocalDateTime.parse("2024-01-02T03:04:05"))
      )
    )
  }

  for (
    (version, isRelease) <- Seq(
      "1.0.0"          -> true,
      "1.0"            -> true,
      "1.0.0-1"        -> true,
      "1.0.0.Final"    -> true,
      "1.0.0-RC1"      -> false,
      "1.0.0-M2"       -> false,
      "1.0.0-SNAPSHOT" -> false,
      "1.0.0+3-abcdef" -> false
    )
  )
    test(s"isReleaseVersion($version) == $isRelease") {
      assertEquals(MavenMetadata.isReleaseVersion(version), isRelease)
    }

  test("snapshot versioning metadata: build number and timestamp are read back") {
    val elem = MavenMetadata.createSnapshotVersioning(
      org,
      name,
      "1.0.0-SNAPSHOT",
      (LocalDateTime.parse("2024-01-02T03:04:05"), 7),
      now,
      Seq((None, "jar", "1.0.0-20240102.030405-7", LocalDateTime.parse("2024-01-02T03:04:05")))
    )
    assertEquals(MavenMetadata.snapshotVersioningBuildNumber(elem), Some(7))
    assertEquals(
      MavenMetadata.currentSnapshotVersioning(elem),
      Some((7, LocalDateTime.parse("2024-01-02T03:04:05")))
    )
  }

  /** Serves `files` (URL -> content), recording requested URLs */
  private final class MapDownload(files: Map[String, String]) extends Download {
    val requested = mutable.ArrayBuffer.empty[(String, Option[Authentication])]
    def downloadIfExists(
      url: String,
      authentication: Option[Authentication],
      logger: DownloadLogger
    ): Option[(Option[Instant], Array[Byte])] = {
      requested.synchronized(requested += url -> authentication)
      files.get(url).map(c => (None, c.getBytes(StandardCharsets.UTF_8)))
    }
  }

  private def moduleFiles(version: String): FileSet = {
    val dir = Path(Seq("org", "example", "lib_3", version))
    FileSet(Seq(
      dir / s"lib_3-$version.pom" -> Content.InMemory(now, "<project/>".getBytes),
      dir / s"lib_3-$version.jar" -> Content.InMemory(now, Array[Byte](1))
    ))
  }

  private def metadataInfo(fs: FileSet): MavenMetadata.Info = {
    val path = Path(Seq("org", "example", "lib_3", "maven-metadata.xml"))
    val c    = fs.elements.collectFirst { case (`path`, c) => c }
      .getOrElse(fail(s"No maven-metadata.xml in ${fs.elements.map(_._1.repr)}"))
    MavenMetadata.info(XML.loadString(new String(c.content(), StandardCharsets.UTF_8)))
  }

  private def update(fs: FileSet, download: Download): FileSet =
    PublishTasks.updateMavenMetadata(
      fs,
      now,
      download,
      repo,
      new DownloadLogger {},
      withMavenSnapshotVersioning = false,
      ec
    ).unsafeRun()(using ec)

  test("updateMavenMetadata creates maven-metadata.xml for a first release") {
    val download = new MapDownload(Map.empty)
    val res      = update(moduleFiles("1.0.0"), download)
    assertEquals(
      metadataInfo(res),
      MavenMetadata.Info(
        Some("1.0.0"),
        Some("1.0.0"),
        Seq("1.0.0"),
        Some(LocalDateTime.parse("2024-01-02T03:04:05"))
      )
    )
    assertEquals(download.requested.map(_._1).toList, List(mdUrl))
  }

  test("updateMavenMetadata merges the new version into the remote maven-metadata.xml") {
    val download = new MapDownload(Map(mdUrl -> metadata("0.9.0", "1.0.0")))
    val info     = metadataInfo(update(moduleFiles("1.1.0"), download))
    assertEquals(info.versions, Seq("0.9.0", "1.0.0", "1.1.0"))
    assertEquals(info.latest, Some("1.1.0"))
    assertEquals(info.release, Some("1.1.0"))
  }

  test("updateMavenMetadata keeps the module files") {
    val res = update(moduleFiles("1.1.0"), new MapDownload(Map.empty))
    assertEquals(
      res.elements.map(_._1.repr).toSet,
      Set(
        "org/example/lib_3/1.1.0/lib_3-1.1.0.pom",
        "org/example/lib_3/1.1.0/lib_3-1.1.0.jar",
        "org/example/lib_3/maven-metadata.xml"
      )
    )
  }

  // Known bug, remove `.fail` once fixed.
  test("updateMavenMetadata doesn't move latest / release back to an older version".fail) {
    val download = new MapDownload(Map(mdUrl -> metadata("1.0.0", "2.0.0")))
    val info     = metadataInfo(update(moduleFiles("1.0.1"), download))
    assertEquals(info.versions, Seq("1.0.0", "2.0.0", "1.0.1"))
    assertEquals(info.latest, Some("2.0.0"))
    assertEquals(info.release, Some("2.0.0"))
  }
}
