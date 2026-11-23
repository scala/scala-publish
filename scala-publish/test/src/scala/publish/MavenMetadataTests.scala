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
import scala.xml.{Elem, XML}

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

  test("updateMavenMetadata doesn't move latest / release back to an older version") {
    val download = new MapDownload(Map(mdUrl -> metadata("1.0.0", "2.0.0")))
    val info     = metadataInfo(update(moduleFiles("1.0.1"), download))
    assertEquals(info.versions, Seq("1.0.0", "2.0.0", "1.0.1"))
    assertEquals(info.latest, Some("2.0.0"))
    assertEquals(info.release, Some("2.0.0"))
  }

  test("updateMavenMetadata moves latest to a newer pre-release, but keeps release") {
    val download = new MapDownload(Map(mdUrl -> metadata("0.9.0", "1.0.0")))
    val info     = metadataInfo(update(moduleFiles("1.1.0-RC1"), download))
    assertEquals(info.versions, Seq("0.9.0", "1.0.0", "1.1.0-RC1"))
    assertEquals(info.latest, Some("1.1.0-RC1"))
    assertEquals(info.release, Some("1.0.0"))
  }

  test("highestVersion compares versions, not strings, and ignores empty ones") {
    assertEquals(MavenMetadata.highestVersion(Some("1.9.0"), Some("1.10.0")), Some("1.10.0"))
    assertEquals(MavenMetadata.highestVersion(Some("2.0.0"), Some("2.0.0-RC1")), Some("2.0.0"))
    assertEquals(MavenMetadata.highestVersion(Some(""), Some("1.0.0")), Some("1.0.0"))
    assertEquals(MavenMetadata.highestVersion(None, None), None)
  }

  private def versioningLabels(elem: Elem): Seq[String] =
    (elem \ "versioning").headOption.toSeq.flatMap(_.child.collect { case e: Elem => e.label })

  private def updateAll(elem: Elem): Elem =
    MavenMetadata.update(
      elem,
      None,
      None,
      Some("1.1.0"),
      Some("1.1.0"),
      Seq("1.1.0"),
      Some(LocalDateTime.parse("2024-01-02T03:04:05"))
    )

  private val updatedInfo = MavenMetadata.Info(
    Some("1.1.0"),
    Some("1.1.0"),
    Seq("1.1.0"),
    Some(LocalDateTime.parse("2024-01-02T03:04:05"))
  )

  test("update adds missing latest, release, versions and lastUpdated, in Maven's order") {
    val elem = XML.loadString(
      """<metadata><groupId>org.example</groupId><artifactId>lib_3</artifactId><versioning/></metadata>"""
    )
    val updated = updateAll(elem)
    assertEquals(MavenMetadata.info(updated), updatedInfo)
    assertEquals(versioningLabels(updated), Seq("latest", "release", "versions", "lastUpdated"))
  }

  test("update adds a missing versioning element") {
    val elem = XML.loadString(
      """<metadata><groupId>org.example</groupId><artifactId>lib_3</artifactId></metadata>"""
    )
    val updated = updateAll(elem)
    assertEquals(MavenMetadata.info(updated), updatedInfo)
    assertEquals(
      updated.child.collect { case e: Elem => e.label },
      Seq("groupId", "artifactId", "versioning")
    )
  }

  test("update inserts missing elements around existing ones, keeping Maven's order") {
    val elem = XML.loadString(
      """<metadata><versioning><release>1.0.0</release><lastUpdated>20230607080910</lastUpdated></versioning></metadata>"""
    )
    val updated = updateAll(elem)
    assertEquals(MavenMetadata.info(updated), updatedInfo)
    assertEquals(versioningLabels(updated), Seq("latest", "release", "versions", "lastUpdated"))
  }

  test("update doesn't add elements it has no value for") {
    val elem    = XML.loadString("""<metadata><versioning/></metadata>""")
    val updated = MavenMetadata.update(elem, None, None, None, None, Nil, None)
    assertEquals(versioningLabels(updated), Nil)
  }

  test("update doesn't add an empty versioning element") {
    val elem    = XML.loadString("""<metadata><groupId>org.example</groupId></metadata>""")
    val updated = MavenMetadata.update(elem, None, None, None, None, Nil, None)
    assertEquals(updated.child.collect { case e: Elem => e.label }, Seq("groupId"))
  }

  test("updateMavenMetadata fills in a remote maven-metadata.xml lacking versions") {
    val remote =
      """<metadata><groupId>org.example</groupId><artifactId>lib_3</artifactId><versioning><latest>1.0.0</latest></versioning></metadata>"""
    val info = metadataInfo(update(moduleFiles("1.1.0"), new MapDownload(Map(mdUrl -> remote))))
    assertEquals(info.versions, Seq("1.1.0"))
    assertEquals(info.latest, Some("1.1.0"))
    assertEquals(info.release, Some("1.1.0"))
  }

  private val snapshotTime = LocalDateTime.parse("2024-01-02T03:04:05")
  private val snapshotJar  = (None, "jar", "1.0.0-20240102.030405-3", snapshotTime)

  private def updateSnapshot(elem: Elem): Elem =
    MavenMetadata.updateSnapshotVersioning(
      elem,
      None,
      None,
      Some("1.0.0-SNAPSHOT"),
      Some((snapshotTime, 3)),
      Some(snapshotTime),
      Seq(snapshotJar)
    )

  private def snapshotVersionValues(elem: Elem): Seq[String] =
    (elem \ "versioning" \ "snapshotVersions" \ "snapshotVersion" \ "value").map(_.text)

  test("updateSnapshotVersioning adds missing snapshot, lastUpdated and snapshotVersions") {
    val elem = XML.loadString(
      """<metadata><groupId>org.example</groupId><artifactId>lib_3</artifactId><version>1.0.0-SNAPSHOT</version><versioning/></metadata>"""
    )
    val updated = updateSnapshot(elem)
    assertEquals(MavenMetadata.currentSnapshotVersioning(updated), Some((3, snapshotTime)))
    assertEquals(MavenMetadata.info(updated).lastUpdated, Some(snapshotTime))
    assertEquals(snapshotVersionValues(updated), Seq("1.0.0-20240102.030405-3"))
    // same order as Maven: snapshot, lastUpdated, snapshotVersions
    assertEquals(versioningLabels(updated), Seq("snapshot", "lastUpdated", "snapshotVersions"))
  }

  test("updateSnapshotVersioning puts a missing snapshot before an existing lastUpdated") {
    val elem = XML.loadString(
      """<metadata><version>1.0.0-SNAPSHOT</version><versioning><lastUpdated>20230607080910</lastUpdated><snapshotVersions/></versioning></metadata>"""
    )
    val updated = updateSnapshot(elem)
    assertEquals(versioningLabels(updated), Seq("snapshot", "lastUpdated", "snapshotVersions"))
    assertEquals(MavenMetadata.currentSnapshotVersioning(updated), Some((3, snapshotTime)))
  }

  test("updateSnapshotVersioning adds missing version and versioning elements") {
    val elem = XML.loadString(
      """<metadata><groupId>org.example</groupId><artifactId>lib_3</artifactId></metadata>"""
    )
    val updated = updateSnapshot(elem)
    assertEquals(
      updated.child.collect { case e: Elem => e.label },
      Seq("groupId", "artifactId", "version", "versioning")
    )
    assertEquals((updated \ "version").text, "1.0.0-SNAPSHOT")
    assertEquals(versioningLabels(updated), Seq("snapshot", "lastUpdated", "snapshotVersions"))
  }

  test("updateSnapshotVersioning doesn't add elements it has no value for") {
    val elem    = XML.loadString("""<metadata><groupId>org.example</groupId></metadata>""")
    val updated = MavenMetadata.updateSnapshotVersioning(elem, None, None, None, None, None, Nil)
    assertEquals(updated.child.collect { case e: Elem => e.label }, Seq("groupId"))
  }

  test("updateMavenMetadata fails, rather than overwriting, when the repository refuses access") {
    val backend = sttp.client3.testing.SttpBackendStub.synchronous.whenAnyRequest
      .thenRespondWithCode(sttp.model.StatusCode.Unauthorized)
    intercept[download.Download.Error.Unauthorized] {
      update(moduleFiles("1.1.0"), download.HttpDownload(backend))
    }
  }

  for (
    snapshot <- Seq(
      "<buildNumber>3</buildNumber>",
      "<timestamp>20240102.030405</timestamp>"
    )
  )
    test(s"currentSnapshotVersioning reports incomplete snapshot info ($snapshot)") {
      val elem = XML.loadString(
        s"<metadata><versioning><snapshot>$snapshot</snapshot></versioning></metadata>"
      )
      val e = intercept[InvalidMetadataException](MavenMetadata.currentSnapshotVersioning(elem))
      assert(
        e.getMessage.startsWith("Incomplete <snapshot> in maven-metadata.xml"),
        clue = e.getMessage
      )
    }
}
