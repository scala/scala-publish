package scala.publish

import coursier.core.{Authentication, Configuration, MinimizedExclusions, ModuleName, Organization}
import coursier.maven.MavenRepository
import sttp.client3.testing.SttpBackendStub

import java.io.{PrintStream, StringWriter}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path as NioPath, Paths}
import java.time.Instant
import java.util.concurrent.Executors

import scala.annotation.nowarn
import scala.jdk.CollectionConverters.*
import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService}
import scala.publish.checksum.logger.{ChecksumLogger, InteractiveChecksumLogger}
import scala.publish.checksum.{ChecksumType, Checksums}
import scala.publish.fileset.{FileSet, Path}
import scala.publish.signing.logger.InteractiveSignerLogger
import scala.publish.signing.{GpgSigner, NopSigner, Signer}
import scala.publish.sonatype.SonatypeApi
import scala.publish.upload.logger.InteractiveUploadLogger
import scala.publish.upload.{DummyUpload, FileUpload, HttpURLConnectionUpload, Upload}
import scala.publish.util.EmaRetryParams

/** Calls the scala-publish API. */
class ApiTests extends ScalaPublishTestSuite {

  private var ec: ExecutionContextExecutorService = scala.compiletime.uninitialized

  override def beforeAll(): Unit =
    ec = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(2))
  override def afterAll(): Unit =
    ec.shutdownNow()

  private val tmpDirs = FunFixture[NioPath](
    setup = _ => Files.createTempDirectory("scala-publish-scala-cli"),
    teardown = dir =>
      if Files.exists(dir) then
        Files.walk(dir)
          .sorted(java.util.Comparator.reverseOrder())
          .forEach(p => Files.deleteIfExists(p))
  )

  private val now = Instant.EPOCH

  /** Like `Publish.buildFileSet` in Scala CLI */
  private def scalaCliFileSet(jar: NioPath): FileSet = {
    val org     = "org.example"
    val name    = "lib_3"
    val ver     = "1.0.0"
    val pomText = Pom.create(
      organization = Organization(org),
      moduleName = ModuleName(name),
      version = ver,
      packaging = None,
      url = Some("https://github.com/example/lib"),
      name = Some("lib"),
      dependencies = Seq((
        Organization("org.scala-lang"),
        ModuleName("scala3-library_3"),
        "3.3.6",
        Option.empty[Configuration],
        MinimizedExclusions.zero
      )),
      description = Some("A library"),
      license = Some(Pom.License("Apache-2.0", "https://spdx.org/licenses/Apache-2.0.html")),
      scm = Some(Pom.Scm(
        "https://github.com/example/lib.git",
        "scm:git:github.com/example/lib.git",
        "scm:git:git@github.com:example/lib.git"
      )),
      developers = Seq(Pom.Developer("jdoe", "Jane Doe", "https://github.com/jdoe", None))
    )
    val basePath = Path(org.split('.').toSeq ++ Seq(name, ver))
    FileSet(Seq(
      basePath / s"$name-$ver.pom" ->
        Content.InMemory(now, pomText.getBytes(StandardCharsets.UTF_8)),
      basePath / s"$name-$ver.jar" -> Content.File(jar)
    ))
  }

  tmpDirs.test("local publish flow: sign, checksum, order and upload with FileUpload") { tmp =>
    val jar = tmp.resolve("input.jar")
    Files.write(jar, Array[Byte](1, 2, 3))
    val repoDir = tmp.resolve("repo")
    val repo    = PublishRepository.Simple(MavenRepository(repoDir.toUri.toString.stripSuffix("/")))

    val fileSet0 = scalaCliFileSet(jar)

    // signing (Publish.scala)
    val signer: Signer = NopSigner
    val signerLogger   = new InteractiveSignerLogger(out = new StringWriter, verbosity = 1)
    val signRes: Either[(Path, Content, String), FileSet] = signer.signatures(
      fileSet = fileSet0,
      now = now,
      dontSignExtensions = ChecksumType.all.map(_.extension).toSet,
      dontSignFiles = Set("maven-metadata.xml"),
      logger = signerLogger
    )
    val fileSet1 = fileSet0 ++ signRes.fold(e => fail(s"Signing failed: $e"), identity)

    // checksums (PublishUtils.computeChecksums)
    val checksumLogger: ChecksumLogger =
      new InteractiveChecksumLogger(new StringWriter, verbosity = 1)
    val checksums = Checksums(
      Seq(ChecksumType.MD5, ChecksumType.SHA1),
      FileSet(fileSet1.elements.filterNot((path, _) =>
        path.elements.lastOption.exists(_.endsWith(".asc"))
      )),
      now,
      ec,
      checksumLogger
    ).unsafeRun()(using ec)
    val fileSet2 = fileSet1 ++ checksums

    val finalFileSet = fileSet2.order(ec).unsafeRun()(using ec)

    // hooks and upload
    val hooks        = Hooks.dummy
    val hooksData    = hooks.beforeUpload(finalFileSet, false).unsafeRun()(using ec)
    val retainedRepo =
      hooks.repository(hooksData, repo, false).getOrElse(repo.repo(false))
    val baseUpload = FileUpload(Paths.get(new java.net.URI(retainedRepo.root)))
    val upload     = DummyUpload(baseUpload)
    assert(upload.isInstanceOf[Upload])
    val uploadLogger =
      InteractiveUploadLogger.create(
        new PrintStream(new java.io.ByteArrayOutputStream),
        dummy = false,
        isLocal = true
      )
    val errors = baseUpload.uploadFileSet(
      repository = retainedRepo,
      fileSet = finalFileSet,
      logger = uploadLogger,
      parallel = None
    ).unsafeRun()(using ec)
    hooks.afterUpload(hooksData).unsafeRun()(using ec)

    assertEquals(errors, Seq.empty)
    val published = {
      val dir = repoDir.resolve("org/example/lib_3/1.0.0")
      Files.list(dir).iterator().asScala.map(_.getFileName.toString).toSeq.sorted
    }
    assertEquals(
      published,
      Seq(
        "lib_3-1.0.0.jar",
        "lib_3-1.0.0.jar.md5",
        "lib_3-1.0.0.jar.sha1",
        "lib_3-1.0.0.pom",
        "lib_3-1.0.0.pom.md5",
        "lib_3-1.0.0.pom.sha1"
      )
    )
    assertEquals(
      Files.readAllBytes(repoDir.resolve("org/example/lib_3/1.0.0/lib_3-1.0.0.jar")).toSeq,
      Seq[Byte](1, 2, 3)
    )
  }

  // the overload Scala CLI calls, deprecated since 1.1.0
  @nowarn("cat=deprecation")
  private def scalaCliHttpUpload(): Upload =
    HttpURLConnectionUpload.create(Some(1000), Some(2000))

  // only checks that this compiles, and that values can be built (no network involved)
  test("repository, Sonatype hooks and HTTP upload setup (RepoParams.scala, Publish.scala)") {
    val auth = Authentication("user", "secret")

    val sonatypeRepo = PublishRepository.Sonatype(
      base = MavenRepository("https://ossrh-staging-api.central.sonatype.com"),
      useLegacySnapshots = false
    )
    val api = SonatypeApi(
      backend = SttpBackendStub.synchronous,
      base = sonatypeRepo.base.root + "/service/local",
      authentication = None,
      verbosity = 0,
      retryOnTimeout = 3,
      stagingRepoRetryParams = EmaRetryParams(
        attempts = 3,
        initialWaitDurationMs = 10 * 1000L,
        factor = 2.0f
      )
    )
    val es    = Executors.newSingleThreadScheduledExecutor()
    val hooks = Hooks.sonatype(
      repo = sonatypeRepo,
      api = api,
      out = new PrintStream(new java.io.ByteArrayOutputStream),
      verbosity = 0,
      batch = true,
      es = es
    )
    try {
      // RepoParams.withAuth
      val repo0: PublishRepository = sonatypeRepo.withAuthentication(auth)
      val hooks0                   = hooks match {
        case s: Hooks.Sonatype =>
          s.copy(
            repo = s.repo.withAuthentication(auth),
            api = s.api.copy(authentication = Some(auth))
          )
        case other => other
      }
      assert(hooks0.isInstanceOf[Hooks.Sonatype])
      assert(repo0.snapshotRepo.root.nonEmpty)
      assert(!repo0.releaseRepo.root.contains("s01"))
      assert(repo0.checkResultsRepo(false).root.nonEmpty)
      assert(repo0.repo(true).root.nonEmpty)

      // HTTP uploads, with the overload Scala CLI calls
      val upload = scalaCliHttpUpload()
      assert(upload.isInstanceOf[HttpURLConnectionUpload])

      // GpgSigner (PublishUtils.scala)
      val gpg: Signer = GpgSigner(key = GpgSigner.Key.Id("ABCDEF"), extraOptions = Seq("--batch"))
      assert(gpg.isInstanceOf[GpgSigner])
    }
    finally es.shutdownNow()
  }

  // UploadError.scala and the error handling in Publish.scala
  test("upload errors, as rendered and matched by Scala CLI") {
    val errors: ::[(Path, Content, Upload.Error)] = ::(
      (
        Path(Seq("org", "example", "a.jar")),
        Content.InMemory(now, Array()),
        new Upload.Error.HttpError(403, Map.empty, "Forbidden")
      ),
      List((
        Path(Seq("org", "example", "b.jar")),
        Content.InMemory(now, Array()),
        new Upload.Error.Unauthorized("https://repo.example.test/b.jar", Some("realm"))
      ))
    )
    val rendered = errors.map { case (path, _, err) => s"${path.repr}: ${err.getMessage}" }
    assertEquals(
      rendered,
      List(
        "org/example/a.jar: HTTP 403\nForbidden",
        "org/example/b.jar: Unauthorized (https://repo.example.test/b.jar, realm)"
      )
    )
    val httpCodeRegex = "HTTP (\\d+)\n.*".r
    errors.head._3 match {
      case e: Upload.Error.HttpError =>
        e.getMessage match {
          case httpCodeRegex("403") =>
          case other                => fail(s"Unexpected message: $other")
        }
      case other => fail(s"Unexpected error: $other")
    }
    assert(errors.tail.forall(_._3.isInstanceOf[Upload.Error.Unauthorized]))
  }

  // Ivy.scala
  test("Pom data types read by Scala CLI's Ivy generation") {
    val license = Pom.License("Apache-2.0", "https://spdx.org/licenses/Apache-2.0.html")
    val scm     = Pom.Scm("url", "connection", "developerConnection")
    val dev     = Pom.Developer("id", "name", "url", Some("mail"))
    assertEquals(
      (license.name, license.url),
      ("Apache-2.0", "https://spdx.org/licenses/Apache-2.0.html")
    )
    assertEquals(
      (scm.url, scm.connection, scm.developerConnection),
      ("url", "connection", "developerConnection")
    )
    assertEquals((dev.id, dev.name, dev.url, dev.mail), ("id", "name", "url", Some("mail")))
    assert(Pom.print(<ivy-module/>).startsWith("<?xml"))
  }

  // `publish --checksum list`
  test("checksum listing and parsing") {
    assertEquals(ChecksumType.all.map(_.name), Seq("sha-1", "md5"))
    assertEquals(ChecksumType.parse("md5").map(_.extension), Right("md5"))
    assert(Content.File(Paths.get("x")).pathOpt.nonEmpty)
    assert(Content.InMemory(now, Array()).pathOpt.isEmpty)
  }
}
