package scala.publish.upload

import coursier.maven.MavenRepository

import java.nio.file.{Files, Path as NioPath}
import java.time.Instant
import java.util.concurrent.Executors

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService}
import scala.publish.fileset.{FileSet, Path}
import scala.publish.upload.logger.UploadLogger
import scala.publish.{Content, ScalaPublishTestSuite}

/** [[FileUpload]] is what Scala CLI uses for every non-HTTP repository (`-R some/dir`, `m2-local`,
  * `ivy2-local`, …).
  */
class FileUploadTests extends ScalaPublishTestSuite {

  private var ec: ExecutionContextExecutorService = scala.compiletime.uninitialized

  override def beforeAll(): Unit =
    ec = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(2))
  override def afterAll(): Unit =
    ec.shutdownNow()

  private val tmpDirs = FunFixture[NioPath](
    setup = _ => Files.createTempDirectory("scala-publish-file-upload"),
    teardown = dir =>
      if Files.exists(dir) then
        Files.walk(dir)
          .sorted(java.util.Comparator.reverseOrder())
          .forEach(p => Files.deleteIfExists(p))
  )

  private final class RecordingLogger extends UploadLogger {
    val uploaded = mutable.ArrayBuffer.empty[(String, Option[Upload.Error])]
    override def uploaded(
      url: String,
      idOpt: Option[Object],
      errorOpt: Option[Upload.Error]
    ): Unit =
      uploaded.synchronized(uploaded += url -> errorOpt)
  }

  /** Creates `base/blocker` as a regular file, so that writing anything under it fails. */
  private def withBlocker(base: NioPath): NioPath = {
    val blocker = base.resolve("blocker")
    Files.write(blocker, Array[Byte](0))
    blocker
  }

  tmpDirs.test("writes the content at the URL path, creating parent directories") { base =>
    val target = base.resolve("org/example/lib_3/1.0.0/lib_3-1.0.0.jar")
    val logger = new RecordingLogger
    val res    =
      FileUpload(base).upload(target.toUri.toString, None, Array[Byte](1, 2, 3), logger)
    assertEquals(res, None)
    assertEquals(Files.readAllBytes(target).toSeq, Seq[Byte](1, 2, 3))
    assertEquals(logger.uploaded.toSeq, Seq(target.toString -> None))
  }

  tmpDirs.test("returns an error when the file can't be written") { base =>
    val target = withBlocker(base).resolve("x/lib.jar")
    val logger = new RecordingLogger
    val res    = FileUpload(base).upload(target.toUri.toString, None, Array[Byte](1), logger)
    res match {
      case Some(_: Upload.Error.FileException) =>
      case other                               => fail(s"Expected a FileException, got $other")
    }
    assert(!Files.exists(target))
    assertEquals(logger.uploaded.size, 1)
    assert(logger.uploaded.head._2.nonEmpty, clue = logger.uploaded)
  }

  tmpDirs.test("rejects URLs outside of the base directory") { base =>
    val outside = base.resolveSibling(s"${base.getFileName}-outside/lib.jar")
    val e       = intercept[Exception] {
      FileUpload(base).upload(outside.toUri.toString, None, Array[Byte](1), new RecordingLogger)
    }
    assert(e.getMessage.startsWith("Invalid path: "), clue = e.getMessage)
    assert(!Files.exists(outside))
  }

  for (parallel <- Seq(false, true))
    tmpDirs.test(s"uploadFileSet reports write failures (parallel = $parallel)") { base =>
      withBlocker(base)
      val now     = Instant.EPOCH
      val ok      = Path(Seq("org", "example", "ok.jar"))
      val failing = Path(Seq("blocker", "x", "failing.jar"))
      val fs      = FileSet(Seq(
        ok      -> Content.InMemory(now, Array[Byte](1)),
        failing -> Content.InMemory(now, Array[Byte](2))
      ))
      val repo   = MavenRepository(base.toUri.toString.stripSuffix("/"))
      val errors = FileUpload(base)
        .uploadFileSet(repo, fs, new RecordingLogger, if parallel then Some(ec) else None)
        .unsafeRun()(using ec)
      assertEquals(errors.map(_._1), Seq(failing))
      assert(errors.forall(_._3.isInstanceOf[Upload.Error.FileException]), clue = errors)
      assert(Files.exists(base.resolve("org/example/ok.jar")))
    }
}
