package scala.publish.upload

import coursier.core.Authentication
import coursier.maven.MavenRepository

import java.time.Instant
import java.util.concurrent.Executors

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService}
import scala.publish.fileset.{FileSet, Path}
import scala.publish.upload.logger.UploadLogger
import scala.publish.{Content, ScalaPublishTestSuite}

/** [[Upload.uploadFileSet]], as called by Scala CLI for every publish */
class UploadTests extends ScalaPublishTestSuite {

  private var ec: ExecutionContextExecutorService = scala.compiletime.uninitialized

  override def beforeAll(): Unit =
    ec = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(4))
  override def afterAll(): Unit =
    ec.shutdownNow()

  private val nop  = new UploadLogger {}
  private val now  = Instant.EPOCH
  private val repo =
    MavenRepository("https://repo.example.test/maven", Some(Authentication("user", "secret")))

  private def fileSet(names: String*): FileSet =
    FileSet(names.map(n => Path(Seq("org", "example", n)) -> Content.InMemory(now, n.getBytes)))

  /** Records upload calls, failing those whose URL ends with one of `failing` */
  private final class RecordingUpload(failing: Set[String] = Set.empty) extends Upload {
    val calls = mutable.ArrayBuffer.empty[(String, Option[Authentication], String)]
    def upload(
      url: String,
      authentication: Option[Authentication],
      content: Array[Byte],
      logger: UploadLogger,
      loggingIdOpt: Option[Object]
    ): Option[Upload.Error] = {
      calls.synchronized(calls += ((url, authentication, new String(content))))
      if failing.exists(url.endsWith) then
        Some(new Upload.Error.HttpError(500, Map.empty, s"failed $url"))
      else None
    }
  }

  private final class RecordingLogger extends UploadLogger {
    val events                             = mutable.ArrayBuffer.empty[String]
    override def start(): Unit             = events.synchronized(events += "start")
    override def stop(keep: Boolean): Unit = events.synchronized(events += "stop")
    override def uploadingSet(id: Object, fileSet: FileSet): Unit =
      events.synchronized(events += s"uploadingSet(${fileSet.elements.length})")
    override def uploadedSet(id: Object, fileSet: FileSet): Unit =
      events.synchronized(events += s"uploadedSet(${fileSet.elements.length})")
  }

  test("uploads every file under the repository root, with its authentication") {
    val upload = new RecordingUpload
    val errors = upload
      .uploadFileSet(repo, fileSet("a.jar", "b.pom"), nop, None)
      .unsafeRun()(using ec)
    assertEquals(errors, Seq.empty)
    assertEquals(
      upload.calls.toList,
      List(
        ("https://repo.example.test/maven/org/example/a.jar", repo.authentication, "a.jar"),
        ("https://repo.example.test/maven/org/example/b.pom", repo.authentication, "b.pom")
      )
    )
  }

  test("sequential upload stops at the first error") {
    val upload = new RecordingUpload(failing = Set("b.jar"))
    val errors = upload
      .uploadFileSet(repo, fileSet("a.jar", "b.jar", "c.jar"), nop, None)
      .unsafeRun()(using ec)
    assertEquals(errors.map(_._1.elements.last), Seq("b.jar"))
    assertEquals(upload.calls.map(_._1.split('/').last).toList, List("a.jar", "b.jar"))
  }

  test("parallel upload attempts every file and returns all errors") {
    val upload = new RecordingUpload(failing = Set("b.jar", "c.jar"))
    val errors = upload
      .uploadFileSet(repo, fileSet("a.jar", "b.jar", "c.jar"), nop, Some(ec))
      .unsafeRun()(using ec)
    assertEquals(errors.map(_._1.elements.last).sorted, Seq("b.jar", "c.jar"))
    assertEquals(
      upload.calls.map(_._1.split('/').last).sorted.toList,
      List("a.jar", "b.jar", "c.jar")
    )
  }

  test("the logger is started and stopped around the upload, even when an upload throws") {
    val throwing = new Upload {
      def upload(
        url: String,
        authentication: Option[Authentication],
        content: Array[Byte],
        logger: UploadLogger,
        loggingIdOpt: Option[Object]
      ): Option[Upload.Error] = throw new RuntimeException("boom")
    }
    for (upload <- Seq(new RecordingUpload, throwing)) {
      val logger = new RecordingLogger
      val res    =
        scala.util.Try(upload.uploadFileSet(
          repo,
          fileSet("a.jar", "b.jar"),
          logger,
          None
        ).unsafeRun()(using ec))
      assertEquals(
        logger.events.toList,
        List("start", "uploadingSet(2)", "uploadedSet(2)", "stop"),
        clue = res
      )
    }
  }

  test("DummyUpload doesn't call the underlying upload") {
    val underlying = new RecordingUpload(failing = Set("a.jar"))
    val errors     = DummyUpload(underlying)
      .uploadFileSet(repo, fileSet("a.jar", "b.jar"), nop, None)
      .unsafeRun()(using ec)
    assertEquals(errors, Seq.empty)
    assertEquals(underlying.calls.toList, Nil)
  }
}
