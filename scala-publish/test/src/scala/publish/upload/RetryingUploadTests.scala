package scala.publish.upload

import coursier.core.Authentication
import coursier.maven.MavenRepository

import java.time.Instant
import java.util.concurrent.Executors

import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.publish.fileset.{FileSet, Path}
import scala.publish.upload.logger.UploadLogger
import scala.publish.util.EmaRetryParams
import scala.publish.{Content, ScalaPublishTestSuite}

class RetryingUploadTests extends ScalaPublishTestSuite {

  private val url    = "https://repo.example.test/maven/org/example/a.jar"
  private val nop    = new UploadLogger {}
  private val params = EmaRetryParams(attempts = 4, initialWaitDurationMs = 100L, factor = 2.0f)

  private def httpError(code: Int) = new Upload.Error.HttpError(code, Map.empty, s"error $code")

  /** Returns the passed results in turn (then the last one), recording calls */
  private final class ScriptedUpload(results: Option[Upload.Error]*) extends Upload {
    val calls = mutable.ArrayBuffer.empty[String]
    def upload(
      url: String,
      authentication: Option[Authentication],
      content: Array[Byte],
      logger: UploadLogger,
      loggingIdOpt: Option[Object]
    ): Option[Upload.Error] = calls.synchronized {
      calls += url
      results(math.min(calls.length, results.length) - 1)
    }
  }

  private def retrying(underlying: Upload): (RetryingUpload, mutable.ArrayBuffer[Long]) = {
    val sleeps = mutable.ArrayBuffer.empty[Long]
    (RetryingUpload(underlying, params, ms => sleeps.synchronized(sleeps += ms)), sleeps)
  }

  test("transient errors are retried with exponential backoff, until success") {
    val underlying       = new ScriptedUpload(Some(httpError(503)), Some(httpError(500)), None)
    val (upload, sleeps) = retrying(underlying)
    assertEquals(upload.upload(url, None, Array[Byte](1), nop), None)
    assertEquals(underlying.calls.length, 3)
    assertEquals(sleeps.toList, List(100L, 200L))
  }

  test("retrying stops after the maximum number of attempts, returning the last error") {
    val underlying       = new ScriptedUpload(Some(httpError(503)))
    val (upload, sleeps) = retrying(underlying)
    upload.upload(url, None, Array[Byte](1), nop) match {
      case Some(e: Upload.Error.HttpError) => assertEquals(e.getMessage, "HTTP 503\nerror 503")
      case other                           => fail(s"Expected an HttpError, got $other")
    }
    assertEquals(underlying.calls.length, 4)
    assertEquals(sleeps.toList, List(100L, 200L, 400L))
  }

  for (
    (description, error) <- Seq(
      "HTTP 403"     -> httpError(403),
      "Unauthorized" -> new Upload.Error.Unauthorized(url, None),
      "I/O errors"   -> new Upload.Error.FileException(new java.io.IOException("disk full"))
    )
  )
    test(s"non-transient errors are not retried ($description)") {
      val underlying       = new ScriptedUpload(Some(error))
      val (upload, sleeps) = retrying(underlying)
      assertEquals(upload.upload(url, None, Array[Byte](1), nop), Some(error))
      assertEquals(underlying.calls.length, 1)
      assertEquals(sleeps.toList, Nil)
    }

  test("successful uploads are attempted once") {
    val underlying       = new ScriptedUpload(None)
    val (upload, sleeps) = retrying(underlying)
    assertEquals(upload.upload(url, None, Array[Byte](1), nop), None)
    assertEquals(underlying.calls.length, 1)
    assertEquals(sleeps.toList, Nil)
  }

  test("uploadFileSet retries each file") {
    val underlying  = new ScriptedUpload(Some(httpError(502)), None)
    val (upload, _) = retrying(underlying)
    val fs          = FileSet(Seq(
      Path(Seq("org", "example", "a.jar")) -> Content.InMemory(Instant.EPOCH, Array[Byte](1))
    ))
    val pool = Executors.newSingleThreadExecutor()
    try {
      val errors = upload
        .uploadFileSet(MavenRepository("https://repo.example.test/maven"), fs, nop, None)
        .unsafeRun()(using ExecutionContext.fromExecutorService(pool))
      assertEquals(errors, Seq.empty)
      assertEquals(underlying.calls.toList, List(url, url))
    }
    finally pool.shutdown()
  }

  test("retries go through the Content overload of the underlying upload") {
    val contents   = mutable.ArrayBuffer.empty[Content]
    val underlying = new Upload {
      def upload(
        url: String,
        authentication: Option[Authentication],
        content: Array[Byte],
        logger: UploadLogger,
        loggingIdOpt: Option[Object]
      ): Option[Upload.Error] = fail("the Array[Byte] overload shouldn't be called")
      override def upload(
        url: String,
        authentication: Option[Authentication],
        content: Content,
        logger: UploadLogger,
        loggingIdOpt: Option[Object]
      ): Option[Upload.Error] = {
        contents += content
        if contents.length < 2 then Some(httpError(503)) else None
      }
    }
    val (upload, _) = retrying(underlying)
    val content     = Content.InMemory(Instant.EPOCH, Array[Byte](1))
    assertEquals(upload.upload(url, None, content, nop, None), None)
    assertEquals(contents.toList, List(content, content))
  }
}
