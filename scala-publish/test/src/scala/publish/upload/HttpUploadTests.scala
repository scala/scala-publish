package scala.publish.upload

import sttp.client3.testing.SttpBackendStub
import sttp.client3.{Response, SttpClientException}
import sttp.model.{Header, StatusCode}

import scala.collection.mutable
import scala.publish.ScalaPublishTestSuite
import scala.publish.upload.logger.UploadLogger

class HttpUploadTests extends ScalaPublishTestSuite {

  private val url = "https://repo.example.test/maven/org/example/a.jar"
  private val id  = new Object

  /** Records the uploading / uploaded calls */
  private final class RecordingLogger extends UploadLogger {
    val events = mutable.ArrayBuffer.empty[String]
    override def uploading(url: String, idOpt: Option[Object], totalOpt: Option[Long]): Unit =
      events += s"uploading($url, ${idOpt.contains(id)}, $totalOpt)"
    override def uploaded(
      url: String,
      idOpt: Option[Object],
      errorOpt: Option[Upload.Error]
    ): Unit =
      events += s"uploaded($url, ${idOpt.contains(id)}, ${errorOpt.map(_.getClass.getSimpleName)})"
  }

  private def respondingWith(code: Int, headers: Header*) =
    HttpUpload(
      SttpBackendStub.synchronous.whenAnyRequest.thenRespond(
        Response("error body", StatusCode(code), "", headers)
      ),
      expect100Continue = false
    )

  test("an invalid URL is reported as an UploadError") {
    val upload = HttpUpload(SttpBackendStub.synchronous.whenAnyRequest.thenRespondOk(), false)
    val url    = "https://repo.example.test/%zz"
    upload.upload(url, None, Array[Byte](1), new UploadLogger {}) match {
      case Some(e: Upload.Error.UploadError) =>
        assertEquals(e.getMessage, s"Error uploading $url")
        e.getCause match {
          case c: IllegalArgumentException =>
            assert(c.getMessage.startsWith(s"Invalid URL: $url"), clue = c.getMessage)
          case other => fail(s"Unexpected cause: $other")
        }
      case other => fail(s"Expected an UploadError, got $other")
    }
  }

  test("successful uploads are reported to the logger as done") {
    val logger = new RecordingLogger
    assertEquals(respondingWith(201).upload(url, None, Array[Byte](1, 2), logger, Some(id)), None)
    assertEquals(
      logger.events.toList,
      List(s"uploading($url, true, Some(2))", s"uploaded($url, true, None)")
    )
  }

  for (
    (code, headers, expected) <- Seq(
      (500, Nil, "HttpError"),
      (401, Seq(Header("WWW-Authenticate", """Basic realm="Test Realm"""")), "Unauthorized")
    )
  )
    test(s"HTTP $code is returned, and reported to the logger") {
      val logger = new RecordingLogger
      val res = respondingWith(code, headers*).upload(url, None, Array[Byte](1), logger, Some(id))
      assertEquals(res.map(_.getClass.getSimpleName), Some(expected))
      assertEquals(
        logger.events.toList,
        List(s"uploading($url, true, Some(1))", s"uploaded($url, true, Some($expected))")
      )
    }

  test("invalid URLs are reported to the logger") {
    val logger = new RecordingLogger
    val url0   = "https://repo.example.test/%zz"
    HttpUpload(SttpBackendStub.synchronous.whenAnyRequest.thenRespondOk(), false)
      .upload(url0, None, Array[Byte](1), logger, Some(id))
    assertEquals(
      logger.events.toList,
      List(s"uploading($url0, true, Some(1))", s"uploaded($url0, true, Some(UploadError))")
    )
  }

  // like HttpURLConnectionUpload
  test("exceptions are reported to the logger as UploadError, then thrown") {
    val backend = SttpBackendStub.synchronous.whenAnyRequest.thenRespondF { r =>
      throw new SttpClientException.ConnectException(r, new java.net.ConnectException("refused"))
    }
    val logger = new RecordingLogger
    intercept[SttpClientException.ConnectException] {
      HttpUpload(backend, false).upload(url, None, Array[Byte](1), logger, Some(id))
    }
    assertEquals(
      logger.events.toList,
      List(s"uploading($url, true, Some(1))", s"uploaded($url, true, Some(UploadError))")
    )
  }

  test("uploads send the scala-publish User-Agent") {
    val agents  = mutable.ArrayBuffer.empty[Option[String]]
    val backend = SttpBackendStub.synchronous.whenAnyRequest.thenRespondF { r =>
      agents += r.headers.find(_.name.equalsIgnoreCase("User-Agent")).map(_.value)
      Response("", StatusCode.Created)
    }
    HttpUpload(backend, false).upload(url, None, Array[Byte](1), new UploadLogger {})
    assertEquals(agents.toList, List(Some(scala.publish.util.UserAgent.current())))
  }
}
