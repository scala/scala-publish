package scala.publish.upload

import sttp.client3.testing.SttpBackendStub

import scala.publish.ScalaPublishTestSuite
import scala.publish.upload.logger.UploadLogger

class HttpUploadTests extends ScalaPublishTestSuite {

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
}
