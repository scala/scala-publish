package scala.publish.download

import coursier.core.Authentication
import sttp.client3.testing.SttpBackendStub
import sttp.client3.{Request, Response}
import sttp.model.{Header, StatusCode}

import java.nio.charset.StandardCharsets
import java.time.Instant

import scala.collection.mutable
import scala.publish.ScalaPublishTestSuite
import scala.publish.download.logger.DownloadLogger

class HttpDownloadTests extends ScalaPublishTestSuite {

  private val url = "https://repo.example.test/maven/org/example/lib_3/maven-metadata.xml"

  /** Answers every request with `response`, recording requests */
  private final class Server(response: Response[Array[Byte]]) {
    val requests = mutable.ArrayBuffer.empty[Request[?, ?]]
    val backend: SttpBackendStub[sttp.client3.Identity, Any] =
      SttpBackendStub.synchronous.whenAnyRequest.thenRespondF { r =>
        requests += r
        response
      }
  }

  private def response(
    code: Int,
    body: String = "",
    headers: Seq[(String, String)] = Nil
  ): Response[Array[Byte]] =
    Response(
      body.getBytes(StandardCharsets.UTF_8),
      StatusCode(code),
      "",
      headers.map((k, v) => Header(k, v))
    )

  private final class RecordingLogger extends DownloadLogger {
    val downloaded = mutable.ArrayBuffer.empty[(Option[Long], Option[Throwable])]
    override def downloadedIfExists(
      url: String,
      size: Option[Long],
      errorOpt: Option[Throwable]
    ): Unit = downloaded += size -> errorOpt
  }

  private def download(
    server: Server,
    logger: DownloadLogger = new DownloadLogger {}
  ): Option[(Option[Instant], String)] =
    HttpDownload(server.backend)
      .downloadIfExists(url, Some(Authentication("user", "secret")), logger)
      .map((lm, b) => (lm, new String(b, StandardCharsets.UTF_8)))

  test("200 returns the content, its last modified time, and sends authentication") {
    val server = new Server(response(
      200,
      "<metadata/>",
      Seq("Last-Modified" -> "Tue, 02 Jan 2024 03:04:05 GMT")
    ))
    assertEquals(
      download(server),
      Some((Some(Instant.parse("2024-01-02T03:04:05Z")), "<metadata/>"))
    )
    assertEquals(
      server.requests.toList.map(_.headers.find(_.name == "Authorization").map(_.value)),
      List(Authentication("user", "secret").allHttpHeaders.toMap.get("Authorization"))
    )
  }

  test("an invalid Last-Modified header is ignored") {
    val server = new Server(response(200, "<metadata/>", Seq("Last-Modified" -> "yesterday")))
    assertEquals(download(server), Some((None, "<metadata/>")))
  }

  for (code <- Seq(404, 410))
    test(s"HTTP $code means the file doesn't exist") {
      assertEquals(download(new Server(response(code))), None)
    }

  test("HTTP 401 is an Unauthorized error, with the realm") {
    val server = new Server(response(
      401,
      headers = Seq("WWW-Authenticate" -> """Basic realm="Test Realm"""")
    ))
    val e = intercept[Download.Error.Unauthorized](download(server))
    assertEquals(e.getMessage, s"Unauthorized ($url, Test Realm)")
  }

  for (code <- Seq(403, 500, 503))
    test(s"HTTP $code is an HttpError with the response body") {
      val e = intercept[Download.Error.HttpError](download(new Server(response(code, "nope"))))
      assertEquals(e.getMessage, s"$url: HTTP $code\nnope")
      assertEquals(e.transient, code >= 500)
    }

  test("errors are reported to the logger") {
    val logger = new RecordingLogger
    intercept[Download.Error.HttpError](download(new Server(response(403)), logger))
    logger.downloaded.toList match {
      case List((None, Some(_: Download.Error.HttpError))) =>
      case other                                           => fail(s"Unexpected: $other")
    }
  }

  test("an invalid URL is an IllegalArgumentException") {
    val e = intercept[IllegalArgumentException] {
      HttpDownload(new Server(response(200)).backend)
        .downloadIfExists("https://repo.example.test/%zz", None, new DownloadLogger {})
    }
    assert(
      e.getMessage.startsWith("Invalid URL: https://repo.example.test/%zz"),
      clue = e.getMessage
    )
  }

  test("downloads send the scala-publish User-Agent") {
    val server = new Server(response(404))
    download(server)
    assertEquals(
      server.requests.toList.map(
        _.headers.find(_.name.equalsIgnoreCase("User-Agent")).map(_.value)
      ),
      List(Some(scala.publish.util.UserAgent.current()))
    )
  }
}
