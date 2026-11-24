package scala.publish.upload

import com.sun.net.httpserver.{HttpExchange, HttpServer}
import coursier.core.Authentication

import java.net.{ConnectException, InetSocketAddress, ServerSocket}
import java.nio.charset.StandardCharsets

import scala.collection.mutable
import scala.concurrent.duration.DurationInt
import scala.publish.ScalaPublishTestSuite
import scala.publish.upload.logger.UploadLogger

/** [[HttpURLConnectionUpload]] is what Scala CLI uses for every http(s) repository. */
class HttpURLConnectionUploadTests extends ScalaPublishTestSuite {

  private final case class Received(
    method: String,
    path: String,
    body: Seq[Byte],
    authorization: Option[String],
    contentType: Option[String] = None
  )

  /** Starts a local HTTP server answering every request with `respond`, and records requests. */
  private def withServer[T](respond: HttpExchange => Unit)(
    f: (String, () => Seq[Received]) => T
  ): T = {
    val received = mutable.ArrayBuffer.empty[Received]
    val server   = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0)
    server.createContext(
      "/",
      exchange =>
        try {
          val body = exchange.getRequestBody.readAllBytes().toSeq
          received.synchronized {
            received += Received(
              exchange.getRequestMethod,
              exchange.getRequestURI.getPath,
              body,
              Option(exchange.getRequestHeaders.getFirst("Authorization")),
              Option(exchange.getRequestHeaders.getFirst("Content-Type"))
            )
          }
          respond(exchange)
        }
        finally exchange.close()
    )
    server.start()
    try f(
        s"http://127.0.0.1:${server.getAddress.getPort}",
        () => received.synchronized(received.toList)
      )
    finally server.stop(0)
  }

  private def reply(
    exchange: HttpExchange,
    code: Int,
    body: String = "",
    headers: Seq[(String, String)] = Nil
  ): Unit = {
    for ((k, v) <- headers) exchange.getResponseHeaders.add(k, v)
    val b = body.getBytes(StandardCharsets.UTF_8)
    exchange.sendResponseHeaders(code, if b.isEmpty then -1 else b.length.toLong)
    if b.nonEmpty then exchange.getResponseBody.write(b)
  }

  private val upload = HttpURLConnectionUpload.create()
  private val nop    = new UploadLogger {}

  test("successful PUT sends the content and the authentication header") {
    withServer(reply(_, 201)) { (base, received) =>
      val auth = Authentication("user", "secret")
      val res  = upload.upload(
        s"$base/org/example/lib_3/1.0.0/lib_3-1.0.0.jar",
        Some(auth),
        Array[Byte](1, 2, 3),
        nop
      )
      assertEquals(res, None)
      assertEquals(
        received(),
        Seq(Received(
          "PUT",
          "/org/example/lib_3/1.0.0/lib_3-1.0.0.jar",
          Seq[Byte](1, 2, 3),
          auth.allHttpHeaders.toMap.get("Authorization"),
          Some("application/octet-stream")
        ))
      )
    }
  }

  test("401 reports Unauthorized with the realm from the WWW-Authenticate response header") {
    withServer(
      reply(_, 401, headers = Seq("WWW-Authenticate" -> """Basic realm="Test Realm""""))
    ) { (base, _) =>
      val url = s"$base/lib.jar"
      upload.upload(url, None, Array[Byte](1), nop) match {
        case Some(e: Upload.Error.Unauthorized) =>
          assertEquals(e.getMessage, s"Unauthorized ($url, Test Realm)")
        case other => fail(s"Expected Unauthorized, got $other")
      }
    }
  }

  test("401 without WWW-Authenticate reports Unauthorized with no realm") {
    withServer(reply(_, 401)) { (base, _) =>
      val url = s"$base/lib.jar"
      upload.upload(url, None, Array[Byte](1), nop) match {
        case Some(e: Upload.Error.Unauthorized) =>
          assertEquals(e.getMessage, s"Unauthorized ($url, [no realm])")
        case other => fail(s"Expected Unauthorized, got $other")
      }
    }
  }

  // Scala CLI parses this message with "HTTP (\\d+)\n.*" (see Publish.scala in scala-cli)
  for (code <- Seq(403, 500))
    test(s"HTTP $code reports an HttpError with the code and response body") {
      withServer(reply(_, code, body = "nope")) { (base, _) =>
        upload.upload(s"$base/lib.jar", None, Array[Byte](1), nop) match {
          case Some(e: Upload.Error.HttpError) =>
            assertEquals(e.getMessage, s"HTTP $code\nnope")
            assertEquals(e.transient, code >= 500)
          case other => fail(s"Expected HttpError, got $other")
        }
      }
    }

  test("the logger gets the error reported by the upload") {
    val logged = mutable.ArrayBuffer.empty[Option[Upload.Error]]
    val logger = new UploadLogger {
      override def uploaded(
        url: String,
        idOpt: Option[Object],
        errorOpt: Option[Upload.Error]
      ): Unit = logged += errorOpt
    }
    withServer(
      reply(_, 401, headers = Seq("WWW-Authenticate" -> """Basic realm="Test Realm""""))
    ) { (base, _) =>
      val res = upload.upload(s"$base/lib.jar", None, Array[Byte](1), logger)
      assertEquals(logged.toList.map(_.map(_.getMessage)), List(res.map(_.getMessage)))
    }
  }

  test("create(Timeouts) maps the connect and read timeouts to the right settings") {
    val timeouts =
      HttpURLConnectionUpload.Timeouts(connect = Some(1.second), read = Some(2.seconds))
    assertEquals(
      HttpURLConnectionUpload.create(timeouts),
      HttpURLConnectionUpload(
        urlSuffix = "",
        readTimeoutMsOpt = Some(2000),
        connectTimeoutMsOpt = Some(1000)
      )
    )
    assertEquals(
      HttpURLConnectionUpload.create(timeouts, urlSuffix = ".suffix"),
      HttpURLConnectionUpload(
        ".suffix",
        readTimeoutMsOpt = Some(2000),
        connectTimeoutMsOpt = Some(1000)
      )
    )
  }

  test("Timeouts default to no timeout") {
    assertEquals(
      HttpURLConnectionUpload.create(HttpURLConnectionUpload.Timeouts()),
      HttpURLConnectionUpload("", None, None)
    )
  }

  test("Timeouts larger than Int.MaxValue milliseconds are capped") {
    val upload = HttpURLConnectionUpload.create(
      HttpURLConnectionUpload.Timeouts(connect = Some(100000.days), read = Some(100000.days))
    )
    assertEquals(upload, HttpURLConnectionUpload("", Some(Int.MaxValue), Some(Int.MaxValue)))
  }

  test("negative Timeouts are rejected") {
    val minusOneSecond = 1.second * -1
    intercept[IllegalArgumentException](HttpURLConnectionUpload.Timeouts(connect =
      Some(minusOneSecond)
    ))
    intercept[IllegalArgumentException](HttpURLConnectionUpload.Timeouts(read =
      Some(minusOneSecond)
    ))
  }

  test("the read timeout from Timeouts is applied to the request") {
    withServer { exchange =>
      Thread.sleep(1500)
      reply(exchange, 201)
    } { (base, _) =>
      val upload = HttpURLConnectionUpload.create(
        HttpURLConnectionUpload.Timeouts(read = Some(200.millis))
      )
      val start = System.nanoTime()
      intercept[java.net.SocketTimeoutException] {
        upload.upload(s"$base/lib.jar", None, Array[Byte](1), nop)
      }
      val elapsedMs = (System.nanoTime() - start) / 1000000L
      assert(elapsedMs < 1200L, clue = s"took $elapsedMs ms")
    }
  }

  test("the URL suffix is appended to the upload URL") {
    withServer(reply(_, 201)) { (base, received) =>
      val res = HttpURLConnectionUpload.create(".suffix")
        .upload(s"$base/lib.jar", None, Array[Byte](1), nop)
      assertEquals(res, None)
      assertEquals(received().map(_.path), Seq("/lib.jar.suffix"))
    }
  }

  test("connection failures are thrown, and reported to the logger as UploadError") {
    val port = {
      val socket = new ServerSocket(0)
      try socket.getLocalPort
      finally socket.close()
    }
    val logged = mutable.ArrayBuffer.empty[Option[Upload.Error]]
    val logger = new UploadLogger {
      override def uploaded(
        url: String,
        idOpt: Option[Object],
        errorOpt: Option[Upload.Error]
      ): Unit = logged += errorOpt
    }
    val url = s"http://127.0.0.1:$port/lib.jar"
    intercept[ConnectException](upload.upload(url, None, Array[Byte](1), logger))
    logged.toList match {
      case List(Some(e: Upload.Error.UploadError)) =>
        assertEquals(e.getMessage, s"Error uploading $url")
        assert(e.getCause.isInstanceOf[ConnectException], clue = e.getCause)
      case other => fail(s"Expected a logged UploadError, got $other")
    }
  }

  test("file contents are streamed with their size as Content-Length, not chunked") {
    val f = java.nio.file.Files.createTempFile("scala-publish-upload", ".jar")
    try {
      val bytes = Array.tabulate[Byte](100000)(i => (i % 127).toByte)
      java.nio.file.Files.write(f, bytes)
      val headers = mutable.ArrayBuffer.empty[(Option[String], Option[String])]
      withServer { exchange =>
        headers += (
          (
            Option(exchange.getRequestHeaders.getFirst("Content-Length")),
            Option(exchange.getRequestHeaders.getFirst("Transfer-Encoding"))
          )
        )
        reply(exchange, 201)
      } { (base, received) =>
        val res =
          upload.upload(s"$base/lib.jar", None, scala.publish.Content.File(f), nop, None)
        assertEquals(res, None)
        assertEquals(received().map(_.body.length), Seq(100000))
        assertEquals(received().map(_.body), Seq(bytes.toSeq))
        assertEquals(headers.toList, List((Some("100000"), None)))
      }
    }
    finally java.nio.file.Files.deleteIfExists(f)
  }
}
