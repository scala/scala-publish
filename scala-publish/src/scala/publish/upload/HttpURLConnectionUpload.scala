package scala.publish.upload

import coursier.core.Authentication

import java.io.{ByteArrayOutputStream, InputStream, OutputStream}
import java.net.{HttpURLConnection, URI}
import java.nio.charset.StandardCharsets

import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.jdk.CollectionConverters.*
import scala.publish.Content
import scala.publish.upload.logger.UploadLogger
import scala.publish.util.{BasicRealm, UserAgent}
import scala.util.control.NonFatal
import scala.util.{Try, Using}

final case class HttpURLConnectionUpload(
  urlSuffix: String,
  readTimeoutMsOpt: Option[Int],
  connectTimeoutMsOpt: Option[Int]
) extends Upload {
  def upload(
    url: String,
    authentication: Option[Authentication],
    content: Array[Byte],
    logger: UploadLogger,
    loggingIdOpt: Option[Object]
  ): Option[Upload.Error] =
    upload0(url, authentication, content.length.toLong, _.write(content), logger, loggingIdOpt)

  /** Streams `content`, rather than loading it in memory */
  override def upload(
    url: String,
    authentication: Option[Authentication],
    content: Content,
    logger: UploadLogger,
    loggingIdOpt: Option[Object]
  ): Option[Upload.Error] =
    upload0(
      url,
      authentication,
      content.size(),
      os => Using.resource(content.inputStream())(_.transferTo(os)),
      logger,
      loggingIdOpt
    )

  private def upload0(
    url: String,
    authentication: Option[Authentication],
    size: Long,
    writeContent: OutputStream => Unit,
    logger: UploadLogger,
    loggingIdOpt: Option[Object]
  ): Option[Upload.Error] = {
    logger.uploading(url, loggingIdOpt, Some(size))

    val res = Try {
      val url0 = URI.create(url + urlSuffix).toURL

      val conn = url0.openConnection().asInstanceOf[HttpURLConnection]

      conn.setRequestMethod("PUT")
      readTimeoutMsOpt
        .foreach(conn.setReadTimeout)
      connectTimeoutMsOpt
        .foreach(conn.setConnectTimeout)

      for (auth <- authentication; (k, v) <- auth.allHttpHeaders)
        conn.setRequestProperty(k, v)

      conn.setDoOutput(true)
      // sets Content-Length, and streams the body rather than buffering it in memory
      conn.setFixedLengthStreamingMode(size)

      conn.setRequestProperty("Content-Type", "application/octet-stream")
      conn.setRequestProperty("User-Agent", UserAgent.current())

      var is: InputStream  = null
      var es: InputStream  = null
      var os: OutputStream = null

      try {
        os = conn.getOutputStream
        writeContent(os)
        os.close()

        val code = conn.getResponseCode
        if code == 401 then {
          val realmOpt = Option(conn.getHeaderField("WWW-Authenticate")).collect {
            case BasicRealm(r) => r
          }
          Some(new Upload.Error.Unauthorized(url, realmOpt))
        }
        else if code / 100 == 2 then None
        else {
          val content = {
            es = Option(conn.getErrorStream)
              .orElse(Try(conn.getInputStream).toOption).orNull

            if es != null then {
              val buf  = Array.ofDim[Byte](16384)
              val baos = new ByteArrayOutputStream
              var read = -1
              while ({
                read = es.read(buf)
                read >= 0
              })
                baos.write(buf, 0, read)
              es.close()
              // FIXME Adjust charset with headers?
              Try(new String(baos.toByteArray, StandardCharsets.UTF_8))
                .getOrElse("")
            }
            else ""
          }

          Some(
            new Upload.Error.HttpError(
              code,
              conn.getHeaderFields.asScala.view.mapValues(_.asScala.toList).iterator.toMap,
              content
            )
          )
        }
      }
      finally {
        // Trying to ensure the same connection is being re-used across requests
        // see https://docs.oracle.com/javase/8/docs/technotes/guides/net/http-keepalive.html
        try {
          if os == null then os = conn.getOutputStream
          if os != null then os.close()
        }
        catch {
          case NonFatal(_) =>
        }
        try {
          if is == null then is = conn.getInputStream
          if is != null then {
            val buf = Array.ofDim[Byte](16384)
            while (is.read(buf) > 0) {}
            is.close()
          }
        }
        catch {
          case NonFatal(_) =>
        }
        try {
          if es == null then es = conn.getErrorStream
          if es != null then {
            val buf = Array.ofDim[Byte](16384)
            while (es.read(buf) > 0) {}
            es.close()
          }
        }
        catch {
          case NonFatal(_) =>
        }
      }
    }

    logger.uploaded(
      url,
      loggingIdOpt,
      res.fold(e => Some(new Upload.Error.UploadError(url, e)), x => x)
    )

    res.get
  }
}

object HttpURLConnectionUpload {
  def create(): Upload                  = HttpURLConnectionUpload("", None, None)
  def create(urlSuffix: String): Upload = HttpURLConnectionUpload(urlSuffix, None, None)

  /** Timeouts of the HTTP requests sent by [[HttpURLConnectionUpload]].
    *
    * @param connect
    *   timeout for establishing the connection, none if unset
    * @param read
    *   timeout for reading the response once connected, none if unset
    */
  final case class Timeouts(
    connect: Option[FiniteDuration] = None,
    read: Option[FiniteDuration] = None
  ) {
    require(connect.forall(_ >= Duration.Zero), s"Negative connect timeout: $connect")
    require(read.forall(_ >= Duration.Zero), s"Negative read timeout: $read")
  }

  private def toMillis(duration: FiniteDuration): Int =
    math.min(duration.toMillis, Int.MaxValue.toLong).toInt

  /** Create a HttpURLConnectionUpload with the passed timeouts */
  def create(timeouts: Timeouts): Upload =
    create(timeouts, "")

  /** Create a HttpURLConnectionUpload with the passed timeouts, appending `urlSuffix` to URLs */
  def create(timeouts: Timeouts, urlSuffix: String): Upload =
    HttpURLConnectionUpload(urlSuffix, timeouts.read.map(toMillis), timeouts.connect.map(toMillis))

  /** Create a HttpURLConnectionUpload with a read timeout
    * @param readTimeoutMs
    *   the response read timeout in miliseconds
    * @param connetionTimeoutMs
    *   the connection timeout in miliseconds
    * @param urlSuffix
    *   the suffix to append to the url
    */
  @deprecated(
    "Use create(Timeouts(connect = ..., read = ...)) instead, these positional timeouts are easily swapped",
    "1.1.0"
  )
  def create(
    readTimeoutMs: Option[Int],
    connectionTimeoutMs: Option[Int],
    urlSuffix: String = ""
  ): Upload =
    HttpURLConnectionUpload(urlSuffix, readTimeoutMs, connectionTimeoutMs)
}
