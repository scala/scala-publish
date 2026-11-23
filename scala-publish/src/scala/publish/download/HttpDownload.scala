package scala.publish.download

import coursier.core.Authentication
import sttp.client3.*
import sttp.model.{Header, Uri}

import java.nio.charset.StandardCharsets
import java.time.format.DateTimeFormatter
import java.time.{Instant, ZonedDateTime}

import scala.publish.download.logger.DownloadLogger
import scala.publish.util.BasicRealm
import scala.util.Try
import scala.util.control.NonFatal

final case class HttpDownload(
  backend: SttpBackend[Identity, Any]
) extends Download {
  def downloadIfExists(
    url: String,
    authentication: Option[Authentication],
    logger: DownloadLogger
  ): Option[(Option[Instant], Array[Byte])] = {

    val uri = Uri.parse(url) match {
      case Left(_)     => ???
      case Right(uri0) => uri0
    }

    val authHeaders = authentication
      .toSeq
      .flatMap(_.allHttpHeaders)
      .map {
        case (k, v) => new Header(k, v)
      }

    val req = basicRequest
      .headers(authHeaders*)
      .response(asByteArrayAlways)
      .get(uri)
    logger.downloadingIfExists(url)

    val res: Either[Throwable, Option[(Option[Instant], Array[Byte])]] =
      try {
        val resp = req.send(backend)

        if resp.isSuccess then {
          val lastModifiedOpt = resp.header("Last-Modified").flatMap { value =>
            Try(ZonedDateTime.parse(value, DateTimeFormatter.RFC_1123_DATE_TIME).toInstant)
              .toOption
          }
          Right(Some((lastModifiedOpt, resp.body)))
        }
        // only "not found" responses mean the file doesn't exist
        else if resp.code.code == 404 || resp.code.code == 410 then Right(None)
        else if resp.code.code == 401 then {
          val realmOpt = resp.header("WWW-Authenticate").collect {
            case BasicRealm(r) => r
          }
          Left(new Download.Error.Unauthorized(url, realmOpt))
        }
        else
          Left(
            new Download.Error.HttpError(
              url,
              resp.code.code,
              resp.headers.groupBy(_.name).map((k, l) => (k, l.map(_.value))),
              new String(resp.body, StandardCharsets.UTF_8)
            )
          )
      }
      catch {
        case NonFatal(e) =>
          Left(new Download.Error.DownloadError(url, e))
      }

    res match {
      case Right(value) =>
        logger.downloadedIfExists(url, value.map(_._2.length.toLong), None)
        value
      case Left(e: Download.Error.DownloadError) =>
        logger.downloadedIfExists(url, None, Some(e))
        throw e.getCause
      case Left(e) =>
        logger.downloadedIfExists(url, None, Some(e))
        throw e
    }
  }
}
