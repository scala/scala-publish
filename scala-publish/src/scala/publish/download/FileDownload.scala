package scala.publish.download

import coursier.core.Authentication

import java.net.URI
import java.nio.file.{Files, Path, Paths}
import java.time.Instant

import scala.publish.download.logger.DownloadLogger
import scala.util.Try
import scala.util.control.NonFatal

/** Copies
  * @param base
  */
final case class FileDownload(base: Path) extends Download {
  private val base0 = base.normalize()
  def downloadIfExists(
    url: String,
    authentication: Option[Authentication],
    logger: DownloadLogger
  ): Option[(Option[Instant], Array[Byte])] = {

    // URLs like file:///…, as built from repository roots, or paths relative to base
    val fileUriOpt = Try(new URI(url)).toOption.filter(_.getScheme == "file")
    val p          = fileUriOpt
      .fold(base0.resolve(url))(uri => base0.resolve(Paths.get(uri)))
      .normalize()
    if p.startsWith(base0) then {
      logger.downloadingIfExists(url)
      val res =
        try if Files.isRegularFile(p) then {
            val lastModified = Files.getLastModifiedTime(p).toInstant
            Right(Some((Some(lastModified), Files.readAllBytes(p))))
          }
          else Right(None)
        catch { case NonFatal(e) => Left(e) }
      logger.downloadedIfExists(
        url,
        res.toOption.flatMap(_.map(_._2.length)),
        res.left.toOption.map(e => new Download.Error.FileException(e))
      )

      res match {
        case Left(e)  => throw new Exception(e)
        case Right(v) => v
      }
    }
    else
      throw new Exception(s"Invalid path: $url (base: $base0, p: $p)")
  }
}
