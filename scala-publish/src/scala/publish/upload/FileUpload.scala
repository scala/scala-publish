package scala.publish.upload

import coursier.core.Authentication
import coursier.paths.Util

import java.net.URI
import java.nio.file.{Files, Path, Paths, StandardCopyOption}

import scala.publish.Content
import scala.publish.upload.logger.UploadLogger
import scala.util.Using
import scala.util.control.NonFatal

/** Copies
  * @param base
  */
final case class FileUpload(base: Path) extends Upload {
  private val base0 = base.normalize()
  def upload(
    url: String,
    authentication: Option[Authentication],
    content: Array[Byte],
    logger: UploadLogger,
    loggingId: Option[Object]
  ): Option[Upload.Error] =
    upload0(url, content.length.toLong, p => Files.write(p, content), logger, loggingId)

  /** Copies `content`, rather than loading it in memory */
  override def upload(
    url: String,
    authentication: Option[Authentication],
    content: Content,
    logger: UploadLogger,
    loggingIdOpt: Option[Object]
  ): Option[Upload.Error] =
    upload0(
      url,
      content.size(),
      p =>
        Using.resource(content.inputStream()) { in =>
          Files.copy(in, p, StandardCopyOption.REPLACE_EXISTING)
        },
      logger,
      loggingIdOpt
    )

  private def upload0(
    url: String,
    size: Long,
    write: Path => Unit,
    logger: UploadLogger,
    loggingId: Option[Object]
  ): Option[Upload.Error] = {
    val p = base0.resolve(Paths.get(new URI(url))).normalize()
    if p.startsWith(base0) then {
      logger.uploading(p.toString, loggingId, Some(size))
      val errorOpt =
        try {
          Util.createDirectories(p.getParent)
          write(p)
          None
        }
        catch {
          case NonFatal(e) =>
            Some(new Upload.Error.FileException(e))
        }
      logger.uploaded(p.toString, loggingId, errorOpt)

      errorOpt
    }
    else
      throw new Exception(s"Invalid path: $url (base: $base0, p: $p)")
  }
}
