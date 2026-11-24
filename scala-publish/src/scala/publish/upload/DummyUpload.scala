package scala.publish.upload

import coursier.core.Authentication

import scala.publish.Content
import scala.publish.upload.logger.UploadLogger

final case class DummyUpload(underlying: Upload) extends Upload {
  def upload(
    url: String,
    authentication: Option[Authentication],
    content: Array[Byte],
    logger: UploadLogger,
    loggingId: Option[Object]
  ): Option[Upload.Error] = None

  override def upload(
    url: String,
    authentication: Option[Authentication],
    content: Content,
    logger: UploadLogger,
    loggingIdOpt: Option[Object]
  ): Option[Upload.Error] = None
}
