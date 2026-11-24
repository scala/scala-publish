package scala.publish.upload

import coursier.core.Authentication

import scala.publish.Content
import scala.publish.upload.logger.UploadLogger
import scala.publish.util.{EmaRetryParams, Retry}

/** Retries uploads failing with transient errors (see [[Upload.Error.transient]], e.g. HTTP 5xx)
  *
  * @param underlying
  *   the upload to retry
  * @param retryParams
  *   maximum number of attempts, wait before the first retry, and factor applied to that wait
  *   before each subsequent retry
  * @param sleep
  *   waits for the passed number of milliseconds
  */
final case class RetryingUpload(
  underlying: Upload,
  retryParams: EmaRetryParams = RetryingUpload.defaultRetryParams,
  sleep: Long => Unit = RetryingUpload.threadSleep
) extends Upload {
  def upload(
    url: String,
    authentication: Option[Authentication],
    content: Array[Byte],
    logger: UploadLogger,
    loggingIdOpt: Option[Object]
  ): Option[Upload.Error] =
    retrying(underlying.upload(url, authentication, content, logger, loggingIdOpt))

  override def upload(
    url: String,
    authentication: Option[Authentication],
    content: Content,
    logger: UploadLogger,
    loggingIdOpt: Option[Object]
  ): Option[Upload.Error] =
    retrying(underlying.upload(url, authentication, content, logger, loggingIdOpt))

  private def retrying(upload: => Option[Upload.Error]): Option[Upload.Error] =
    Retry(retryParams, sleep)((res: Option[Upload.Error]) => res.exists(_.transient))(_ => upload)
}

object RetryingUpload {
  def defaultRetryParams: EmaRetryParams  = EmaRetryParams(3, 1000L, 2.0f)
  private def threadSleep(ms: Long): Unit = Retry.threadSleep(ms)
}
