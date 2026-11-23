package scala.publish.util

import sttp.model.Uri

private[publish] object Urls {

  /** Parses `url`, failing with an [[IllegalArgumentException]] if it's not a valid URL */
  def parse(url: String): Either[IllegalArgumentException, Uri] =
    Uri.parse(url).left.map(err => new IllegalArgumentException(s"Invalid URL: $url ($err)"))
}
