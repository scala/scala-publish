package scala.publish.util

/** `User-Agent` sent with the HTTP requests of scala-publish
  *
  * Maven Central asks tools to identify themselves with a specific `User-Agent`, rather than the
  * default one of their HTTP client (see https://central.sonatype.org/faq/429-tooling-provider/).
  * Tools using scala-publish can identify themselves too, via the `scala.publish.http.agent` Java
  * property (e.g. `Scala-CLI/1.10 (+https://github.com/VirtusLab/scala-cli) scala-publish/1.1`).
  */
object UserAgent {

  /** Java property overriding the default `User-Agent` */
  val property: String = "scala.publish.http.agent"

  private val projectUrl = "https://github.com/scala/scala-publish"
  private val MajorMinor = """(\d+)\.(\d+)(?:[.+-].*)?""".r

  /** The scala-publish `User-Agent` for `version`, keeping only its major and minor parts */
  def apply(version: String): String = {
    val versionPart = version match {
      case MajorMinor(major, minor) => s"/$major.$minor"
      case _                        => ""
    }
    s"scala-publish$versionPart (+$projectUrl)"
  }

  /** The `User-Agent` of this version of scala-publish */
  lazy val default: String = apply(BuildInfo.version)

  /** The `User-Agent` to send: the `scala.publish.http.agent` Java property if set, else
    * [[default]]
    */
  def current(props: Map[String, String] = sys.props.toMap): String =
    props.getOrElse(property, default)
}
