package scala.publish.fileset

/** Signatures and checksums of a file, like `foo.jar.asc`, `foo.jar.sha1` or `foo.jar.asc.md5` */
private[publish] object SideFiles {
  private val extensions = Set("asc", "md5", "sha1", "sha256", "sha512")

  /** `name` itself, followed by the names of the files it is a signature or checksum of
    *
    * For example, `foo.jar.asc.md5`, `foo.jar.asc` and `foo.jar` for `foo.jar.asc.md5`.
    */
  def bases(name: String): Iterator[String] =
    Iterator
      .iterate(Option(name)) {
        _.flatMap { n =>
          val idx = n.lastIndexOf('.')
          if idx > 0 && extensions(n.substring(idx + 1)) then Some(n.substring(0, idx))
          else None
        }
      }
      .takeWhile(_.nonEmpty)
      .flatten

  /** Whether `name` is `base`, or a signature or checksum of it */
  def isSameOrSideFileOf(name: String, base: String): Boolean =
    bases(name).contains(base)

  /** Whether `name` is one of `bases`, or a signature or checksum of one of them */
  def isSameOrSideFileOfAny(name: String, bases: Set[String]): Boolean =
    this.bases(name).exists(bases)
}
