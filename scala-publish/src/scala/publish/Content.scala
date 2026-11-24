package scala.publish

import java.io.{ByteArrayInputStream, InputStream}
import java.nio.file.{Files, Path}
import java.time.Instant

/** Content of a file, either on disk or in memory.
  */
sealed abstract class Content extends Product with Serializable {
  def lastModified: Instant

  /** The whole content, in memory. Prefer [[inputStream]] for possibly large content. */
  def content(): Array[Byte]

  /** Streams the content. The caller is responsible for closing the returned stream. */
  def inputStream(): InputStream

  /** Size of the content, in bytes */
  def size(): Long

  def pathOpt: Option[Path] = None
}

object Content {
  final case class File(path: Path) extends Content {
    def lastModified: Instant =
      Files.getLastModifiedTime(path).toInstant
    def content(): Array[Byte] =
      Files.readAllBytes(path)
    def inputStream(): InputStream =
      Files.newInputStream(path)
    def size(): Long =
      Files.size(path)
    override def pathOpt: Option[Path] =
      Some(path)
  }

  final case class InMemory(lastModified: Instant, content0: Array[Byte]) extends Content {
    def content(): Array[Byte] =
      content0
    def inputStream(): InputStream =
      new ByteArrayInputStream(content0)
    def size(): Long =
      content0.length.toLong
  }
}
