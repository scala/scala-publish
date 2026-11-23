package scala.publish.signing

import java.nio.file.attribute.{FileAttribute, PosixFilePermission, PosixFilePermissions}
import java.nio.file.{FileSystem, Files, Path}

import scala.jdk.CollectionConverters.*

private[signing] object TempFiles {

  /** Initial attributes restricting access to temporary files to their owner, on `fs`
    *
    * Empty on file systems not supporting POSIX attributes (like on Windows), where passing them
    * would fail.
    */
  def privateAttributes(fs: FileSystem): Seq[FileAttribute[?]] =
    if fs.supportedFileAttributeViews().contains("posix") then
      Seq(
        PosixFilePermissions.asFileAttribute(
          Set(PosixFilePermission.OWNER_READ, PosixFilePermission.OWNER_WRITE).asJava
        )
      )
    else Nil

  /** Creates a temporary file in `dir`, only readable and writable by its owner where supported */
  def createPrivate(dir: Path, prefix: String, suffix: String): Path =
    Files.createTempFile(dir, prefix, suffix, privateAttributes(dir.getFileSystem)*)
}
