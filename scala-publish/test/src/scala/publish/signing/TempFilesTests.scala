package scala.publish.signing

import java.net.URI
import java.nio.file.attribute.PosixFilePermission
import java.nio.file.{FileSystems, Files, Path as NioPath}

import scala.jdk.CollectionConverters.*
import scala.publish.ScalaPublishTestSuite
import scala.util.Properties

/** Temporary files holding content to sign and signatures, created by [[GpgSigner]] */
class TempFilesTests extends ScalaPublishTestSuite {

  private val tmpDirs = FunFixture[NioPath](
    setup = _ => Files.createTempDirectory("scala-publish-temp-files"),
    teardown = dir =>
      if Files.exists(dir) then
        Files.walk(dir)
          .sorted(java.util.Comparator.reverseOrder())
          .forEach(p => Files.deleteIfExists(p))
  )

  tmpDirs.test(
    "on POSIX file systems, private temporary files are only accessible by their owner"
  ) {
    dir =>
      assume(!Properties.isWin, "POSIX only")
      val f = TempFiles.createPrivate(dir, "signer", ".asc")
      assert(Files.isRegularFile(f))
      assertEquals(f.getParent, dir)
      assertEquals(
        Files.getPosixFilePermissions(f).asScala.toSet,
        Set(PosixFilePermission.OWNER_READ, PosixFilePermission.OWNER_WRITE)
      )
  }

  // like Windows file systems, a zip file system doesn't support POSIX attributes by default
  tmpDirs.test("private temporary files can be created on non-POSIX file systems") { dir =>
    val zip = dir.resolve("fs.zip")
    val fs  = FileSystems.newFileSystem(
      URI.create("jar:" + zip.toUri),
      Map("create" -> "true").asJava
    )
    try {
      assert(!fs.supportedFileAttributeViews().contains("posix"))
      val f = TempFiles.createPrivate(fs.getPath("/"), "signer", ".asc")
      assert(Files.isRegularFile(f))
    }
    finally fs.close()
  }

  test("POSIX initial attributes are only used on file systems supporting them") {
    assert(TempFiles.privateAttributes(FileSystems.getDefault).nonEmpty || Properties.isWin)
    val zip = Files.createTempFile("scala-publish-temp-files", ".zip")
    Files.delete(zip)
    val fs = FileSystems.newFileSystem(
      URI.create("jar:" + zip.toUri),
      Map("create" -> "true").asJava
    )
    try {
      assert(!fs.supportedFileAttributeViews().contains("posix"))
      // passing POSIX attributes there fails on e.g. Windows file systems
      assertEquals(TempFiles.privateAttributes(fs), Nil)
    }
    finally {
      fs.close()
      Files.deleteIfExists(zip)
    }
  }
}
