package scala.publish.signing

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path as NioPath}
import java.time.Instant

import scala.publish.{Content, ScalaPublishTestSuite}
import scala.util.Properties

/** [[GpgSigner]] on Windows, against a fake `gpg` batch script
  *
  * Checks in particular that temporary files can be created there (POSIX permissions aren't
  * supported on Windows file systems).
  */
class WindowsGpgSignerTests extends ScalaPublishTestSuite {

  private val tmpDirs = FunFixture[NioPath](
    setup = _ => {
      assume(Properties.isWin, "Windows only (the fake gpg is a batch script)")
      Files.createTempDirectory("scala-publish-gpg")
    },
    teardown = dir =>
      if Files.exists(dir) then
        Files.walk(dir)
          .sorted(java.util.Comparator.reverseOrder())
          .forEach(p => Files.deleteIfExists(p))
  )

  /** Writes a fake gpg, writing a fake signature to the file passed after --output */
  private def fakeGpg(dir: NioPath, exitCode: Int = 0): NioPath = {
    val script = dir.resolve("fake-gpg.cmd")
    Files.write(
      script,
      s"""@echo off
         |:loop
         |if "%~1"=="" goto done
         |if "%~1"=="--output" set "OUT=%~2"
         |shift
         |goto loop
         |:done
         |echo fake signature> "%OUT%"
         |exit /b $exitCode
         |""".stripMargin.replace("\n", "\r\n").getBytes(StandardCharsets.UTF_8)
    )
    script
  }

  tmpDirs.test("signs files") { dir =>
    val file = dir.resolve("lib.jar")
    Files.write(file, Array[Byte](1, 2, 3))
    val signer = GpgSigner(GpgSigner.Key.Default, command = fakeGpg(dir).toString)
    assertEquals(signer.sign(Content.File(file)).map(_.trim), Right("fake signature"))
  }

  tmpDirs.test("signs in-memory content, through a temporary file") { dir =>
    val signer = GpgSigner(GpgSigner.Key.Default, command = fakeGpg(dir).toString)
    assertEquals(
      signer.sign(Content.InMemory(Instant.EPOCH, Array[Byte](1))).map(_.trim),
      Right("fake signature")
    )
  }

  tmpDirs.test("a failing gpg is reported with its exit code") { dir =>
    val file = dir.resolve("lib.jar")
    Files.write(file, Array[Byte](1))
    val signer = GpgSigner(GpgSigner.Key.Default, command = fakeGpg(dir, exitCode = 2).toString)
    assertEquals(signer.sign(Content.File(file)), Left("gpg failed (return code: 2)"))
  }
}
