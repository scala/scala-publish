package scala.publish.signing

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path as NioPath}
import java.time.Instant

import scala.jdk.CollectionConverters.*
import scala.publish.{Content, ScalaPublishTestSuite}
import scala.util.Properties

/** [[GpgSigner]] against a fake `gpg` script recording its arguments (POSIX only) */
class GpgSignerTests extends ScalaPublishTestSuite {

  private val tmpDirs = FunFixture[NioPath](
    setup = _ => {
      assume(!Properties.isWin, "The fake gpg is a POSIX shell script")
      Files.createTempDirectory("scala-publish-gpg")
    },
    teardown = dir =>
      if Files.exists(dir) then
        Files.walk(dir)
          .sorted(java.util.Comparator.reverseOrder())
          .forEach(p => Files.deleteIfExists(p))
  )

  /** Writes a fake gpg, recording its arguments in `args.txt` and writing a fake signature */
  private def fakeGpg(dir: NioPath, exitCode: Int = 0): NioPath = {
    val script = dir.resolve("fake-gpg")
    val args   = dir.resolve("args.txt")
    Files.write(
      script,
      s"""#!/bin/sh
         |for a in "$$@"; do echo "$$a" >> "$args"; done
         |out=""
         |while [ $$# -gt 0 ]; do
         |  if [ "$$1" = "--output" ]; then out="$$2"; fi
         |  shift
         |done
         |echo "fake signature" > "$$out"
         |exit $exitCode
         |""".stripMargin.getBytes(StandardCharsets.UTF_8)
    )
    script.toFile.setExecutable(true)
    script
  }

  private def recordedArgs(dir: NioPath): Seq[String] =
    Files.readAllLines(dir.resolve("args.txt")).asScala.toSeq

  tmpDirs.test("signs files with the expected gpg arguments") { dir =>
    val file = dir.resolve("lib.jar")
    Files.write(file, Array[Byte](1, 2, 3))
    val signer = GpgSigner(
      GpgSigner.Key.Id("ABCDEF"),
      command = fakeGpg(dir).toString,
      extraOptions = Seq("--pinentry-mode", "loopback")
    )
    assertEquals(signer.sign(Content.File(file)), Right("fake signature\n"))
    val args = recordedArgs(dir)
    assertEquals(
      args.take(7),
      Seq("--pinentry-mode", "loopback", "--local-user", "ABCDEF", "--armor", "--yes", "--output")
    )
    assertEquals(args.drop(8), Seq("--detach-sign", file.toAbsolutePath.toString))
  }

  tmpDirs.test("the default key passes no --local-user") { dir =>
    val file = dir.resolve("lib.jar")
    Files.write(file, Array[Byte](1))
    val signer = GpgSigner(GpgSigner.Key.Default, command = fakeGpg(dir).toString)
    assert(signer.sign(Content.File(file)).isRight)
    assert(!recordedArgs(dir).contains("--local-user"))
  }

  tmpDirs.test("in-memory content is signed through a temporary file, removed afterwards") {
    dir =>
      val signer = GpgSigner(GpgSigner.Key.Default, command = fakeGpg(dir).toString)
      assertEquals(
        signer.sign(Content.InMemory(Instant.EPOCH, Array[Byte](1))),
        Right("fake signature\n")
      )
      val signedFile = NioPath.of(recordedArgs(dir).last)
      assert(!Files.exists(signedFile), clue = signedFile)
  }

  tmpDirs.test("a failing gpg is reported with its exit code") { dir =>
    val file = dir.resolve("lib.jar")
    Files.write(file, Array[Byte](1))
    val signer = GpgSigner(GpgSigner.Key.Default, command = fakeGpg(dir, exitCode = 2).toString)
    assertEquals(signer.sign(Content.File(file)), Left("gpg failed (return code: 2)"))
  }
}
