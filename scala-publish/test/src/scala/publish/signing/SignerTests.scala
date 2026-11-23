package scala.publish.signing

import java.nio.charset.StandardCharsets
import java.time.Instant

import scala.publish.fileset.{FileSet, Path}
import scala.publish.signing.logger.SignerLogger
import scala.publish.{Content, ScalaPublishTestSuite}

class SignerTests extends ScalaPublishTestSuite {

  /** Signs content by prefixing it with "signed:" */
  private object FakeSigner extends Signer {
    def sign(content: Content): Either[String, String] =
      Right("signed:" + new String(content.content(), StandardCharsets.UTF_8))
  }

  private val now = Instant.EPOCH
  private val dir = Seq("org", "example", "lib_3", "1.0.0")

  private def fileSet(names: String*): FileSet =
    FileSet(names.map(n => Path(dir :+ n) -> Content.InMemory(now, n.getBytes)))

  /** Names of the signature files the signer adds for `names` */
  private def signedNames(
    names: Seq[String],
    dontSignExtensions: Set[String] = Set("md5", "sha1"),
    dontSignFiles: Set[String] = Set("maven-metadata.xml")
  ): Seq[String] =
    FakeSigner
      .signatures(fileSet(names*), now, dontSignExtensions, dontSignFiles, SignerLogger.nop)
      .fold(err => fail(s"Unexpected error: $err"), _.elements.map(_._1.elements.last))

  test("signs files, skipping checksums, metadata and existing signatures") {
    assertEquals(
      signedNames(Seq(
        "lib_3-1.0.0.jar",
        "lib_3-1.0.0.jar.md5",
        "lib_3-1.0.0.pom",
        "lib_3-1.0.0.pom.asc",
        "maven-metadata.xml",
        "maven-metadata.xml.sha1"
      )),
      Seq("lib_3-1.0.0.jar.asc")
    )
  }

  test("checksums of signatures are neither signed nor treated as signatures") {
    assertEquals(
      signedNames(Seq(
        "lib_3-1.0.0.jar",
        "lib_3-1.0.0.jar.asc.md5",
        "lib_3-1.0.0.jar.asc.sha1"
      )),
      Seq("lib_3-1.0.0.jar.asc")
    )
  }

  test("files whose name merely contains '.asc.' are signed") {
    assertEquals(
      signedNames(Seq("lib.asc.utils-2.0.jar", "lib_3-2.0.asc.1.pom")),
      Seq("lib.asc.utils-2.0.jar.asc", "lib_3-2.0.asc.1.pom.asc")
    )
  }

  test("signature content comes from the signer") {
    val res = FakeSigner.signatures(fileSet("a.jar"), now, Set.empty, Set.empty, SignerLogger.nop)
    val sig = res.fold(err => fail(s"Unexpected error: $err"), _.elements.map(_._2))
    assertEquals(sig.map(c => new String(c.content(), StandardCharsets.UTF_8)), Seq("signed:a.jar"))
  }
}
