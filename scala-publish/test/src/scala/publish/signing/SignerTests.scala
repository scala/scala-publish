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

  /** Like Scala CLI's `BouncycastleExternalSigner`, which matches on both kinds of content */
  private object FailingSigner extends Signer {
    val signed = scala.collection.mutable.ArrayBuffer.empty[String]
    def sign(content: Content): Either[String, String] = {
      val name = content match {
        case Content.File(path)         => path.getFileName.toString
        case Content.InMemory(_, bytes) => new String(bytes, StandardCharsets.UTF_8)
      }
      signed += name
      if name.endsWith(".pom") then Left(s"cannot sign $name") else Right("sig")
    }
  }

  test("signing stops at the first failure, reporting its path, content and message") {
    val res = FailingSigner.signatures(
      fileSet("a.jar", "b.pom", "c.jar"),
      now,
      Set.empty,
      Set.empty,
      SignerLogger.nop
    )
    res match {
      case Left((path, content, msg)) =>
        assertEquals(path, Path(dir :+ "b.pom"))
        assertEquals(new String(content.content(), StandardCharsets.UTF_8), "b.pom")
        assertEquals(msg, "cannot sign b.pom")
      case Right(fs) => fail(s"Expected a failure, got $fs")
    }
    assertEquals(FailingSigner.signed.toList, List("a.jar", "b.pom"))
  }

  test("the logger is started and stopped around signing, even when the signer throws") {
    val events = scala.collection.mutable.ArrayBuffer.empty[String]
    val logger = new SignerLogger {
      override def start(): Unit                          = events += "start"
      override def stop(keep: Boolean): Unit              = events += "stop"
      override def signing(id: Object, fs: FileSet): Unit = events +=
        s"signing(${fs.elements.length})"
      override def signed(id: Object, fs: FileSet): Unit = events +=
        s"signed(${fs.elements.length})"
      override def signingElement(id: Object, path: Path): Unit =
        events += s"signingElement(${path.elements.last})"
      override def signedElement(id: Object, path: Path, excOpt: Option[Throwable]): Unit =
        events += s"signedElement(${path.elements.last}, ${excOpt.map(_.getMessage)})"
    }
    val throwing = new Signer {
      def sign(content: Content): Either[String, String] = throw new RuntimeException("boom")
    }
    intercept[RuntimeException](throwing.signatures(
      fileSet("a.jar"),
      now,
      Set.empty,
      Set.empty,
      logger
    ))
    assertEquals(
      events.toList,
      List(
        "start",
        "signing(1)",
        "signingElement(a.jar)",
        "signedElement(a.jar, Some(boom))",
        "signed(1)",
        "stop"
      )
    )
  }

  test("NopSigner adds no signatures") {
    assertEquals(
      NopSigner.signatures(fileSet("a.jar"), now, Set.empty, Set.empty, SignerLogger.nop),
      Right(FileSet.empty)
    )
  }
}
