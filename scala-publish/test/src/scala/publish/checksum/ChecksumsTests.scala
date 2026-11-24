package scala.publish.checksum

import java.nio.charset.StandardCharsets
import java.time.Instant
import java.util.concurrent.Executors

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService}
import scala.publish.checksum.logger.ChecksumLogger
import scala.publish.fileset.{FileSet, Path}
import scala.publish.{Content, ScalaPublishTestSuite}

/** [[Checksums]], as called by Scala CLI (`PublishUtils.computeChecksums` there) */
class ChecksumsTests extends ScalaPublishTestSuite {

  private var ec: ExecutionContextExecutorService = scala.compiletime.uninitialized

  override def beforeAll(): Unit =
    ec = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(2))
  override def afterAll(): Unit =
    ec.shutdownNow()

  private val now   = Instant.EPOCH
  private val types = Seq(ChecksumType.MD5, ChecksumType.SHA1)
  private val dir   = Seq("org", "example", "lib_3", "1.0.0")

  private def fileSet(files: (String, String)*): FileSet =
    FileSet(files.map {
      case (n, c) =>
        Path(dir :+ n) -> Content.InMemory(now, c.getBytes(StandardCharsets.UTF_8))
    })

  private def contents(fs: FileSet): Map[String, String] =
    fs.elements.map {
      case (p, c) =>
        p.elements.last -> new String(c.content(), StandardCharsets.UTF_8)
    }.toMap

  private def compute(fs: FileSet, logger: ChecksumLogger = new ChecksumLogger {}): FileSet =
    Checksums(types, fs, now, ec, logger).unsafeRun()(using ec)

  test("computes the missing checksums of every file, and only those") {
    val res = compute(fileSet("a.jar" -> "abc", "a.pom" -> "<project/>"))
    assertEquals(
      contents(res),
      Map(
        "a.jar.md5"  -> "900150983cd24fb0d6963f7d28e17f72",
        "a.jar.sha1" -> "a9993e364706816aba3e25717850c26c9cd0d89d",
        "a.pom.md5"  -> Checksum.compute(ChecksumType.MD5, "<project/>".getBytes).repr,
        "a.pom.sha1" -> Checksum.compute(ChecksumType.SHA1, "<project/>".getBytes).repr
      )
    )
    assert(res.elements.forall(_._1.elements.init == dir))
  }

  test("existing checksums are kept, not recomputed") {
    val res = compute(fileSet("a.jar" -> "abc", "a.jar.sha1" -> "existing"))
    assertEquals(contents(res).keySet, Set("a.jar.md5"))
  }

  test("signatures get checksums too (Scala CLI filters them out beforehand)") {
    val res = compute(fileSet("a.jar" -> "abc", "a.jar.asc" -> "sig"))
    assertEquals(
      contents(res).keySet,
      Set("a.jar.md5", "a.jar.sha1", "a.jar.asc.md5", "a.jar.asc.sha1")
    )
  }

  test("Checksums.clear removes checksum files only") {
    val fs = fileSet(
      "a.jar"      -> "",
      "a.jar.md5"  -> "",
      "a.jar.sha1" -> "",
      "a.jar.asc"  -> "",
      "a.pom"      -> ""
    )
    assertEquals(
      Checksums.clear(types, fs).elements.map(_._1.elements.last),
      Seq("a.jar", "a.jar.asc", "a.pom")
    )
    assertEquals(
      Checksums.clear(Seq(ChecksumType.MD5), fs).elements.map(_._1.elements.last),
      Seq("a.jar", "a.jar.sha1", "a.jar.asc", "a.pom")
    )
  }

  test("the logger is started, told about every checksum, and stopped") {
    val events = mutable.ArrayBuffer.empty[String]
    val logger = new ChecksumLogger {
      override def start(): Unit             = events.synchronized(events += "start")
      override def stop(keep: Boolean): Unit = events.synchronized(events += "stop")
      override def computingSet(id: Object, fs: FileSet): Unit =
        events.synchronized(events += s"computingSet(${fs.elements.length})")
      override def computedSet(id: Object, fs: FileSet): Unit =
        events.synchronized(events += s"computedSet(${fs.elements.length})")
      override def computed(
        id: Object,
        type0: ChecksumType,
        path: String,
        errorOpt: Option[Throwable]
      ): Unit =
        events.synchronized(events += s"computed($path)")
    }
    compute(fileSet("a.jar" -> "abc"), logger)
    assertEquals(events.head, "start")
    assertEquals(events(1), "computingSet(2)")
    assertEquals(
      events.slice(2, 4).sorted.toList,
      List(
        "computed(org/example/lib_3/1.0.0/a.jar.md5)",
        "computed(org/example/lib_3/1.0.0/a.jar.sha1)"
      )
    )
    assertEquals(events.drop(4).toList, List("computedSet(2)", "stop"))
  }

  test("SHA-256 and SHA-512 checksum files") {
    val res = Checksums(
      Seq(ChecksumType.SHA256, ChecksumType.SHA512),
      fileSet("a.jar" -> "abc"),
      now,
      ec,
      new ChecksumLogger {}
    ).unsafeRun()(using ec)
    assertEquals(
      contents(res),
      Map(
        "a.jar.sha256" -> "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad",
        "a.jar.sha512" ->
          "ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f"
      )
    )
  }
}
