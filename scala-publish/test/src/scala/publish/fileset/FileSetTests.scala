package scala.publish.fileset

import java.time.Instant

import scala.publish.dir.DirContent
import scala.publish.{Content, ScalaPublishTestSuite}

class FileSetTests extends ScalaPublishTestSuite {

  private val now                     = Instant.EPOCH
  private def content(s: String)      = Content.InMemory(now, s.getBytes)
  private val dir                     = Seq("org", "example", "app", "1.0")
  private def path(name: String)      = Path(dir :+ name)
  private def names(fs: FileSet)      = fs.elements.map(_._1.elements.last)
  private def names(dc: DirContent)   = dc.elements.map(_._1)
  private def fileSet(ns: String*)    = FileSet(ns.map(n => path(n) -> content(n)))
  private def dirContent(ns: String*) = DirContent(ns.map(n => n -> content(n)))

  private val initial = Seq(
    "app-1.0.tar",
    "app-1.0.tar.asc",
    "app-1.0.tar.md5",
    "app-1.0.tar.asc.sha1",
    "app-1.0.tar.gz",
    "app-1.0.tar.gz.sha1"
  )

  test("FileSet.update replaces the file and drops its stale signatures and checksums") {
    val updated = fileSet(initial*).update(path("app-1.0.tar"), content("new"))
    assertEquals(names(updated), Seq("app-1.0.tar.gz", "app-1.0.tar.gz.sha1", "app-1.0.tar"))
    assertEquals(
      updated.elements.collectFirst {
        case (p, c) if p == path("app-1.0.tar") => new String(c.content())
      },
      Some("new")
    )
  }

  test("FileSet.removeAll keeps other files sharing the name as a prefix") {
    assertEquals(
      names(fileSet(initial*).removeAll(path("app-1.0.tar"))),
      Seq("app-1.0.tar.gz", "app-1.0.tar.gz.sha1")
    )
  }

  test("FileSet.++ keeps other files sharing a name as a prefix") {
    val merged = fileSet(initial*) ++ fileSet("app-1.0.tar", "app-1.0.tar.asc")
    assertEquals(
      names(merged),
      Seq("app-1.0.tar.gz", "app-1.0.tar.gz.sha1", "app-1.0.tar", "app-1.0.tar.asc")
    )
  }

  test("FileSet.++ only considers files in the same directory") {
    val other  = FileSet(Seq(Path(Seq("elsewhere", "app-1.0.tar")) -> content("x")))
    val merged = fileSet(initial*) ++ other
    assertEquals(merged.elements.length, initial.length + 1)
  }

  test("DirContent.update and removeAll keep other files sharing the name as a prefix") {
    val dc = dirContent(initial*)
    assertEquals(
      names(dc.update("app-1.0.tar", content("new"))),
      Seq("app-1.0.tar.gz", "app-1.0.tar.gz.sha1", "app-1.0.tar")
    )
    assertEquals(
      names(dc.removeAll("app-1.0.tar")),
      Seq("app-1.0.tar.gz", "app-1.0.tar.gz.sha1")
    )
    assertEquals(names(dc.remove("app-1.0.tar")), Seq("app-1.0.tar.gz", "app-1.0.tar.gz.sha1"))
  }

  test("DirContent.remove of maven-metadata.xml drops its checksums") {
    val dc = dirContent(
      "maven-metadata.xml",
      "maven-metadata.xml.md5",
      "maven-metadata.xml.sha1",
      "app-1.0.pom"
    )
    assertEquals(names(dc.remove("maven-metadata.xml")), Seq("app-1.0.pom"))
  }

  test("FileSet.++ scales linearly-ish with the number of files") {
    val n      = 20000
    val big    = FileSet((1 to n).map(i => path(s"f$i.jar") -> content("")))
    val other  = FileSet((1 to n by 2).map(i => path(s"f$i.jar") -> content("new")))
    val start  = System.nanoTime()
    val merged = big ++ other
    val ms     = (System.nanoTime() - start) / 1000000L
    assertEquals(merged.elements.length, n)
    assert(ms < 2000L, clue = s"took $ms ms")
  }
}
