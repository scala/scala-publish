package scala.publish.fileset

import coursier.core.{ModuleName, Organization}

import java.time.Instant

import scala.publish.dir.DirContent
import scala.publish.{Content, ScalaPublishTestSuite}
import scala.xml.XML

class GroupTests extends ScalaPublishTestSuite {

  private val now = Instant.parse("2024-01-02T03:04:05Z")

  private def module(version: String): Group.Module = {
    val names = Seq(s"lib-$version.pom", s"lib-$version.jar", s"lib-$version-sources.jar")
    Group.Module(
      Organization("org.example"),
      ModuleName("lib"),
      version,
      None,
      DirContent(names.map(n => n -> Content.InMemory(now, n.getBytes)))
    )
  }

  private def snapshotValues(m: Group.Module): Seq[String] =
    m.mavenMetadataContentOpt.toSeq.flatMap { c =>
      (XML.loadString(new String(c.content())) \\ "snapshotVersion" \ "value").map(_.text)
    }

  for (
    (version, expected) <- Seq(
      "1.0-SNAPSHOT" -> "1.0-20240102.030405-1",
      "1.0.SNAPSHOT" -> "1.0-20240102.030405-1",
      // no separator: same as Maven, which replaces "SNAPSHOT" with "<timestamp>-<build number>"
      "1.0SNAPSHOT" -> "1.020240102.030405-1"
    )
  )
    test(s"addSnapshotVersioning for $version") {
      val m = module(version).addSnapshotVersioning(now, Set.empty)
      assertEquals(m.snapshotVersioning, Some(expected))
      assertEquals(
        m.files.elements.map(_._1).sorted,
        Seq(
          s"lib-$expected-sources.jar",
          s"lib-$expected.jar",
          s"lib-$expected.pom",
          "maven-metadata.xml"
        )
      )
      assertEquals(snapshotValues(m), Seq(expected, expected, expected))
    }

  test("addSnapshotVersioning increments the build number of existing metadata") {
    val first  = module("1.0SNAPSHOT").addSnapshotVersioning(now, Set.empty)
    val second = module("1.0SNAPSHOT")
      .copy(files =
        module("1.0SNAPSHOT").files
          .update("maven-metadata.xml", first.mavenMetadataContentOpt.get)
      )
      .addSnapshotVersioning(now.plusSeconds(60), Set.empty)
    assertEquals(second.snapshotVersioning, Some("1.020240102.030505-2"))
  }

  test("addSnapshotVersioning lists artifacts when existing metadata has no snapshotVersions") {
    val existing =
      """<metadata><groupId>org.example</groupId><artifactId>lib</artifactId><version>1.0-SNAPSHOT</version><versioning><snapshot><timestamp>20240101.000000</timestamp><buildNumber>4</buildNumber></snapshot><lastUpdated>20240101000000</lastUpdated></versioning></metadata>"""
    val m       = module("1.0-SNAPSHOT")
    val updated = m
      .copy(files = m.files.update("maven-metadata.xml", Content.InMemory(now, existing.getBytes)))
      .addSnapshotVersioning(now, Set.empty)
    assertEquals(updated.snapshotVersioning, Some("1.0-20240102.030405-5"))
    assertEquals(snapshotValues(updated), Seq.fill(3)("1.0-20240102.030405-5"))
  }
}
