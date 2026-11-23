package scala.publish.checksum

import scala.publish.ScalaPublishTestSuite

class ChecksumTypeTests extends ScalaPublishTestSuite {

  // Scala CLI lists these in `publish --checksum list` (name) and uses the extensions when
  // building file names
  test("available checksum types, names and extensions") {
    assertEquals(
      ChecksumType.all.map(t => (t, t.name, t.extension)),
      Seq((ChecksumType.SHA1, "sha-1", "sha1"), (ChecksumType.MD5, "md5", "md5"))
    )
  }

  // Scala CLI forwards the values of `--checksum` to ChecksumType.parse
  for (
    (input, expected) <- Seq(
      "sha-1" -> ChecksumType.SHA1,
      "SHA-1" -> ChecksumType.SHA1,
      "sha1"  -> ChecksumType.SHA1,
      "SHA1"  -> ChecksumType.SHA1,
      "md5"   -> ChecksumType.MD5,
      "MD5"   -> ChecksumType.MD5
    )
  )
    test(s"parse accepts '$input'") {
      assertEquals(ChecksumType.parse(input), Right(expected))
    }

  for (input <- Seq("sha256", "sha-256", "", "foo"))
    test(s"parse rejects '$input'") {
      assertEquals(ChecksumType.parse(input), Left(s"Unrecognized checksum: $input"))
    }
}
