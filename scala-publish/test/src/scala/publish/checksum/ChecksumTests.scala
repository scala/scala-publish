package scala.publish.checksum

import java.math.BigInteger
import java.nio.charset.StandardCharsets

import scala.publish.ScalaPublishTestSuite

class ChecksumTests extends ScalaPublishTestSuite {

  private val abc = "abc".getBytes(StandardCharsets.UTF_8)

  test("MD5 of a known value") {
    assertEquals(
      Checksum.compute(ChecksumType.MD5, abc).repr,
      "900150983cd24fb0d6963f7d28e17f72"
    )
  }

  test("SHA-1 of a known value") {
    assertEquals(
      Checksum.compute(ChecksumType.SHA1, abc).repr,
      "a9993e364706816aba3e25717850c26c9cd0d89d"
    )
  }

  test("repr is zero-padded to the full checksum size") {
    assertEquals(Checksum(ChecksumType.MD5, BigInteger.ONE).repr, "0" * 31 + "1")
    assertEquals(Checksum(ChecksumType.SHA1, BigInteger.ZERO).repr, "0" * 40)
  }

  test("values out of range for the checksum type are rejected") {
    intercept[IllegalArgumentException](Checksum(ChecksumType.MD5, BigInteger.valueOf(-1L)))
    intercept[IllegalArgumentException](
      Checksum(ChecksumType.MD5, BigInteger.ONE.shiftLeft(128))
    )
  }
}
