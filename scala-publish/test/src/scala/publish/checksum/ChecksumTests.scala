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

  test("checksums can be computed from a stream") {
    def stream = new java.io.ByteArrayInputStream(abc)
    assertEquals(
      Checksum.compute(ChecksumType.MD5, stream).repr,
      "900150983cd24fb0d6963f7d28e17f72"
    )
    assertEquals(
      Checksum.compute(ChecksumType.SHA1, stream).repr,
      "a9993e364706816aba3e25717850c26c9cd0d89d"
    )
  }

  test("SHA-256 and SHA-512 of a known value") {
    assertEquals(
      Checksum.compute(ChecksumType.SHA256, abc).repr,
      "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
    )
    assertEquals(
      Checksum.compute(ChecksumType.SHA512, abc).repr,
      "ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f"
    )
    assertEquals(Checksum(ChecksumType.SHA256, BigInteger.ONE).repr, "0" * 63 + "1")
    assertEquals(Checksum(ChecksumType.SHA512, BigInteger.ONE).repr, "0" * 127 + "1")
  }
}
