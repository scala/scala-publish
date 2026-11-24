package scala.publish.checksum

import java.math.BigInteger
import java.util.Locale

/** A type of checksum.
  *
  * @param name
  * @param extension:
  *   extension of this checksum type, without a prefix `"."`
  * @param size:
  *   size of hexadecimal string representation - size in bits is this one times 4
  */
sealed abstract class ChecksumType(
  val name: String,
  val extension: String,
  val size: Int
) extends Product with Serializable {
  private val firstInvalidValue              = BigInteger.valueOf(16L).pow(size)
  def validValue(value: BigInteger): Boolean =
    value.compareTo(BigInteger.ZERO) >= 0 &&
    value.compareTo(firstInvalidValue) < 0
}

object ChecksumType {
  case object SHA1   extends ChecksumType("sha-1", "sha1", 40)
  case object MD5    extends ChecksumType("md5", "md5", 32)
  case object SHA256 extends ChecksumType("sha-256", "sha256", 64)
  case object SHA512 extends ChecksumType("sha-512", "sha512", 128)

  val all: Seq[ChecksumType]         = Seq(SHA1, MD5, SHA256, SHA512)
  val map: Map[String, ChecksumType] = all.map(c => c.name -> c).toMap

  /** Parses a checksum type from its name (e.g. `sha-1`) or its extension (e.g. `sha1`), ignoring
    * case
    */
  def parse(s: String): Either[String, ChecksumType] = {
    val lowerCase = s.toLowerCase(Locale.ROOT)
    all
      .find(t => t.name == lowerCase || t.extension == lowerCase)
      .toRight(s"Unrecognized checksum: $s")
  }
}
