package scala.publish.checksum

import java.io.InputStream
import java.math.BigInteger
import java.security.MessageDigest

/** A… checksum
  */
final case class Checksum(`type`: ChecksumType, value: BigInteger) {
  require(`type`.validValue(value), s"Invalid ${`type`.name} checksum value: $value")
  def repr: String =
    String.format(s"%0${`type`.size}x", value)
}

object Checksum {

  def compute(`type`: ChecksumType, content: Array[Byte]): Checksum = {

    val md = MessageDigest.getInstance(`type`.name)
    md.update(content)
    val digest        = md.digest()
    val calculatedSum = new BigInteger(1, digest)

    Checksum(`type`, calculatedSum)
  }

  /** Computes the checksum of the content read from `content`, without closing it */
  def compute(`type`: ChecksumType, content: InputStream): Checksum = {
    val md  = MessageDigest.getInstance(`type`.name)
    val buf = new Array[Byte](64 * 1024)
    var n   = content.read(buf)
    while n >= 0 do {
      md.update(buf, 0, n)
      n = content.read(buf)
    }
    Checksum(`type`, new BigInteger(1, md.digest()))
  }

}
