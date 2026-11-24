package scala.publish

import java.nio.file.Files
import java.time.Instant

class ContentTests extends ScalaPublishTestSuite {

  test("file content can be streamed, and its size known without reading it") {
    val f = Files.createTempFile("scala-publish-content", ".jar")
    try {
      Files.write(f, Array[Byte](1, 2, 3, 4))
      val c = Content.File(f)
      assertEquals(c.size(), 4L)
      val in = c.inputStream()
      try assertEquals(in.readAllBytes().toSeq, Seq[Byte](1, 2, 3, 4))
      finally in.close()
    }
    finally Files.deleteIfExists(f)
  }

  test("in-memory content can be streamed, and its size known") {
    val c = Content.InMemory(Instant.EPOCH, Array[Byte](5, 6))
    assertEquals(c.size(), 2L)
    val in = c.inputStream()
    try assertEquals(in.readAllBytes().toSeq, Seq[Byte](5, 6))
    finally in.close()
  }
}
