package scala.publish.dir

import java.nio.file.{Files, Path as NioPath}

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Promise, TimeoutException}
import scala.publish.ScalaPublishTestSuite
import scala.publish.dir.logger.DirLogger
import scala.util.Try
import scala.util.control.NonFatal

class DirTests extends ScalaPublishTestSuite {

  private val tmpDirs = FunFixture[NioPath](
    setup = _ => Files.createTempDirectory("scala-publish-dir").toRealPath(),
    teardown = dir =>
      if Files.exists(dir) then
        Files.walk(dir)
          .sorted(java.util.Comparator.reverseOrder())
          .forEach(p => Files.deleteIfExists(p))
  )

  private def write(base: NioPath, rel: String): Unit = {
    val p = base.resolve(rel)
    Files.createDirectories(p.getParent)
    Files.write(p, rel.getBytes)
  }

  /** Creates a symbolic link, skipping the test if that's not allowed (e.g. on Windows) */
  private def symlink(link: NioPath, target: NioPath): Unit =
    try Files.createSymbolicLink(link, target)
    catch {
      case NonFatal(e) =>
        assume(false, s"Cannot create symbolic links here: $e")
    }

  /** Runs `f` on a daemon thread, failing rather than hanging if it doesn't terminate */
  private def bounded[T](f: => T): T = {
    val result         = Promise[T]()
    val task: Runnable = () => result.complete(Try(f))
    val thread         = new Thread(task, "dir-test")
    thread.setDaemon(true)
    thread.start()
    try Await.result(result.future, 10.seconds)
    catch {
      case _: TimeoutException =>
        thread.interrupt()
        fail("Walking the directory didn't terminate")
    }
  }

  private def listed(base: NioPath): Seq[String] =
    bounded(Dir.fileSet(base, new DirLogger {}).elements.map(_._1.repr).sorted)

  tmpDirs.test("lists regular files recursively, with paths relative to the directory") {
    base =>
      write(base, "a.txt")
      write(base, "sub/b.txt")
      write(base, "sub/deeper/c.txt")
      assertEquals(listed(base), Seq("a.txt", "sub/b.txt", "sub/deeper/c.txt"))
  }

  tmpDirs.test("follows symbolic links to directories outside of the current path") { base =>
    write(base, "sub/b.txt")
    symlink(base.resolve("link"), base.resolve("sub"))
    assertEquals(listed(base), Seq("link/b.txt", "sub/b.txt"))
  }

  tmpDirs.test("doesn't loop on symbolic links pointing to a parent directory") { base =>
    write(base, "a.txt")
    write(base, "sub/b.txt")
    symlink(base.resolve("sub/loop"), base)
    symlink(base.resolve("self"), base.resolve("."))
    assertEquals(listed(base), Seq("a.txt", "sub/b.txt"))
  }

  tmpDirs.test("isRepository doesn't loop on symbolic links pointing to a parent directory") {
    base =>
      write(base, "org/example/lib/1.0/lib-1.0.pom")
      symlink(base.resolve("org/example/loop"), base)
      symlink(base.resolve("org/self"), base.resolve("org/."))
      assert(bounded(Dir.isRepository(base)))
  }
}
