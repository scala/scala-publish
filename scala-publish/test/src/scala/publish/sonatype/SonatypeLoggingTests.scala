package scala.publish.sonatype

import sttp.client3.Response
import sttp.client3.testing.SttpBackendStub
import sttp.model.StatusCode

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import java.util.concurrent.Executors

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.DurationInt
import scala.publish.ScalaPublishTestSuite

/** What [[SonatypeApi]] prints, depending on verbosity */
class SonatypeLoggingTests extends ScalaPublishTestSuite {

  private val base    = "https://staging.example.test/service/local"
  private val profile = SonatypeApi.Profile("p1", "org.example", s"$base/staging/profiles/p1")

  private val backend = SttpBackendStub.synchronous.whenAnyRequest.thenRespondF { r =>
    val path = r.uri.path.mkString("/", "/", "")
    val body =
      if path.endsWith("/start") then """{"data":{"stagedRepositoryId":"r1"}}"""
      else if path.endsWith("/profile_repositories/p1") then
        """{"data":[{"profileId":"p1","profileName":"org.example","repositoryId":"r1","type":"closed"}]}"""
      else ""
    Response(body.getBytes(StandardCharsets.UTF_8), StatusCode.Ok)
  }

  private def api(verbosity: Int) = SonatypeApi(backend, base, None, verbosity = verbosity)

  /** What `f` prints to `Console.err` (runs on the current thread) */
  private def stderrOf(f: => Unit): String = {
    val baos = new ByteArrayOutputStream
    Console.withErr(baos)(f)
    new String(baos.toByteArray, StandardCharsets.UTF_8)
  }

  private def waitForClosed(verbosity: Int): Unit = {
    val es = Executors.newSingleThreadScheduledExecutor()
    try
      api(verbosity)
        .waitForStatus("p1", "r1", "closed", None, 1, 1.millis, 1.0, es)
        // parasitic: runs on the current thread, so that Console.withErr applies
        .unsafeRun()(using ExecutionContext.parasitic)
    finally es.shutdown()
  }

  test("creating a staging repository is reported by default") {
    val err = stderrOf(api(0).createStagingRepository(profile, "start"))
    assert(err.contains("Creating staging repository for profile org.example"), clue = err)
  }

  test("creating a staging repository isn't reported in quiet mode") {
    assertEquals(stderrOf(api(-1).createStagingRepository(profile, "start")), "")
  }

  test("repository statuses are reported by default") {
    val err = stderrOf(waitForClosed(0))
    assert(err.contains("Repository r1 has status closed"), clue = err)
  }

  test("repository statuses aren't reported in quiet mode") {
    assertEquals(stderrOf(waitForClosed(-1)), "")
  }
}
