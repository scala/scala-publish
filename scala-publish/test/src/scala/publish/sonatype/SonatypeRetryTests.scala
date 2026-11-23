package scala.publish.sonatype

import sttp.client3.testing.SttpBackendStub
import sttp.client3.{Request, Response, SttpClientException}
import sttp.model.{Method, StatusCode}

import java.io.FileNotFoundException
import java.nio.charset.StandardCharsets
import java.util.concurrent.{Executors, ScheduledExecutorService}

import scala.collection.mutable
import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService}
import scala.publish.util.EmaRetryParams
import scala.publish.{Hooks, PublishRepository, ScalaPublishTestSuite}

/** Retries of the requests sent by [[SonatypeApi]] */
class SonatypeRetryTests extends ScalaPublishTestSuite {

  private var es: ScheduledExecutorService        = scala.compiletime.uninitialized
  private var ec: ExecutionContextExecutorService = scala.compiletime.uninitialized

  override def beforeAll(): Unit = {
    es = Executors.newSingleThreadScheduledExecutor()
    ec = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(2))
  }
  override def afterAll(): Unit = {
    es.shutdownNow()
    ec.shutdownNow()
  }

  private val base    = "https://staging.example.test/service/local"
  private val profile = SonatypeApi.Profile("p1", "org.example", s"$base/staging/profiles/p1")

  private val profilesJson =
    """{"data":[{"id":"p1","name":"org.example","resourceURI":"x"}]}"""
  private val reposJson =
    """{"data":[{"profileId":"p1","profileName":"org.example","repositoryId":"r1","type":"closed"}]}"""

  private type Outcome = Either[Request[?, ?] => Exception, (Int, String)]
  private def ok(body: String = ""): Outcome = Right((200, body))
  private def status(code: Int): Outcome     = Right((code, s"error $code"))
  private val connectFailure: Outcome        =
    Left(r => new SttpClientException.ConnectException(r, new java.net.ConnectException("refused")))
  private val timeout: Outcome =
    Left(r =>
      new SttpClientException.TimeoutException(r, new java.net.SocketTimeoutException("timeout"))
    )

  /** Answers requests with `outcomes` in turn (then the last one), recording them */
  private final class Server(outcomes: Outcome*) {
    val requests = mutable.ArrayBuffer.empty[(Method, String)]
    val backend  = SttpBackendStub.synchronous.whenAnyRequest.thenRespondF { r =>
      val n = requests.synchronized {
        requests += r.method -> r.uri.path.mkString("/", "/", "")
        requests.length
      }
      outcomes(math.min(n, outcomes.length) - 1) match {
        case Left(e)             => throw e(r)
        case Right((code, body)) =>
          Response(body.getBytes(StandardCharsets.UTF_8), StatusCode(code))
      }
    }
  }

  private val requestRetries =
    EmaRetryParams(attempts = 3, initialWaitDurationMs = 10L, factor = 2.0f)
  private val stagingRetries =
    EmaRetryParams(attempts = 3, initialWaitDurationMs = 100L, factor = 3.0f)

  private def api(server: Server, sleeps: mutable.ArrayBuffer[Long]): SonatypeApi =
    SonatypeApi(
      server.backend,
      base,
      None,
      verbosity = -1,
      stagingRepoRetryParams = stagingRetries,
      requestRetryParams = Some(requestRetries),
      sleep = ms => sleeps.synchronized(sleeps += ms)
    )

  private def run[T](task: coursier.util.Task[T]): T = task.unsafeRun()(using ec)

  // GET requests: transient failures (HTTP 5xx, connection failures, timeouts) are retried

  for (
    (description, failure) <- Seq(
      "HTTP 503"            -> status(503),
      "connection failures" -> connectFailure,
      "timeouts"            -> timeout
    )
  )
    test(s"GET requests are retried on $description") {
      val server = new Server(failure, failure, ok(profilesJson))
      val sleeps = mutable.ArrayBuffer.empty[Long]
      assertEquals(run(api(server, sleeps).listProfiles()), Seq(profile.copy(uri = "x")))
      assertEquals(server.requests.length, 3)
      assertEquals(sleeps.toList, List(10L, 20L))
    }

  test("GET requests aren't retried on client errors") {
    val server = new Server(status(401))
    val sleeps = mutable.ArrayBuffer.empty[Long]
    intercept[FileNotFoundException](run(api(server, sleeps).listProfiles()))
    assertEquals(server.requests.length, 1)
    assertEquals(sleeps.toList, Nil)
  }

  test("GET requests give up after the maximum number of attempts, keeping the error message") {
    val server = new Server(status(500))
    val sleeps = mutable.ArrayBuffer.empty[Long]
    val e      = intercept[Exception](api(server, sleeps).listProfileRepositories(Some("p1")))
    assertEquals(
      e.getMessage,
      s"Failed to get $base/staging/profile_repositories/p1 (http status: 500, response: error 500)"
    )
    assertEquals(server.requests.length, 3)
  }

  test("the number of attempts and waits of GET requests are configurable") {
    val server = new Server(status(502))
    val sleeps = mutable.ArrayBuffer.empty[Long]
    val api0   =
      api(server, sleeps).copy(requestRetryParams = Some(EmaRetryParams(5, 1000L, 1.5f)))
    intercept[Exception](api0.listProfileRepositories(Some("p1")))
    assertEquals(server.requests.length, 5)
    assertEquals(sleeps.toList, List(1000L, 1500L, 2250L, 3375L))
  }

  test("without requestRetryParams, retryOnTimeout gives the number of attempts") {
    val server = new Server(status(502))
    val sleeps = mutable.ArrayBuffer.empty[Long]
    val api0   = api(server, sleeps).copy(requestRetryParams = None, retryOnTimeout = 2)
    intercept[Exception](api0.listProfileRepositories(Some("p1")))
    assertEquals(server.requests.length, 2)
    assertEquals(sleeps.toList, List(1000L))
  }

  test("waiting for a repository status retries transient failures when listing repositories") {
    val server = new Server(status(503), ok(reposJson))
    val sleeps = mutable.ArrayBuffer.empty[Long]
    run(api(server, sleeps).waitForStatus("p1", "r1", "closed", None, 1, 1.millis, 1.0, es))
    assertEquals(server.requests.length, 2)
  }

  // POST requests: retried on HTTP 5xx and connection failures, not on timeouts (the request may
  // have been processed)

  test("staging repository actions are retried on HTTP 5xx and connection failures") {
    val server = new Server(status(503), connectFailure, ok())
    val sleeps = mutable.ArrayBuffer.empty[Long]
    api(server, sleeps).sendCloseStagingRepositoryRequest(profile, "r1", "closing")
    assertEquals(server.requests.length, 3)
    assertEquals(sleeps.toList, List(100L, 300L))
  }

  test("staging repository actions aren't retried on client errors") {
    val server = new Server(status(403))
    val sleeps = mutable.ArrayBuffer.empty[Long]
    val e      = intercept[Exception] {
      api(server, sleeps).sendCloseStagingRepositoryRequest(profile, "r1", "closing")
    }
    assertEquals(
      e.getMessage,
      s"Failed to get $base/staging/profiles/p1/finish (http status: 403, response: error 403)"
    )
    assertEquals(server.requests.length, 1)
    assertEquals(sleeps.toList, Nil)
  }

  test("staging repository actions aren't retried on timeouts") {
    val server = new Server(timeout, ok())
    val sleeps = mutable.ArrayBuffer.empty[Long]
    intercept[SttpClientException.TimeoutException] {
      api(server, sleeps).sendCloseStagingRepositoryRequest(profile, "r1", "closing")
    }
    assertEquals(server.requests.length, 1)
  }

  test("creating a staging repository is retried on HTTP 5xx, not on timeouts") {
    val created = """{"data":{"stagedRepositoryId":"r1"}}"""
    val server  = new Server(status(502), ok(created))
    val sleeps  = mutable.ArrayBuffer.empty[Long]
    assertEquals(api(server, sleeps).createStagingRepository(profile, "start"), "r1")
    assertEquals(server.requests.length, 2)

    val server0 = new Server(timeout, ok(created))
    intercept[SttpClientException.TimeoutException](
      api(server0, sleeps).createStagingRepository(profile, "start")
    )
    assertEquals(server0.requests.length, 1)
  }

  test("promoting is retried on HTTP 5xx") {
    val server = new Server(status(500), ok())
    val sleeps = mutable.ArrayBuffer.empty[Long]
    api(server, sleeps).promoteStagingRepository("r1", "promote")
    assertEquals(
      server.requests.map(_._2).toList,
      List.fill(2)(s"/service/local/staging/bulk/promote")
    )
  }

  test("Hooks.Sonatype: the status polling attempts and waits are configurable") {
    val polls   = new java.util.concurrent.atomic.AtomicInteger
    val backend = SttpBackendStub.synchronous.whenAnyRequest.thenRespondF { r =>
      val path = r.uri.path.mkString("/", "/", "")
      val body =
        if path.endsWith("/profile_repositories/p1") then {
          polls.incrementAndGet()
          reposJson.replace("closed", "open")
        }
        else if path.endsWith("/activity") then "[]"
        else ""
      Response(body.getBytes(StandardCharsets.UTF_8), StatusCode.Ok)
    }
    val api0  = SonatypeApi(backend, base, None, verbosity = -1, sleep = _ => ())
    val hooks = Hooks.Sonatype(
      PublishRepository.Sonatype(),
      api0,
      new java.io.PrintStream(new java.io.ByteArrayOutputStream),
      verbosity = -1,
      batch = true,
      es = es,
      statusPollParams = EmaRetryParams(2, 1L, 1.0f)
    )
    val e = intercept[Exception](run(hooks.afterUpload(Some((profile, "r1")))))
    assert(e.getMessage.startsWith("Repository r1 in state open after"), clue = e.getMessage)
    assertEquals(polls.get, 2)
  }

  test("Hooks.sonatype accepts status polling parameters") {
    val server = new Server(ok())
    val hooks  = Hooks.sonatype(
      repo = PublishRepository.Sonatype(),
      api = api(server, mutable.ArrayBuffer.empty),
      out = new java.io.PrintStream(new java.io.ByteArrayOutputStream),
      verbosity = -1,
      batch = true,
      es = es,
      statusPollParams = EmaRetryParams(7, 10L, 2.0f)
    )
    hooks match {
      case s: Hooks.Sonatype => assertEquals(s.statusPollParams, EmaRetryParams(7, 10L, 2.0f))
      case other             => fail(s"Unexpected hooks: $other")
    }
  }
}
