package scala.publish.sonatype

import coursier.core.Authentication
import coursier.maven.MavenRepository
import coursier.util.Task
import sttp.client3.testing.SttpBackendStub
import sttp.client3.{ByteArrayBody, Request, Response, StringBody}
import sttp.model.{Method, StatusCode}

import java.io.{ByteArrayOutputStream, FileNotFoundException, PrintStream}
import java.nio.charset.StandardCharsets
import java.time.Instant
import java.util.concurrent.{Executors, ScheduledExecutorService}

import scala.collection.mutable
import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService}
import scala.publish.fileset.{FileSet, Path}
import scala.publish.{Content, Hooks, PublishRepository, ScalaPublishTestSuite}

/** Exercises the Sonatype / Central Portal (OSSRH Staging API) release flow, the way Scala CLI
  * drives it (see `RepoParams` and `Publish` in scala-cli): `Hooks.sonatype(...)` with a
  * `PublishRepository.Sonatype` and a `SonatypeApi` whose base is `<repo base>/service/local`, then
  * `beforeUpload` → `repository` → `afterUpload`.
  */
class SonatypeStagingFlowTests extends ScalaPublishTestSuite {
  import SonatypeStagingFlowTests.*

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

  private def run[T](task: Task[T]): T = task.unsafeRun()(using ec)

  private def hooksFor(
    server: FakeStagingServer,
    auth: Option[Authentication] = None
  ): Hooks.Sonatype = {
    val repo = PublishRepository.Sonatype(MavenRepository(server.repoBase))
    val api  = SonatypeApi(
      server.backend,
      base = server.apiBase,
      authentication = None,
      verbosity = -1,
      stagingRepoRetryParams = noWaitRetryParams
    )
    val out = new PrintStream(new ByteArrayOutputStream)
    Hooks.sonatype(repo, api, out, verbosity = -1, batch = true, es = es) match {
      case s: Hooks.Sonatype =>
        auth.fold(s) { a =>
          // same as scala-cli's RepoParams.withAuth
          s.copy(
            repo = s.repo.withAuthentication(a),
            api = s.api.copy(authentication = Some(a))
          )
        }
      case other => fail(s"Expected Hooks.Sonatype, got $other")
    }
  }

  test("release: full staging flow (profile, start, finish, closed, promote, released)") {
    val server = new FakeStagingServer
    val hooks  = hooksFor(server)

    val data = run(hooks.beforeUpload(fileSet("org.example"), isSnapshot = false))
    assertEquals(data, Some((profileExample, server.stagedRepoId)))

    val uploadRepo = hooks.repository(data, hooks.repo, false)
    assertEquals(
      uploadRepo.map(_.root),
      Some(s"${server.repoBase}/service/local/staging/deployByRepositoryId/${server.stagedRepoId}")
    )

    run(hooks.afterUpload(data))

    assertEquals(server.repoStatus, "released")
    val calls = server.calls.map(c => (c.method, c.path))
    assertEquals(
      calls.filter(_._1 == Method.POST),
      Seq(
        Method.POST -> s"/service/local/staging/profiles/${profileExample.id}/start",
        Method.POST -> s"/service/local/staging/profiles/${profileExample.id}/finish",
        Method.POST -> "/service/local/staging/bulk/promote"
      )
    )
    assertEquals(calls.head, Method.GET -> "/service/local/staging/profiles")
    assert(
      calls.contains(
        Method.GET -> s"/service/local/staging/profile_repositories/${profileExample.id}"
      )
    )
  }

  test("release: request bodies sent to the staging API") {
    val server = new FakeStagingServer
    val hooks  = hooksFor(server)
    val data   = run(hooks.beforeUpload(fileSet("org.example"), isSnapshot = false))
    run(hooks.afterUpload(data))

    def bodyOf(pathSuffix: String) =
      server.calls.find(_.path.endsWith(pathSuffix)).map(_.body).getOrElse(fail(
        s"No call to $pathSuffix"
      ))

    assertEquals(bodyOf("/start"), """{"data":{"description":"create staging repository"}}""")
    assertEquals(
      bodyOf("/finish"),
      s"""{"data":{"description":"closing repository","stagedRepositoryId":"${server.stagedRepoId}"}}"""
    )
    assertEquals(
      bodyOf("/bulk/promote"),
      s"""{"data":{"stagedRepositoryIds":["${server.stagedRepoId}"],"description":"promoting repository","autoDropAfterRelease":true}}"""
    )
    for (c <- server.calls.filter(_.method == Method.POST))
      assertEquals(c.contentType, Some("application/json"), clue = c.path)
  }

  test("release: profile matching a parent organization is selected") {
    val server = new FakeStagingServer
    val hooks  = hooksFor(server)
    val data   = run(hooks.beforeUpload(fileSet("org.example.sub.project"), isSnapshot = false))
    assertEquals(data.map(_._1), Some(profileExample))
  }

  test("release: no matching profile fails with an explicit message") {
    val server = new FakeStagingServer
    val hooks  = hooksFor(server)
    val e      = intercept[Exception] {
      run(hooks.beforeUpload(fileSet("com.unknown"), isSnapshot = false))
    }
    assertEquals(
      e.getMessage,
      "No Sonatype profile found to publish under organization(s) com.unknown"
    )
    assert(!server.calls.exists(_.path.endsWith("/start")))
  }

  test("release: modules spanning several profiles are rejected") {
    val server = new FakeStagingServer
    val hooks  = hooksFor(server)
    val e      = intercept[Exception] {
      run(hooks.beforeUpload(fileSet("org.example", "io.other"), isSnapshot = false))
    }
    assert(
      e.getMessage.startsWith("Cannot publish to several Sonatype profiles at once"),
      clue = e.getMessage
    )
  }

  test("snapshot: no staging requests at all") {
    val server = new FakeStagingServer
    val hooks  = hooksFor(server)
    val data   = run(hooks.beforeUpload(fileSet("org.example"), isSnapshot = true))
    assertEquals(data, None)
    assertEquals(hooks.repository(data, hooks.repo, true), None)
    run(hooks.afterUpload(data))
    assertEquals(server.calls, Seq.empty)
  }

  test("authentication is sent to the staging API and set on the upload repository") {
    val server = new FakeStagingServer
    val auth   = Authentication("user", "secret")
    val hooks  = hooksFor(server, Some(auth))
    val data   = run(hooks.beforeUpload(fileSet("org.example"), isSnapshot = false))
    run(hooks.afterUpload(data))

    val expected = auth.allHttpHeaders.toMap.get("Authorization")
    assert(expected.nonEmpty)
    for (c <- server.calls)
      assertEquals(c.authorization, expected, clue = c.path)

    val uploadRepo = hooks.repository(data, hooks.repo, false)
    assertEquals(uploadRepo.flatMap(_.authentication), Some(auth))
  }

  test("close failure reported by repository activity fails fast with its message") {
    val server = new FakeStagingServer(closeFailures = Seq("Missing: no javadoc jar found"))
    val hooks  = hooksFor(server)
    val data   = run(hooks.beforeUpload(fileSet("org.example"), isSnapshot = false))
    val e      = intercept[Exception](run(hooks.afterUpload(data)))
    assertEquals(
      e.getMessage,
      s"Error waiting for repository ${server.stagedRepoId} to be closed: Missing: no javadoc jar found"
    )
    assert(!server.calls.exists(_.path.endsWith("/bulk/promote")))
  }

  test("several close failures are listed comma-separated") {
    val server = new FakeStagingServer(closeFailures =
      Seq("Missing: no javadoc jar found", "Invalid POM: missing <scm>")
    )
    val hooks = hooksFor(server)
    val data  = run(hooks.beforeUpload(fileSet("org.example"), isSnapshot = false))
    val e     = intercept[Exception](run(hooks.afterUpload(data)))
    assertEquals(
      e.getMessage,
      s"""Error waiting for repository ${server.stagedRepoId} to be closed: Missing: no javadoc jar found, Invalid POM: missing <scm>"""
    )
  }

  test("waitForStatus gives up after maxAttempt") {
    val server = new FakeStagingServer
    val api    = SonatypeApi(server.backend, server.apiBase, None, verbosity = -1)
    val e      = intercept[Exception] {
      run(api.waitForStatus(
        profileId = profileExample.id,
        repositoryId = server.stagedRepoId,
        status = "closed",
        checkActivityOpt = Some("close"),
        maxAttempt = 3,
        initialDelay = 1.millis,
        backoffFactor = 1.0,
        es = es
      ))
    }
    assert(e.getMessage.startsWith(s"Repository ${server.stagedRepoId} in state open after"))
    assertEquals(
      server.calls.count(_.path.endsWith(s"/profile_repositories/${profileExample.id}")),
      3
    )
  }

  test("waitForStatus fails when the repository is unknown") {
    val server = new FakeStagingServer
    val api    = SonatypeApi(server.backend, server.apiBase, None, verbosity = -1)
    val e      = intercept[Exception] {
      run(api.waitForStatus(
        profileId = profileExample.id,
        repositoryId = "does-not-exist",
        status = "closed",
        checkActivityOpt = None,
        maxAttempt = 3,
        initialDelay = 1.millis,
        backoffFactor = 1.0,
        es = es
      ))
    }
    assertEquals(e.getMessage, "Repository does-not-exist not found")
  }

  // Scala CLI maps these to user-friendly errors by matching on the exception message
  // (see `Publish.scala` in scala-cli), so the message shape is part of the contract.
  for (status <- Seq(401, 403))
    test(s"HTTP $status when listing profiles: exception type and message format") {
      val server = new FakeStagingServer(profilesStatus = StatusCode(status))
      val hooks  = hooksFor(server)
      val e      = intercept[FileNotFoundException] {
        run(hooks.beforeUpload(fileSet("org.example"), isSnapshot = false))
      }
      val expectedPrefix =
        s"Failed to get ${server.apiBase}/staging/profiles (http status: $status, response: "
      assert(e.getMessage.startsWith(expectedPrefix), clue = e.getMessage)
      assert(
        raw"Failed to get .*/staging/profiles \(http status: $status,.*".r.matches(e.getMessage),
        clue = e.getMessage
      )
    }

  test("HTTP 500 on staging action is retried, then reported with the response body") {
    val server = new FakeStagingServer(finishStatus = StatusCode.InternalServerError)
    val api    = SonatypeApi(
      server.backend,
      server.apiBase,
      None,
      verbosity = -1,
      stagingRepoRetryParams = noWaitRetryParams
    )
    val e = intercept[Exception] {
      api.sendCloseStagingRepositoryRequest(profileExample, server.stagedRepoId, "closing")
    }
    assertEquals(
      e.getMessage,
      s"Failed to get ${server.apiBase}/staging/profiles/${profileExample.id}/finish (http status: 500, response: finish failed)"
    )
    assertEquals(
      server.calls.count(_.path.endsWith("/finish")),
      noWaitRetryParams.attempts
    )
  }

  test("an invalid API URL is an IllegalArgumentException") {
    val server = new FakeStagingServer
    val api = SonatypeApi(server.backend, "https://staging.example.test/%zz", None, verbosity = -1)
    val e   = intercept[IllegalArgumentException](run(api.listProfiles()))
    assert(
      e.getMessage.startsWith("Invalid URL: https://staging.example.test/%zz/staging/profiles"),
      clue = e.getMessage
    )
  }

  // with autoDropAfterRelease = true (sent by promoteStagingRepository), the staging repository
  // can be dropped as soon as it's released
  test("release: a staging repository dropped right after release counts as released") {
    val server = new FakeStagingServer(dropRepoAt = Some("released"))
    val hooks  = hooksFor(server)
    val data   = run(hooks.beforeUpload(fileSet("org.example"), isSnapshot = false))
    run(hooks.afterUpload(data))
    assert(server.calls.exists(_.path.endsWith("/bulk/promote")))
  }

  test("release: a staging repository disappearing before being closed is still an error") {
    val server = new FakeStagingServer(dropRepoAt = Some("closed"))
    val hooks  = hooksFor(server)
    val data   = run(hooks.beforeUpload(fileSet("org.example"), isSnapshot = false))
    val e      = intercept[Exception](run(hooks.afterUpload(data)))
    assertEquals(e.getMessage, s"Repository ${server.stagedRepoId} not found")
    assert(!server.calls.exists(_.path.endsWith("/bulk/promote")))
  }

  test("rawCreateStagingRepository uses the same endpoint as createStagingRepository") {
    val server = new FakeStagingServer
    val api    = SonatypeApi(server.backend, server.apiBase, None, verbosity = -1)
    // profile URIs come from the server, and may not match the API base
    val profile = profileExample.copy(uri = "https://elsewhere.example.test/profiles/p-example")
    val raw     = api.rawCreateStagingRepository(profile, "create staging repository")
    assert(raw.toString.contains(server.stagedRepoId), clue = raw)
    assertEquals(
      server.calls.map(c => (c.method, c.path)),
      Seq(Method.POST -> s"/service/local/staging/profiles/${profileExample.id}/start")
    )
  }
}

object SonatypeStagingFlowTests {
  private val noWaitRetryParams = scala.publish.util.EmaRetryParams(3, 0L, 1.0f)

  private val profileExample = SonatypeApi.Profile(
    "p-example",
    "org.example",
    "https://staging.example.test/service/local/staging/profiles/p-example"
  )
  private val profileOther = SonatypeApi.Profile(
    "p-other",
    "io.other",
    "https://staging.example.test/service/local/staging/profiles/p-other"
  )

  /** A minimal Maven module file set for each organization, as Scala CLI builds it. */
  private def fileSet(orgs: String*): FileSet = {
    val now = Instant.EPOCH
    FileSet(
      orgs.flatMap { org =>
        val dir = Path(org.split('.').toSeq ++ Seq("lib_3", "1.0.0"))
        Seq(
          dir / "lib_3-1.0.0.pom" -> Content.InMemory(now, "<project/>".getBytes),
          dir / "lib_3-1.0.0.jar" -> Content.InMemory(now, Array[Byte](1, 2, 3))
        )
      }
    )
  }

  final case class Call(
    method: Method,
    path: String,
    body: String,
    authorization: Option[String],
    contentType: Option[String]
  )

  /** In-memory imitation of the Nexus 2 staging REST API, as emulated by the Central Portal OSSRH
    * Staging API.
    */
  final class FakeStagingServer(
    profilesStatus: StatusCode = StatusCode.Ok,
    finishStatus: StatusCode = StatusCode.Created,
    closeFailures: Seq[String] = Nil,
    // the staging repository disappears once it reaches this status (e.g. dropped after release)
    dropRepoAt: Option[String] = None
  ) {
    val repoBase     = "https://staging.example.test"
    val apiBase      = s"$repoBase/service/local"
    val stagedRepoId = "orgexample-1001"

    @volatile var repoStatus: String = "open"

    private val calls0   = mutable.ArrayBuffer.empty[Call]
    def calls: Seq[Call] = calls0.synchronized(calls0.toList)

    private def json(s: String, code: StatusCode = StatusCode.Ok) =
      Response(s.getBytes(StandardCharsets.UTF_8), code)

    private def record(r: Request[?, ?]): Call = {
      val body = r.body match {
        case ByteArrayBody(b, _) => new String(b, StandardCharsets.UTF_8)
        case StringBody(s, _, _) => s
        case _                   => ""
      }
      val c = Call(
        r.method,
        r.uri.path.mkString("/", "/", ""),
        body,
        r.headers.find(_.name.equalsIgnoreCase("Authorization")).map(_.value),
        r.headers.find(_.name.equalsIgnoreCase("Content-Type")).map(_.value)
      )
      calls0.synchronized(calls0 += c)
      c
    }

    private val profilesJson =
      Seq(profileExample, profileOther)
        .map(p => s"""{"id":"${p.id}","name":"${p.name}","resourceURI":"${p.uri}"}""")
        .mkString("""{"data":[""", ",", "]}")

    private def handle(r: Request[?, ?]): Response[Array[Byte]] = {
      val c = record(r)
      (c.method, c.path) match {
        case (Method.GET, "/service/local/staging/profiles") =>
          if profilesStatus.isSuccess then json(profilesJson)
          else json("""{"errors":[{"msg":"denied"}]}""", profilesStatus)

        case (Method.POST, p)
            if p == s"/service/local/staging/profiles/${profileExample.id}/start" =>
          json(
            s"""{"data":{"stagedRepositoryId":"$stagedRepoId","description":"x"}}""",
            StatusCode.Created
          )

        case (Method.POST, p)
            if p == s"/service/local/staging/profiles/${profileExample.id}/finish" =>
          if finishStatus.isSuccess then {
            if closeFailures.isEmpty then repoStatus = "closed"
            json("", finishStatus)
          }
          else json("finish failed", finishStatus)

        case (Method.POST, "/service/local/staging/bulk/promote") =>
          repoStatus = "released"
          json("", StatusCode.Created)

        case (Method.GET, p)
            if p == s"/service/local/staging/profile_repositories/${profileExample.id}" =>
          if dropRepoAt.contains(repoStatus) then json("""{"data":[]}""")
          else
            json(
              s"""{"data":[{"profileId":"${profileExample.id}","profileName":"${profileExample.name}","repositoryId":"$stagedRepoId","type":"$repoStatus"}]}"""
            )

        case (Method.GET, p) if p == s"/service/local/staging/repository/$stagedRepoId/activity" =>
          val events = closeFailures.map { msg =>
            s"""{"name":"ruleFailed","severity":1,"properties":[{"name":"failureMessage","value":"$msg"}]}"""
          }.mkString(",")
          json(s"""[{"name":"close","events":[$events]}]""")

        case _ =>
          json("not found", StatusCode.NotFound)
      }
    }

    val backend: SttpBackendStub[sttp.client3.Identity, Any] =
      SttpBackendStub.synchronous.whenAnyRequest.thenRespondF(r => handle(r))
  }

}
