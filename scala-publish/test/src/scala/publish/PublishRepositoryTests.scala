package scala.publish

import coursier.core.Authentication
import coursier.maven.MavenRepository

/** Pins the repository URLs derived by [[PublishRepository]] for the bases Scala CLI uses (see
  * `RepoParams` in scala-cli). These are current behaviours: a change here changes where Scala CLI
  * uploads to, or what it prints to users.
  */
class PublishRepositoryTests extends ScalaPublishTestSuite {
  private val centralStaging = "https://ossrh-staging-api.central.sonatype.com"
  private val legacyOss      = "https://oss.sonatype.org"
  private val legacyS01      = "https://s01.oss.sonatype.org"

  test("Sonatype defaults target the Central Portal OSSRH Staging API") {
    val repo = PublishRepository.Sonatype()
    assertEquals(repo.base.root, centralStaging)
    assertEquals(repo.centralPortalBase, "https://central.sonatype.com")
    assertEquals(repo.useLegacySnapshots, false)
  }

  test("Sonatype (Central Portal) derived repositories") {
    val repo = PublishRepository.Sonatype(MavenRepository(centralStaging))
    assertEquals(repo.restBase, s"$centralStaging/service/local")
    assertEquals(repo.snapshotRepo.root, "https://central.sonatype.com/repository/maven-snapshots")
    assertEquals(repo.readSnapshotRepo.root, repo.snapshotRepo.root)
    assertEquals(repo.releaseRepo.root, s"$centralStaging/service/local/staging/deploy/maven2")
    assertEquals(
      repo.releaseRepoOf("orgexample-1001").root,
      s"$centralStaging/service/local/staging/deployByRepositoryId/orgexample-1001"
    )
    assertEquals(repo.readReleaseRepo.root, s"$centralStaging/content/repositories/releases")
    assertEquals(repo.repo(isSnapshot = true).root, repo.snapshotRepo.root)
    assertEquals(repo.repo(isSnapshot = false).root, repo.releaseRepo.root)
    assertEquals(repo.checkResultsRepo(isSnapshot = true).root, repo.snapshotRepo.root)
    assertEquals(
      repo.checkResultsRepo(isSnapshot = false).root,
      s"$centralStaging/content/repositories/public"
    )
  }

  for (base <- Seq(legacyOss, legacyS01))
    test(s"Sonatype legacy ($base) with legacy snapshots") {
      val repo = PublishRepository.Sonatype(MavenRepository(base), useLegacySnapshots = true)
      assertEquals(repo.snapshotRepo.root, s"$base/content/repositories/snapshots")
      assertEquals(repo.releaseRepo.root, s"$base/service/local/staging/deploy/maven2")
      assertEquals(
        repo.releaseRepoOf("id-1").root,
        s"$base/service/local/staging/deployByRepositoryId/id-1"
      )
      assertEquals(
        repo.checkResultsRepo(isSnapshot = false).root,
        s"$base/content/repositories/public"
      )
    }

  test("Sonatype.withAuthentication keeps the Sonatype type and authenticates all derived repos") {
    val auth                             = Authentication("user", "secret")
    val repo: PublishRepository.Sonatype =
      PublishRepository.Sonatype(MavenRepository(centralStaging)).withAuthentication(auth)
    val derived = Seq(
      repo.snapshotRepo,
      repo.releaseRepo,
      repo.releaseRepoOf("id-1"),
      repo.readReleaseRepo,
      repo.checkResultsRepo(isSnapshot = false)
    )
    for (r <- derived)
      assertEquals(r.authentication, Some(auth), clue = r.root)
  }

  test("Simple repository: same repo for snapshots and releases, authentication propagated") {
    val auth  = Authentication("user", "secret")
    val write = MavenRepository("https://repo.example.test/maven")
    val read  = MavenRepository("https://read.example.test/maven")

    val simple = PublishRepository.Simple(write)
    assertEquals(simple.repo(isSnapshot = true), write)
    assertEquals(simple.repo(isSnapshot = false), write)
    assertEquals(simple.readRepo(isSnapshot = false), write)
    assertEquals(simple.checkResultsRepo(isSnapshot = false), write)

    val withRead = PublishRepository.Simple(write, Some(read)).withAuthentication(auth)
    assertEquals(withRead.snapshotRepo.authentication, Some(auth))
    assertEquals(withRead.readRepo(isSnapshot = false).root, read.root)
    assertEquals(withRead.readRepo(isSnapshot = false).authentication, Some(auth))
  }

  test("PublishRepository.withAuthentication via the base type (as scala-cli calls it)") {
    val auth                          = Authentication("user", "secret")
    val repos: Seq[PublishRepository] = Seq(
      PublishRepository.Simple(MavenRepository("https://repo.example.test/maven")),
      PublishRepository.Sonatype(MavenRepository(centralStaging))
    )
    for (r <- repos) {
      val authed = r.withAuthentication(auth)
      assertEquals(authed.getClass, r.getClass)
      assertEquals(authed.repo(isSnapshot = false).authentication, Some(auth))
      assertEquals(authed.repo(isSnapshot = true).authentication, Some(auth))
    }
  }
}
