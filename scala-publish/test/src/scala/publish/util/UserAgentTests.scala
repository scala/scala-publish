package scala.publish.util

import scala.publish.ScalaPublishTestSuite

class UserAgentTests extends ScalaPublishTestSuite {

  test("format, with the major and minor version only") {
    assertEquals(
      UserAgent("1.1.0"),
      "scala-publish/1.1 (+https://github.com/scala/scala-publish)"
    )
    assertEquals(
      UserAgent("1.2.3-SNAPSHOT"),
      "scala-publish/1.2 (+https://github.com/scala/scala-publish)"
    )
    assertEquals(
      UserAgent("1.1.0+12-abcdef12-SNAPSHOT"),
      "scala-publish/1.1 (+https://github.com/scala/scala-publish)"
    )
  }

  test("unparseable versions are left out") {
    assertEquals(UserAgent(""), "scala-publish (+https://github.com/scala/scala-publish)")
    assertEquals(UserAgent("foo"), "scala-publish (+https://github.com/scala/scala-publish)")
  }

  test("default uses the version of the library") {
    assertEquals(UserAgent.default, UserAgent(BuildInfo.version))
    assert(UserAgent.default.startsWith("scala-publish/"), clue = UserAgent.default)
  }

  test("the scala.publish.http.agent Java property overrides the default") {
    assertEquals(UserAgent.current(Map.empty), UserAgent.default)
    assertEquals(
      UserAgent.current(Map("scala.publish.http.agent" -> "Scala-CLI/1.10 scala-publish/1.1")),
      "Scala-CLI/1.10 scala-publish/1.1"
    )
  }
}
