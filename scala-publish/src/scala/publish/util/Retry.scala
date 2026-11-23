package scala.publish.util

import scala.annotation.tailrec

private[publish] object Retry {

  /** Runs `attempt` until its result isn't retryable, or the maximum number of attempts is reached
    *
    * @param params
    *   maximum number of attempts, wait before the first retry, and factor applied to that wait
    *   before each subsequent retry
    * @param sleep
    *   waits for the passed number of milliseconds
    * @param retryable
    *   whether a result should be retried
    * @param attempt
    *   performs an attempt, given its number (starting at 1)
    * @return
    *   the result of the last attempt
    */
  def apply[T](params: EmaRetryParams, sleep: Long => Unit)(retryable: T => Boolean)(
    attempt: Int => T
  ): T = {
    @tailrec
    def loop(n: Int, waitMs: Long): T = {
      val res = attempt(n)
      if retryable(res) && n < params.attempts then {
        sleep(waitMs)
        loop(n + 1, params.update(waitMs))
      }
      else res
    }

    loop(1, params.initialWaitDurationMs)
  }

  def threadSleep(ms: Long): Unit = Thread.sleep(ms)
}
