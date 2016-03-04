package org.scalajs.testsuite.jsinterop

import scala.scalajs.js

object TimeoutMock {

  private var installed = false

  @noinline
  def withMockedTimeout[A](body: (Int => Unit) => A): A = {
    assert(!installed, "Mock timeout already installed.")
    import js.Dynamic.global

    val realSetTimeout = global.setTimeout
    val realClearTimeout = global.clearTimeout

    val mockTimeouts = new MockTimeouts
    val mockSetTimeout: js.Function =
      (fun: js.Function0[_], delay: Int) => mockTimeouts.setTimeout(fun, delay)
    val mockClearTimeout: js.Function =
      (timeout: MockTimeout) => mockTimeouts.clearTimeout(timeout)

    try {
      global.setTimeout = mockSetTimeout
      global.clearTimeout = mockClearTimeout
      installed = true
      body(mockTimeouts.tick)
    } finally {
      global.setTimeout = realSetTimeout
      global.clearTimeout = realClearTimeout
      installed = false
    }
  }

  private class MockTimeouts {
    private var timeouts = List.empty[MockTimeout]
    private var currentTime: Int = 0

    def setTimeout(fun: js.Function0[_], delay: Int): MockTimeout = {
      assert(delay >= 0, "Delay should be positive.")
      val triggerTime = currentTime + delay
      assert(triggerTime >= 0, "Time overflow")
      val (before, after) = timeouts.span(_.triggerTime <= triggerTime)
      val timeout = new MockTimeout(triggerTime, fun)
      timeouts = before ::: timeout :: after
      timeout
    }

    def clearTimeout(timeout: MockTimeout): Unit = {
      timeout.clearTimeout()
    }

    def tick(ms: Int): Unit = {
      assert(ms >= 0)
      val targetTime = currentTime + ms
      assert(targetTime >= 0)
      while (timeouts.nonEmpty && timeouts.head.triggerTime <= targetTime) {
        val timeout = timeouts.head
        timeouts = timeouts.tail
        currentTime = timeout.triggerTime
        timeout.execute()
      }
      currentTime = targetTime
    }
  }

  class MockTimeout(val triggerTime: Int, fun: js.Function0[_]) {
    private var cleared = false

    def clearTimeout(): Unit = {
      cleared = true
    }

    def execute(): Unit = {
      if (!cleared)
        fun()
    }
  }
}
