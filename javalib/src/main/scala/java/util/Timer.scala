package java.util

import scala.collection._
import scala.concurrent.duration._

class Timer() {
  private[util] var canceled: Boolean = false

  def this(isDaemon: Boolean) = this()

  def this(name: String) = this()

  def this(name: String, isDaemon: Boolean) = this()

  private def acquire(task: TimerTask): Unit = {
    if (canceled)
      throw new IllegalStateException("Timer already cancelled.")
    else if (task.owner != null || task.canceled) {
      throw new IllegalStateException("TimerTask already scheduled or canceled.")
    }
    task.owner = this
  }

  private def checkDelay(delay: Long): Unit = {
    if (delay < 0 || (delay + System.currentTimeMillis) < 0)
      throw new IllegalArgumentException("Negative delay.")
  }

  private def checkTime(time: Date): Unit = {
    if (time.getTime < 0)
      throw new IllegalArgumentException(s"Negative time: $time.")
  }

  private def checkPeriod(period: Long): Unit = {
    if (period <= 0)
      throw new IllegalArgumentException("Non-positive period.")
  }

  private def scheduleOnce(task: TimerTask, delay: Long): Unit = {
    acquire(task)
    task.timeout(delay.millis) {
      task.scheduledOnceAndStarted = true
      task.doRun()
    }
  }

  private def getMillisUntil(time: Date): Long =
    math.max(0L, time.getTime - System.currentTimeMillis())

  def schedule(task: TimerTask, delay: Long): Unit = {
    checkDelay(delay)
    scheduleOnce(task, delay)
  }

  def schedule(task: TimerTask, time: Date): Unit = {
    checkTime(time)
    val delay = getMillisUntil(time)
    scheduleOnce(task, delay)
  }

  private def schedulePeriodically(
      task: TimerTask, delay: Long, period: Long): Unit = {
    acquire(task)
    task.timeout(delay.millis) {
      def loop(): Unit = {
        val startTime = System.nanoTime()
        task.doRun()
        val endTime = System.nanoTime()
        val duration = (endTime - startTime) / 1000000
        task.timeout((period - duration).millis) {
          loop()
        }
      }
      loop()
    }
  }

  def schedule(task: TimerTask, delay: Long, period: Long): Unit = {
    checkDelay(delay)
    checkPeriod(period)
    schedulePeriodically(task, delay, period)
  }

  def schedule(task: TimerTask, firstTime: Date, period: Long): Unit = {
    checkTime(firstTime)
    checkPeriod(period)
    val delay = getMillisUntil(firstTime)
    schedulePeriodically(task, delay, period)
  }

  private def scheduleFixed(
      task: TimerTask, delay: Long, period: Long): Unit = {
    acquire(task)
    task.timeout(delay.millis) {
      def loop(scheduledTime: Long): Unit = {
        task.doRun()
        val nextScheduledTime = scheduledTime + period
        val nowTime = System.nanoTime / 1000000L
        if (nowTime >= nextScheduledTime) {
          // Re-run immediately.
          loop(nextScheduledTime)
        } else {
          // Re-run after a timeout.
          task.timeout((nextScheduledTime - nowTime).millis) {
            loop(nextScheduledTime)
          }
        }
      }
      loop(System.nanoTime / 1000000L + period)
    }
  }

  def scheduleAtFixedRate(task: TimerTask, delay: Long, period: Long): Unit = {
    checkDelay(delay)
    checkPeriod(period)
    scheduleFixed(task, delay, period)
  }

  def scheduleAtFixedRate(task: TimerTask, firstTime: Date, period: Long): Unit = {
    checkTime(firstTime)
    checkPeriod(period)
    val delay = getMillisUntil(firstTime)
    scheduleFixed(task, delay, period)
  }

  def cancel(): Unit = canceled = true

  def purge(): Int = 0

}
