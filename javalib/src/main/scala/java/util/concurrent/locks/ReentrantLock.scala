package java.util.concurrent.locks

import java.io.Serializable
import java.lang.Thread
import java.util.concurrent.TimeUnit

class ReentrantLock(fair: Boolean) extends Lock with Serializable {

  private var locked = 0

  def this() = this(false)

  def lock(): Unit = locked += 1

  def lockInterruptibly(): Unit = {
    if (Thread.interrupted())
      throw new InterruptedException()
    else
      lock()
  }

  def tryLock(): Boolean = {
    locked += 1
    true
  }

  def tryLock(time: Long, unit: TimeUnit): Boolean = {
    if (Thread.interrupted())
      throw new InterruptedException()
    else
      tryLock()
  }

  def unlock(): Unit = {
    if (locked <= 0)
      throw new IllegalMonitorStateException()
    else
      locked -= 1
  }

  //Not implemented:
  //def newCondition(): Condition

  def getHoldCount(): Int = locked

  def isHeldByCurrentThread(): Boolean = isLocked()

  def isLocked(): Boolean = locked > 0

  final def isFair(): Boolean = fair

  protected def getOwner(): Thread = {
    if (isLocked)
      Thread.currentThread()
    else
      null
  }

  //Not Implemented
  //final def hasQueuedThreads(): Boolean

  //Not Implemented
  //final def hasQueuedThread(thread: Thread): Boolean

  //Not Imolemented
  //final def getQueueLength(): Int

  //Not Implemented
  //protected def getQueuedThreads(): Collection[Thread]

  //Not Implemented
  //def hasWaiters(condition: Condition): Boolean

  //Not Implemented
  //def getWaitQueueLength(condition: Condition): Int

  //Not Implemented
  //protected def getWaitingThreads(condition: Condition): Collection[Thread]

  override def toString(): String = {
    val lckString =
      if (isLocked()) s"Locked by ${Thread.currentThread().getName()}"
      else "Unlocked"

    s"${super.toString()}[$lckString]"
  }
}
