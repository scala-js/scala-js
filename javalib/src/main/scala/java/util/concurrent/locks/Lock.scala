package java.util.concurrent.locks

import java.util.concurrent.TimeUnit

trait Lock {
  def lock(): Unit
  def lockInterruptibly(): Unit
  def tryLock(): Boolean
  def tryLock(time: Long, unit: TimeUnit): Boolean
  def unlock(): Unit

  //Not implemented:
  //def newCondition(): Condition
}
