package java.util.concurrent.atomic

/**
 * Created by haoyi on 1/22/14.
 */
class AtomicReference[T](var value: T) extends java.io.Serializable {
  def get(): T = value
  def set(newValue: T): Unit = value = newValue
  def lazySet(newValue: T): Unit = set(newValue)
  def compareAndSet(expect: T, newValue: T): Boolean = {
    if (expect != value) false else {
      value = newValue
      true
    }
  }
  def weakCompareAndSet(expect: T, newValue: T): Boolean = compareAndSet(expect, newValue)
  def getAndSet(newValue: T): T = {
    val old = value
    value = newValue
    old
  }
}
