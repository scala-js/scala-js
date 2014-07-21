package java.util.concurrent.atomic

class AtomicBoolean(private[this] var value: Boolean) extends Serializable {
  def get(): Boolean = value
  def set(newValue: Boolean): Unit = value = newValue
  def lazySet(newValue: Boolean): Unit = set(newValue)
  def compareAndSet(expect: Boolean, newValue: Boolean): Boolean = {
    if (expect != value) false else {
      value = newValue
      true
    }
  }
  def weakCompareAndSet(expect: Boolean, newValue: Boolean): Boolean = compareAndSet(expect, newValue)
  def getAndSet(newValue: Boolean) = {
    val old = value
    value = newValue
    old
  }
}
