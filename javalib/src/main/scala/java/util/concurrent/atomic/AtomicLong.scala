package java.util.concurrent.atomic

class AtomicLong(private[this] var value: Long) extends Serializable {
  def get(): Long = value
  def set(newValue: Long): Unit = value = newValue
  def lazySet(newValue: Long): Unit = set(newValue)
  def compareAndSet(expect: Long, newValue: Long): Boolean = {
    if (expect != value) false else {
      value = newValue
      true
    }
  }
  def weakCompareAndSet(expect: Long, newValue: Long): Boolean = compareAndSet(expect, newValue)
  def getAndSet(newValue: Long): Long = {
    val old = value
    value = newValue
    old
  }
  def getAndIncrement(): Long = {
    value += 1
    value - 1
  }
  def getAndDecrement(): Long = {
    value -= 1
    value + 1
  }
  def getAndAdd(delta: Long): Long = {
    value += delta
    value - delta
  }
  def incrementAndGet(): Long = {
    value += 1
    value
  }
  def decrementAndGet(): Long = {
    value -= 1
    value
  }
  def addAndGet(delta: Long): Long = {
    value += delta
    value
  }
  def intValue(): Int = value.toInt
  def longValue(): Long = value.toLong
  def floatValue(): Float = value.toFloat
  def doubleValue(): Double = value.toDouble
}
