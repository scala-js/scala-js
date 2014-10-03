package java.util.concurrent.atomic

class AtomicInteger(private[this] var value: Int) extends Serializable {
  def get(): Int = value
  def set(newValue: Int): Unit = value = newValue
  def lazySet(newValue: Int): Unit = set(newValue)
  def compareAndSet(expect: Int, newValue: Int): Boolean = {
    if (expect != value) false else {
      value = newValue
      true
    }
  }
  def weakCompareAndSet(expect: Int, newValue: Int): Boolean = compareAndSet(expect, newValue)
  def getAndSet(newValue: Int): Int = {
    val old = value
    value = newValue
    old
  }
  def getAndIncrement(): Int = {
    value += 1
    value - 1
  }
  def getAndDecrement(): Int = {
    value -= 1
    value + 1
  }
  def getAndAdd(delta: Int): Int = {
    value += delta
    value - delta
  }
  def incrementAndGet(): Int = {
    value += 1
    value
  }
  def decrementAndGet(): Int = {
    value -= 1
    value
  }
  def addAndGet(delta: Int): Int = {
    value += delta
    value
  }
  def intValue(): Int = value.toInt
  def longValue(): Long = value.toLong
  def floatValue(): Float = value.toFloat
  def doubleValue(): Double = value.toDouble
}
