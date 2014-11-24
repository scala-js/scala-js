package java.util.concurrent.atomic

class AtomicInteger(private[this] var value: Int)
    extends Number with Serializable {

  def this() = this(0)

  final def get(): Int = value

  final def set(newValue: Int): Unit =
    value = newValue

  final def lazySet(newValue: Int): Unit =
    set(newValue)

  final def getAndSet(newValue: Int): Int = {
    val old = value
    value = newValue
    old
  }

  final def compareAndSet(expect: Int, update: Int): Boolean = {
    if (expect != value) false else {
      value = update
      true
    }
  }

  final def weakCompareAndSet(expect: Int, update: Int): Boolean =
    compareAndSet(expect, update)

  final def getAndIncrement(): Int =
    getAndAdd(1)

  final def getAndDecrement(): Int =
    getAndAdd(-1)

  @inline final def getAndAdd(delta: Int): Int = {
    val old = value
    value = old + delta
    old
  }

  final def incrementAndGet(): Int =
    addAndGet(1)

  final def decrementAndGet(): Int =
    addAndGet(-1)

  @inline final def addAndGet(delta: Int): Int = {
    val newValue = value + delta
    value = newValue
    newValue
  }

  override def toString(): String =
    value.toString()

  def intValue(): Int = value
  def longValue(): Long = value.toLong
  def floatValue(): Float = value.toFloat
  def doubleValue(): Double = value.toDouble
}
