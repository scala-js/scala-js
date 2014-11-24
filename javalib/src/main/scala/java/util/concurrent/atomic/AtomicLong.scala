package java.util.concurrent.atomic

class AtomicLong(private[this] var value: Long) extends Number with Serializable {
  def this() = this(0L)

  final def get(): Long = value

  final def set(newValue: Long): Unit =
    value = newValue

  final def lazySet(newValue: Long): Unit =
    set(newValue)

  final def getAndSet(newValue: Long): Long = {
    val old = value
    value = newValue
    old
  }

  final def compareAndSet(expect: Long, update: Long): Boolean = {
    if (expect != value) false else {
      value = update
      true
    }
  }

  final def weakCompareAndSet(expect: Long, update: Long): Boolean =
    compareAndSet(expect, update)

  final def getAndIncrement(): Long =
    getAndAdd(1L)

  final def getAndDecrement(): Long =
    getAndAdd(-1L)

  @inline final def getAndAdd(delta: Long): Long = {
    val old = value
    value = old + delta
    old
  }

  final def incrementAndGet(): Long =
    addAndGet(1L)

  final def decrementAndGet(): Long =
    addAndGet(-1L)

  @inline final def addAndGet(delta: Long): Long = {
    val newValue = value + delta
    value = newValue
    newValue
  }

  override def toString(): String =
    value.toString()

  def intValue(): Int = value.toInt
  def longValue(): Long = value
  def floatValue(): Float = value.toFloat
  def doubleValue(): Double = value.toDouble
}
