package java.util.concurrent.atomic

class AtomicLongArray(length: Int) extends Serializable {
  def this(array: Array[Long]) = {
    this(array.size)
    System.arraycopy(array, 0, inner, 0, length)
  }

  private val inner: Array[Long] = new Array[Long](length)

  final def length(): Int =
    inner.length

  final def get(i: Int): Long =
    inner(i)

  final def set(i: Int, newValue: Long): Unit =
    inner(i) = newValue

  final def lazySet(i: Int, newValue: Long): Unit =
    set(i, newValue)

  final def getAndSet(i: Int, newValue: Long): Long = {
    val ret = get(i)
    set(i, newValue)
    ret
  }

  final def compareAndSet(i: Int, expect: Long, update: Long): Boolean = {
    if (get(i) != expect) {
      false
    } else {
      set(i, update)
      true
    }
  }

  final def weakCompareAndSet(i: Int, expect: Long, update: Long): Boolean =
    compareAndSet(i, expect, update)

  final def getAndIncrement(i: Int): Long =
    getAndAdd(i, 1)

  final def getAndDecrement(i: Int): Long =
    getAndAdd(i, -1)

  final def getAndAdd(i: Int, delta: Long): Long = {
    val ret = get(i)
    set(i, ret + delta)
    ret
  }

  final def incrementAndGet(i: Int): Long =
    addAndGet(i, 1)

  final def decrementAndGet(i: Int): Long =
    addAndGet(i, -1)

  final def addAndGet(i: Int, delta: Long): Long = {
    set(i, get(i) + delta)
    get(i)
  }

  override def toString(): String =
    inner.mkString("[", ", ", "]")
}
