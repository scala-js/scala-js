package java.util.concurrent.atomic

class AtomicReferenceArray[E <: AnyRef](
    length: Int) extends Serializable {

  def this(array: Array[E]) = {
    this(array.size)
    System.arraycopy(array, 0, inner, 0, length)
  }

  private val inner: Array[AnyRef] = new Array[AnyRef](length)

  final def length(): Int =
    inner.length

  final def get(i: Int): E =
    inner(i).asInstanceOf[E]

  final def set(i: Int, newValue: E): Unit =
    inner(i) = newValue

  final def lazySet(i: Int, newValue: E): Unit =
    set(i, newValue)

  final def getAndSet(i: Int, newValue: E): E = {
    val ret = get(i)
    set(i, newValue)
    ret
  }

  final def compareAndSet(i: Int, expect: E, update: E): Boolean = {
    if (get(i) ne expect) false else {
      set(i, update)
      true
    }
  }

  final def weakCompareAndSet(i: Int, expect: E, update: E): Boolean =
    compareAndSet(i, expect, update)

  override def toString(): String =
    inner.mkString("[", ", ", "]")
}
