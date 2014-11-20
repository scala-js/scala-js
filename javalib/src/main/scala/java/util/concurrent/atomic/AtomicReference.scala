package java.util.concurrent.atomic

class AtomicReference[T <: AnyRef](
    private[this] var value: T) extends Serializable {

  def this() = this(null.asInstanceOf[T])

  final def get(): T = value

  final def set(newValue: T): Unit =
    value = newValue

  final def lazySet(newValue: T): Unit =
    set(newValue)

  final def compareAndSet(expect: T, update: T): Boolean = {
    if (expect ne value) false else {
      value = update
      true
    }
  }

  final def weakCompareAndSet(expect: T, update: T): Boolean =
    compareAndSet(expect, update)

  final def getAndSet(newValue: T): T = {
    val old = value
    value = newValue
    old
  }

  override def toString(): String =
    String.valueOf(value)
}
