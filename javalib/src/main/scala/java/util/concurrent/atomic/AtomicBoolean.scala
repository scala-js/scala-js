package java.util.concurrent.atomic

class AtomicBoolean(private[this] var value: Boolean) extends Serializable {
  def this() = this(false)

  final def get(): Boolean = value

  final def compareAndSet(expect: Boolean, update: Boolean): Boolean = {
    if (expect != value) false else {
      value = update
      true
    }
  }

  // For some reason, this method is not final
  def weakCompareAndSet(expect: Boolean, update: Boolean): Boolean =
    compareAndSet(expect, update)

  final def set(newValue: Boolean): Unit =
    value = newValue

  final def lazySet(newValue: Boolean): Unit =
    set(newValue)

  final def getAndSet(newValue: Boolean): Boolean = {
    val old = value
    value = newValue
    old
  }

  override def toString(): String =
    value.toString()
}
