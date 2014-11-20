package java.lang

class ThreadLocal[T] {
  private var hasValue: Boolean = false
  private var v: T = _

  protected def initialValue(): T = null.asInstanceOf[T]

  def get(): T = {
    if (!hasValue)
      set(initialValue)
    v
  }

  def set(o: T): Unit = {
    v = o
    hasValue = true
  }

  def remove(): Unit = {
    hasValue = false
    v = null.asInstanceOf[T] // for gc
  }
}
