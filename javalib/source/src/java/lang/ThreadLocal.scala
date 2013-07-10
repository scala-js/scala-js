package java.lang

class ThreadLocal[T] {
  private final var hasValue = false
  private final var i: T = _
  private final var v: T = _
  private final var m: ThreadLocal.ThreadLocalMap = new ThreadLocal.ThreadLocalMap()

  protected def initialValue(): T = i

  def get(): T = {
    if (!hasValue)
      set(initialValue)
    v
  }

  def remove() {
    hasValue = false
  }

  def set(o: T) {
    v = o
    hasValue = true
  }

  def childValue(parentValue: T): T = parentValue

  def createMap(t: Thread, firstValue: T) {}
  def getMap(t: Thread) = m
}

object ThreadLocal {
  class ThreadLocalMap
}
