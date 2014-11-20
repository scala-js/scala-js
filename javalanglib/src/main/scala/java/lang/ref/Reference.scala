package java.lang.ref

abstract class Reference[T >: Null <: AnyRef](private[this] var referent: T) {
  def get(): T = referent
  def clear(): Unit = referent = null
  def isEnqueued(): Boolean = false
  def enqueue(): Boolean = false
}
