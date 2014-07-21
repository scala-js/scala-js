package java.lang.ref

class SoftReference[T >: Null <: AnyRef](referent: T, queue: ReferenceQueue[_]) extends Reference[T](referent) {
  def this(referent: T) = this(referent, null)
}
