package java.lang.ref

class WeakReference[T >: Null <: AnyRef](referent: T,
    queue: ReferenceQueue[_ >: T]) extends Reference[T](referent) {

  def this(referent: T) = this(referent, null)
}
