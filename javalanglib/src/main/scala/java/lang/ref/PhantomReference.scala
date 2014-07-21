package java.lang.ref


class PhantomReference[T >: Null <: AnyRef](referent: T, queue: ReferenceQueue[_]) extends Reference[T](referent) {
  def this(referent: T) = this(referent, null)
  override def get: T = null
}
