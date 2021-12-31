package java.lang.ref
import scala.scalajs.js

class WeakReference[T] private (jsWeakRef: js.WeakRef[T]) extends Reference[T]() {
  def this(referent: T) = this(new js.WeakRef[T](referent))
  def get(): T = {
    val referent = jsWeakRef.deref()
    (if (js.isUndefined(referent)) {
       null
     } else {
       referent
     }).asInstanceOf[T]
  }
}
