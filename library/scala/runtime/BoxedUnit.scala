package scala.runtime

class BoxedUnit private {
  override def equals(that: Any) = this eq that.asInstanceOf[AnyRef]

  override def hashCode() = 0

  override def toString() = "()"
}

object BoxedUnit {
  val UNIT = new BoxedUnit
  val TYPE = java.lang.Void.TYPE
}
