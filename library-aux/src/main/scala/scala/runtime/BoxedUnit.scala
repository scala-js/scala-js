package scala.runtime

class BoxedUnit private {
  override def equals(that: Any): Boolean = sys.error("stub")

  override def hashCode(): Int = sys.error("stub")

  override def toString(): String = sys.error("stub")
}

object BoxedUnit {
  val UNIT: BoxedUnit = sys.error("stub")
  val TYPE: Class[Void] = sys.error("stub")
}
