package scala.runtime

/* This is a hijacked class. Its only instance is the value 'undefined'.
 * Constructors are not emitted.
 */
class BoxedUnit private () extends AnyRef with java.io.Serializable {
  @inline override def equals(that: Any): Boolean =
    this eq that.asInstanceOf[AnyRef]

  @inline override def hashCode(): Int = 0

  @inline override def toString(): String = "undefined"
}

object BoxedUnit {
  def UNIT: BoxedUnit = sys.error("stub")

  final val TYPE = classOf[Unit]
}
