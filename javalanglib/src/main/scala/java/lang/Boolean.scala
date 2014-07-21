package java.lang

// This class is not emitted, but we need to define its members correctly
final class Boolean(value: scala.Boolean) extends Comparable[Boolean] {

  def this(v: String) = this(Boolean.parseBoolean(v))

  def booleanValue(): scala.Boolean = sys.error("stub")

  override def equals(that: Any): scala.Boolean = sys.error("stub")

  override def compareTo(that: Boolean): Int = sys.error("stub")

  override def toString(): String = sys.error("stub")

  override def hashCode(): Int = sys.error("stub")

}

object Boolean {
  val TYPE = classOf[scala.Boolean]
  val TRUE = new Boolean(true)
  val FALSE = new Boolean(false)

  def valueOf(booleanValue: scala.Boolean): Boolean =
    if (booleanValue) TRUE else FALSE
  def valueOf(s: String): Boolean = valueOf(parseBoolean(s))

  def parseBoolean(s: String): scala.Boolean =
    (s != null) && s.equalsIgnoreCase("true")

  def toString(b: scala.Boolean): String = if (b) "true" else "false"
}
