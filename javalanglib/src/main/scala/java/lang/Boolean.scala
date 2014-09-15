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
  final val TYPE = classOf[scala.Boolean]

  /* TRUE and FALSE are supposed to be vals. However, they are better
   * optimized as defs, because they end up being just the constant true and
   * false (since `new Boolean(x)` is a no-op).
   * Since vals and defs are binary-compatible (although they're not strictly
   * speaking source-compatible, because of stability), we implement them as
   * defs. Source-compatibility is not an issue because user code is compiled
   * against the JDK .class files anyway.
   * Moreover, preserving the identity of TRUE and FALSE is not an issue
   * either, since they are primitive booleans in the end.
   */
  def TRUE: Boolean = new Boolean(true)
  def FALSE: Boolean = new Boolean(false)

  @inline def valueOf(booleanValue: scala.Boolean): Boolean = {
    // We don't care about identity, since they end up as primitive booleans
    new Boolean(booleanValue)
  }

  @inline def valueOf(s: String): Boolean = valueOf(parseBoolean(s))

  @inline def parseBoolean(s: String): scala.Boolean =
    (s != null) && s.equalsIgnoreCase("true")

  @inline def toString(b: scala.Boolean): String = b.toString
}
