package java.lang

import scala.scalajs.js

/* This is a hijacked class. Its instances are primitive booleans.
 * Constructors are not emitted.
 */
final class Boolean private ()
    extends AnyRef with java.io.Serializable with Comparable[Boolean] {

  def this(value: scala.Boolean) = this()
  def this(v: String) = this()

  @inline def booleanValue(): scala.Boolean =
    this.asInstanceOf[scala.Boolean]

  @inline override def equals(that: Any): scala.Boolean =
    this eq that.asInstanceOf[AnyRef]

  @inline override def hashCode(): Int =
    if (booleanValue) 1231 else 1237

  @inline override def compareTo(that: Boolean): Int =
    Boolean.compare(booleanValue, that.booleanValue)

  @inline override def toString(): String =
    Boolean.toString(booleanValue)

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

  @inline def toString(b: scala.Boolean): String =
    "" + b

  @inline def compare(x: scala.Boolean, y: scala.Boolean): scala.Int =
    if (x == y) 0 else if (x) 1 else -1
}
