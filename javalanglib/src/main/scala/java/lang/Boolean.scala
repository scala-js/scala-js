/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

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
    if (booleanValue()) 1231 else 1237

  @inline override def compareTo(that: Boolean): Int =
    Boolean.compare(booleanValue(), that.booleanValue())

  @inline override def toString(): String =
    Boolean.toString(booleanValue())

}

object Boolean {
  /* TYPE should be a `final val`, but that crashes the JVM back-end, so we
   * use a 'def' instead, which is binary compatible.
   */
  def TYPE: Class[_] = scala.Predef.classOf[scala.Boolean]

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
  @inline def TRUE: Boolean = valueOf(true)
  @inline def FALSE: Boolean = valueOf(false)

  @inline def `new`(value: scala.Boolean): Boolean = valueOf(value)

  @inline def `new`(s: String): Boolean = valueOf(s)

  @inline def valueOf(b: scala.Boolean): Boolean = b.asInstanceOf[Boolean]

  @inline def valueOf(s: String): Boolean = valueOf(parseBoolean(s))

  @inline def parseBoolean(s: String): scala.Boolean =
    (s != null) && s.equalsIgnoreCase("true")

  @inline def toString(b: scala.Boolean): String =
    "" + b

  @inline def compare(x: scala.Boolean, y: scala.Boolean): scala.Int =
    if (x == y) 0 else if (x) 1 else -1
}
