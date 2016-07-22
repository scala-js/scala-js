/* !!!!!
 * THIS FILE IS ALMOST COPY-PASTED IN javalanglib/ AND javalib/.
 * THEY MUST BE KEPT IN SYNC.
 *
 * This file acts as a bridge for java.lang.Math to be able to access the
 * additional methods introduced in JDK 8 from javalib/ even when compiling
 * under JDK 6 or 7.
 *
 * In javalib/, this file has only the signatures, with stub implementations.
 * In javalanglib/, the methods delegate to the corresponding methods in
 * java.lang.Math.
 * The build only keeps the .sjsir file from javalanglib/.
 * This way, we create a bridge between javalanglib and javalib. But that
 * means the binary interface of MathJDK8Bridge must be strictly equivalent in
 * the two copies.
 *
 * (Yes, this is a hack.)
 * !!!!!
 */

package java.lang

private[java] object MathJDK8Bridge {
  def addExact(a: scala.Int, b: scala.Int): scala.Int = Math.addExact(a, b)

  def addExact(a: scala.Long, b: scala.Long): scala.Long = Math.addExact(a, b)

  def subtractExact(a: scala.Int, b: scala.Int): scala.Int =
    Math.subtractExact(a, b)

  def subtractExact(a: scala.Long, b: scala.Long): scala.Long =
    Math.subtractExact(a, b)

  def multiplyExact(a: scala.Int, b: scala.Int): scala.Int =
    Math.multiplyExact(a, b)

  def multiplyExact(a: scala.Long, b: scala.Long): scala.Long =
    Math.multiplyExact(a, b)

  def incrementExact(a: scala.Int): scala.Int = Math.incrementExact(a)

  def incrementExact(a: scala.Long): scala.Long = Math.incrementExact(a)

  def decrementExact(a: scala.Int): scala.Int = Math.decrementExact(a)

  def decrementExact(a: scala.Long): scala.Long = Math.decrementExact(a)

  def negateExact(a: scala.Int): scala.Int = Math.negateExact(a)

  def negateExact(a: scala.Long): scala.Long = Math.negateExact(a)

  def toIntExact(a: scala.Long): scala.Int = Math.toIntExact(a)

  def floorDiv(a: scala.Int, b: scala.Int): scala.Int = Math.floorDiv(a, b)

  def floorDiv(a: scala.Long, b: scala.Long): scala.Long = Math.floorDiv(a, b)

  def floorMod(a: scala.Int, b: scala.Int): scala.Int = Math.floorMod(a, b)

  def floorMod(a: scala.Long, b: scala.Long): scala.Long = Math.floorMod(a, b)

  // A few other Math-related methods that are not really in Math

  def toUnsignedString(i: scala.Int): String =
    Integer.toUnsignedString(i)

  def toUnsignedString(i: scala.Long): String =
    Long.toUnsignedString(i)

  def divideUnsigned(a: scala.Int, b: scala.Int): scala.Int =
    Integer.divideUnsigned(a, b)

  def divideUnsigned(a: scala.Long, b: scala.Long): scala.Long =
    Long.divideUnsigned(a, b)

  def remainderUnsigned(a: scala.Int, b: scala.Int): scala.Int =
    Integer.remainderUnsigned(a, b)

  def remainderUnsigned(a: scala.Long, b: scala.Long): scala.Long =
    Long.remainderUnsigned(a, b)
}
