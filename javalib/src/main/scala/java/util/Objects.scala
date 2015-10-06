package java.util

import scala.reflect.ClassTag

object Objects {

  @inline
  def equals(a: AnyRef, b: AnyRef): Boolean =
    if (a == null) b == null
    else a.equals(b)

  @inline
  def deepEquals(a: AnyRef, b: AnyRef): Boolean = {
    if (a eq b) true
    else if (a == null || b == null) false
    else {
      (a, b) match {
        case (a1: Array[AnyRef], a2: Array[AnyRef])   => Arrays.deepEquals(a1, a2)
        case (a1: Array[Long], a2: Array[Long])       => Arrays.equals(a1, a2)
        case (a1: Array[Int], a2: Array[Int])         => Arrays.equals(a1, a2)
        case (a1: Array[Short], a2: Array[Short])     => Arrays.equals(a1, a2)
        case (a1: Array[Byte], a2: Array[Byte])       => Arrays.equals(a1, a2)
        case (a1: Array[Char], a2: Array[Char])       => Arrays.equals(a1, a2)
        case (a1: Array[Boolean], a2: Array[Boolean]) => Arrays.equals(a1, a2)
        case (a1: Array[Float], a2: Array[Float])     => Arrays.equals(a1, a2)
        case (a1: Array[Double], a2: Array[Double])   => Arrays.equals(a1, a2)
        case _                                        => a === b
      }
    }
  }

  @inline
  def hashCode(o: AnyRef): Int =
    if (o == null) 0
    else o.hashCode()

  @inline
  def hash(values: Array[AnyRef]): Int =
    Arrays.hashCode(values)

  @inline
  def toString(o: AnyRef): String =
    String.valueOf(o)

  @inline
  def toString(o: AnyRef, nullDefault: String): String =
    if (o == null) nullDefault
    else o.toString

  @inline
  def compare[T](a: T, b: T, c: Comparator[_ >: T]): Int =
    if (a.asInstanceOf[AnyRef] eq b.asInstanceOf[AnyRef]) 0
    else c.compare(a, b)

  @inline
  def requireNonNull[T](obj: T): T =
    if (obj == null) throw new NullPointerException
    else obj

  @inline
  def requireNonNull[T](obj: T, message: String): T =
    if (obj == null) throw new NullPointerException(message)
    else obj

  @inline
  def isNull(obj: AnyRef): Boolean =
    obj == null

  @inline
  def nonNull(obj: AnyRef): Boolean =
    obj != null

  // Requires the implementation of java.util.function
  // @inline
  // def requireNonNull[T](obj: T, messageSupplier: Supplier[String]): T =
  //   if (obj == null) throw new NullPointerException(messageSupplier.get())
  //   else obj
}
