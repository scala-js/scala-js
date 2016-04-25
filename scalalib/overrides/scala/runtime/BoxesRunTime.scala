package scala.runtime

import scala.math.ScalaNumber

object BoxesRunTime {
  def boxToCharacter(c: Char): java.lang.Character =
    java.lang.Character.valueOf(c)

  @inline
  def unboxToChar(c: Object): Char =
    if (c eq null) 0
    else c.asInstanceOf[java.lang.Character].charValue()

  def equals(x: Object, y: Object): Boolean =
    if (x eq y) true
    else equals2(x, y)

  @inline // only called by equals(), not by codegen
  def equals2(x: Object, y: Object): Boolean = {
    x match {
      case xn: java.lang.Number    => equalsNumObject(xn, y)
      case xc: java.lang.Character => equalsCharObject(xc, y)
      case null                    => y eq null
      case _                       => x.equals(y)
    }
  }

  def equalsNumObject(xn: java.lang.Number, y: Object): Boolean = {
    y match {
      case yn: java.lang.Number    => equalsNumNum(xn, yn)
      case yc: java.lang.Character => equalsNumChar(xn, yc)
      case _ =>
        if (xn eq null)
          y eq null
        else
          xn.equals(y)
    }
  }

  def equalsNumNum(xn: java.lang.Number, yn: java.lang.Number): Boolean = {
    (xn: Any) match {
      case xn: Double =>
        (yn: Any) match {
          case yn: Double      => xn == yn
          case yn: Long        => xn == yn
          case yn: ScalaNumber => yn.equals(xn) // xn is not a ScalaNumber
          case _               => false         // xn.equals(yn) must be false here
        }
      case xn: Long =>
        (yn: Any) match {
          case yn: Long        => xn == yn
          case yn: Double      => xn == yn
          case yn: ScalaNumber => yn.equals(xn) // xn is not a ScalaNumber
          case _               => false         // xn.equals(yn) must be false here
        }
      case null => yn eq null
      case _    => xn.equals(yn)
    }
  }

  def equalsCharObject(xc: java.lang.Character, y: Object): Boolean = {
    y match {
      case yc: java.lang.Character => xc.charValue() == yc.charValue()
      case yn: java.lang.Number    => equalsNumChar(yn, xc)
      case _ =>
        if (xc eq null)
          y eq null
        else
          false // xc.equals(y) must be false here, because y is not a Char
    }
  }

  @inline
  private def equalsNumChar(xn: java.lang.Number, yc: java.lang.Character): Boolean = {
    (xn: Any) match {
      case xn: Double => xn == yc.charValue()
      case xn: Long   => xn == yc.charValue()
      case _ =>
        if (xn eq null) yc eq null
        else xn.equals(yc)
    }
  }

  @inline
  def hashFromLong(n: java.lang.Long): Int =
    Statics.longHash(n.asInstanceOf[Long])

  @inline
  def hashFromDouble(n: java.lang.Double): Int =
    Statics.doubleHash(n.asInstanceOf[Double])

  @inline
  def hashFromFloat(n: java.lang.Float): Int =
    Statics.floatHash(n.asInstanceOf[Float])

  @inline // called only by ScalaRunTime.hash()
  def hashFromNumber(n: java.lang.Number): Int = {
    (n: Any) match {
      case n: Double => Statics.doubleHash(n)
      case n: Long   => Statics.longHash(n)
      case n         => n.hashCode()
    }
  }

  @inline
  def hashFromObject(a: Object): Int =
    Statics.anyHash(a)
}
