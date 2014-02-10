package scala.runtime

object BoxesRunTime {
  def boxToBoolean(b: Boolean): java.lang.Boolean = java.lang.Boolean.valueOf(b)
  def boxToCharacter(c: Char): java.lang.Character = java.lang.Character.valueOf(c)
  def boxToByte(b: Byte): java.lang.Byte = java.lang.Byte.valueOf(b)
  def boxToShort(s: Short): java.lang.Short = java.lang.Short.valueOf(s)
  def boxToInteger(i: Int): java.lang.Integer = java.lang.Integer.valueOf(i)
  def boxToLong(l: Long): java.lang.Long = java.lang.Long.valueOf(l)
  def boxToFloat(f: Float): java.lang.Float = java.lang.Float.valueOf(f)
  def boxToDouble(d: Double): java.lang.Double = java.lang.Double.valueOf(d)

  def unboxToBoolean(b: Object): Boolean = if (b eq null) false else b.asInstanceOf[java.lang.Boolean].booleanValue()
  def unboxToChar(c: Object): Char = if (c eq null) 0 else c.asInstanceOf[java.lang.Character].charValue()
  def unboxToByte(b: Object): Byte = if (b eq null) 0 else b.asInstanceOf[java.lang.Byte].byteValue()
  def unboxToShort(s: Object): Short = if (s eq null) 0 else s.asInstanceOf[java.lang.Short].shortValue()
  def unboxToInt(i: Object): Int = if (i eq null) 0 else i.asInstanceOf[java.lang.Integer].intValue()
  def unboxToLong(l: Object): Long = if (l eq null) 0 else l.asInstanceOf[java.lang.Long].longValue()
  def unboxToFloat(f: Object): Float = if (f eq null) 0 else f.asInstanceOf[java.lang.Float].floatValue()
  def unboxToDouble(d: Object): Double = if (d eq null) 0 else d.asInstanceOf[java.lang.Double].doubleValue()

  def equals(x: Object, y: Object): Boolean = if (x eq y) true else equals2(x,y)

  def equals2(x: Object, y: Object): Boolean = {
    x match {
      case xn: java.lang.Number => equalsNumObject(xn, y)
      case xc: java.lang.Character => equalsCharObject(xc, y)
      case x => if (x eq null) (y eq null) else x.equals(y)
    }
  }

  def equalsNumObject(xn: java.lang.Number, y: Object): Boolean = {
    y match {
      case yn: java.lang.Number => equalsNumNum(xn, yn)
      case yc: java.lang.Character => equalsNumChar(xn, yc)
      case y => if (xn eq null) (y eq null) else xn.equals(y)
    }
  }

  private object Codes {
    val CHAR = 0
    val BYTE = 1
    val SHORT = 2
    val INT = 3
    val LONG = 4
    val FLOAT = 5
    val DOUBLE = 6
    val OTHER = 7
  }

  private def typeCode(a: Object): Int = {
    import Codes._
    a match {
      case _:java.lang.Integer => INT
      case _:java.lang.Byte => BYTE
      case _:java.lang.Character => CHAR
      case _:java.lang.Long => LONG
      case _:java.lang.Double => DOUBLE
      case _:java.lang.Short => SHORT
      case _:java.lang.Float => FLOAT
      case _ => OTHER
    }
  }

  private def eqTypeCode(a: java.lang.Number): Int = {
    import Codes._
    a match {
      case _:java.lang.Integer => INT
      case _:java.lang.Byte => INT
      case _:java.lang.Long => LONG
      case _:java.lang.Double => DOUBLE
      case _:java.lang.Short => INT
      case _:java.lang.Float => FLOAT
      case _ => OTHER
    }
  }

  def equalsNumNum(xn: java.lang.Number, yn: java.lang.Number): Boolean = {
    import scala.math.ScalaNumber
    import Codes._
    val xcode = eqTypeCode(xn)
    val ycode = eqTypeCode(yn)
    val dcode = if (ycode > xcode) { ycode } else { xcode }
    dcode match {
      case c if c == INT => xn.intValue() == yn.intValue()
      case c if c == LONG => xn.longValue() == yn.longValue()
      case c if c == FLOAT => xn.floatValue() == yn.floatValue()
      case c if c == DOUBLE => xn.doubleValue() == yn.doubleValue()
      case _ => {
        if (yn.isInstanceOf[ScalaNumber] && (!xn.isInstanceOf[ScalaNumber])) {
          yn.equals(xn)
        } else if (xn eq null) {
          yn eq null
        } else {
          xn.equals(yn)
        }
      }
    }
  }

  def equalsCharObject(xc: java.lang.Character, y: Object): Boolean = {
    y match {
      case yc:java.lang.Character => xc.charValue() == yc.charValue()
      case yn:java.lang.Number => equalsNumChar(yn, xc)
      case _ => {
        if (xc eq null)
          y eq null
        else
          xc.equals(y)
      }
    }
  }

  private def equalsNumChar(xn: java.lang.Number, yc: java.lang.Character): Boolean = {
    import Codes._
    val ch = yc.charValue()
    eqTypeCode(xn) match {
      case c if c == INT => xn.intValue() == ch
      case c if c == LONG => xn.longValue() == ch
      case c if c == FLOAT => xn.floatValue() == ch
      case c if c == DOUBLE => xn.doubleValue() == ch
      case _ => {
        if (xn eq null)
          yc eq null
        else
          xn.equals(yc)
      }
    }
  }

  def hashFromLong(n: java.lang.Long): Int = {
    val iv = n.intValue()
    if (iv == n.longValue()) iv
    else n.hashCode()
  }

  def hashFromDouble(n: java.lang.Double): Int = {
    val iv = n.intValue()
    val dv = n.doubleValue()
    val lv = n.longValue()

    if (iv == dv) iv
    else if (lv == dv) java.lang.Long.valueOf(lv).hashCode()
    else n.hashCode()
  }

  def hashFromFloat(n: java.lang.Float): Int = {
    val iv = n.intValue()
    val fv = n.floatValue()
    val lv = n.longValue()
    if (iv == fv) iv
    else if (lv == fv) java.lang.Long.valueOf(lv).hashCode()
    else n.hashCode()
  }

  def hashFromNumber(n: java.lang.Number): Int = {
    n match {
      case l:java.lang.Long => hashFromLong(l)
      case d:java.lang.Double => hashFromDouble(d)
      case f:java.lang.Float => hashFromFloat(f)
      case n => n.hashCode()
    }
  }

  def hashFromObject(a: Object): Int = {
    a match {
      case n:java.lang.Number => hashFromNumber(n)
      case a => a.hashCode()
    }
  }
}
