package java.lang

class Character(value: scala.Char) {
  def charValue(): scala.Char = value

  protected def scala_==(other: Object): scala.Boolean = equals(other)
  /*other match {
    case chr:Character =>
      this.charValue == chr.charValue

    case num:Number if num.isInt =>
      this.charValue.toInt == num.intValue

    case _ => equals(other)
  }*/

  @native override def toString: String = sys.error("stub")
}

object Character {
  val TYPE = null
  val MIN_VALUE: scala.Char = 0
  val MAX_VALUE: scala.Char = 0xff

  def valueOf(charValue: scala.Char) = new Character(charValue)

  def toString(c: scala.Char) = valueOf(c).toString
}
