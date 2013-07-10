package java.lang

object String {
  def valueOf(value: scala.Boolean) = new java.lang.Boolean(value).toString()
  def valueOf(value: scala.Char) = new java.lang.Character(value).toString()
  def valueOf(value: scala.Byte) = new java.lang.Byte(value).toString()
  def valueOf(value: scala.Short) = new java.lang.Short(value).toString()
  def valueOf(value: scala.Int) = new java.lang.Integer(value).toString()
  def valueOf(value: scala.Long) = new java.lang.Long(value).toString()
  def valueOf(value: scala.Float) = new java.lang.Float(value).toString()
  def valueOf(value: scala.Double) = new java.lang.Double(value).toString()
  def valueOf(value: java.lang.Object) = value.toString()
}
