package java.lang

class Boolean(value: scala.Boolean) {
  def booleanValue(): scala.Boolean = value

  override def toString: String = if (value) "true" else "false"

  override def hashCode = if (value) 1231 else 1237
}

object Boolean {
  val TYPE = null
  val TRUE = new Boolean(true)
  val FALSE = new Boolean(false)

  def valueOf(booleanValue: scala.Boolean) = if (booleanValue) TRUE else FALSE

  def toString(b: scala.Boolean) = if (b) "true" else "false"
}
