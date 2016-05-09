package java.util

class EventObject(@transient protected val source: AnyRef) extends java.io.Serializable {
  private val serialVersionUID: Long = 5516075349620653480L
  def getSource = source
}
