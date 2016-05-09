package java.util

class EventObject(protected val source: AnyRef) extends java.io.Serializable {
  def getSource = source
}
