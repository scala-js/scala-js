package java.util

class EventObject(protected var source: AnyRef) extends java.io.Serializable {
  def getSource(): AnyRef = source
}
