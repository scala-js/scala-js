package java.util

class EventObject(protected var source: AnyRef) {
  def getSource(): AnyRef = source

  override def toString(): String =
    s"${getClass.getSimpleName}[source=$source]"
}
