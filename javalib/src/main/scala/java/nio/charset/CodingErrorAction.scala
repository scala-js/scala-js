package java.nio.charset

class CodingErrorAction private (name: String) {
  override def toString(): String = name
}

object CodingErrorAction {
  val IGNORE = new CodingErrorAction("IGNORE")
  val REPLACE = new CodingErrorAction("REPLACE")
  val REPORT = new CodingErrorAction("REPORT")
}
