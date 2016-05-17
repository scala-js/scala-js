package java.util.logging

object ErrorManager {
  val GENERIC_FAILURE = 0
  val WRITE_FAILURE   = 1
  val FLUSH_FAILURE   = 2
  val CLOSE_FAILURE   = 3
  val OPEN_FAILURE    = 4
  val FORMAT_FAILURE  = 5
}

class ErrorManager() {
  // The spec is a bit vague. This will implement the most
  // obvious interpretation of outputting only once
  private[this] var called = false

  def error(msg: String, ex: Exception, code: Int): Unit = {
    if (!called) {
      System.err.println(msg)
      ex.printStackTrace(System.err)
      called = true
    }
  }

}
