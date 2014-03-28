package scala.scalajs.sbtplugin.testing

/** Dummy Exception to wrap stack traces passed to SBT */
class TestException(
  message: String, 
  stackTrace: Array[StackTraceElement]
) extends Exception(message) {
  override def getStackTrace = stackTrace
}
