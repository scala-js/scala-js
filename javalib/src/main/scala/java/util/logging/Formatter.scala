package java.util.logging

abstract class Formatter protected () {

  def format(record: LogRecord): String

  def getHead(h: Handler): String = ""

  def getTail(h: Handler): String = ""

  def formatMessage(record: LogRecord): String = {
    val msg = record.getMessage
    val params = record.getParameters

    if (params != null && params.length > 0) {
      // The Java spec uses java.text formatting not available in Scala.js
      // Instead we'll do simple text replacement, very imperative
      var msgAccumulator = new StringBuilder()
      var inParam = false
      var paramInFlight:StringBuilder = null
      var substitutionFailure = false // track failure to break the loop
      var i = 0

      // Do one run over msg keeping track if a param needs replacement
      while (i < msg.length && !substitutionFailure) {
        val currentChar = msg.charAt(i)
        i = i + 1
        if (currentChar == '{' && !inParam) {
          // Beginning of param
          inParam = true
          paramInFlight = new StringBuilder()
        } else if (inParam && currentChar != '}') {
          // accumulate the param
          paramInFlight += currentChar
        } else if (currentChar == '}') {
          // end of param, replace placeholder by value if possible
          inParam = false
          val (failed, replacement) =
            try {
              val index = paramInFlight.toInt
              if (index >= 0 && index < params.length) {
                (false, params(index).toString)
              } else if (index > 0) {
                (false, "{" + index + "}")
              } else {
                // Negative indexes break substitution on the JVM
                (true, "")
              }
            } catch {
              case e: Exception =>
                // The JVM will halt replacing if it cannot parse one param
                (true, "")
            }

          // The JVM will fail if e.g. there are bogus params and would not replace
          // any parameter
          if (failed) substitutionFailure = failed
          else msgAccumulator ++= replacement
        } else {
          msgAccumulator += currentChar
        }
      }
      if (substitutionFailure || inParam) msg
      else msgAccumulator.result()
    } else {
      msg
    }
  }
}
