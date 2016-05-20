package java.util.logging

abstract class Formatter protected () {

  def format(record: LogRecord): String

  def getHead(h: Handler): String = ""

  def getTail(h: Handler): String = ""

  def formatMessage(record: LogRecord): String =
    // The Java spec uses java.text formatting not available in Scala.js
    if (Option(record.getParameters).exists(_.nonEmpty)) {
      // Simple text replacement
      val msg = record.getMessage
      record.getParameters.zipWithIndex.foldRight(msg) { case ((x, i), t) =>
        t.replace(s"{$i}", x.toString)
      }
    } else {
      record.getMessage
    }
}
