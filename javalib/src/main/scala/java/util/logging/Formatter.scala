package java.util.logging

abstract class Formatter protected () {

  def format(record: LogRecord): String

  def getHead(h: Handler): String = ""

  def getTail(h: Handler): String = ""

  def formatMessage(record: LogRecord): String =
    // The Java spec uses java.text formatting not available in Scala.js
    // Default to no formatting
    record.getMessage
}
