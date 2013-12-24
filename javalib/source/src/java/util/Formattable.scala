package java.util

trait Formattable {
  def formatTo(formatter: Formatter, flags: Int, width: Int, precision: Int): Unit
}
