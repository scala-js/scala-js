package java.io

trait Appendable {
  def append(c: Char): Appendable
  def append(csq: CharSequence): Appendable
  def append(csq: CharSequence, start: Int, end: Int): Appendable
}
