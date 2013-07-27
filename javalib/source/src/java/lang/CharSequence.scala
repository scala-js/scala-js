package java.lang

trait CharSequence {
  def length(): scala.Int
  def charAt(index: scala.Int): scala.Char
  def subSequence(start: scala.Int, end: scala.Int): CharSequence
  def toString(): String
}
