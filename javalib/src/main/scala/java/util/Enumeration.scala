package java.util

trait Enumeration[E] {
  def hasMoreElements(): Boolean
  def nextElement(): E
}
