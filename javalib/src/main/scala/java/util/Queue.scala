package java.util

trait Queue[E] extends Collection[E] {
  def add(e: E): Boolean
  def offer(e: E): Boolean
  def remove(): E
  def poll(): E
  def element(): E
  def peek(): E
}
