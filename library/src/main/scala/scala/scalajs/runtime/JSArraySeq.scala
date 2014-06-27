package scala.scalajs.runtime

import scala.scalajs.js

/** A simple wrapper Seq for a native JS array. Is used by JSExports
  * for repeated parameter lists. The map function is used for boxing
  */
@deprecated("Only exists for binary compat. Use WrappedArray instead.", "0.5.1")
class JSArraySeq[A](
  private val arr: js.Array[A],
  private val offset: Int) extends Seq[A] {

  def apply(i: Int): A = arr(i+offset)
  def iterator: Iterator[A] = new JSArraySeqIterator
  def length: Int = (arr.length - offset).toInt

  class JSArraySeqIterator extends Iterator[A] {
    private[this] var nextIndex: Int = 0
    def next() = {
      val elem = JSArraySeq.this.apply(nextIndex)
      nextIndex += 1
      elem
    }

    def hasNext = nextIndex < JSArraySeq.this.length
  }

}
