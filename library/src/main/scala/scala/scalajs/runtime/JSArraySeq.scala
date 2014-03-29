package scala.scalajs.runtime

import scala.scalajs.js

/** A simple wrapper Seq for a native JS array. Is used by JSExports
  * for repeated parameter lists. The map function is used for boxing
  */
class JSArraySeq[A,B](
  private val arr: js.Array[A],
  private val offset: Int,
  private val map: js.Function1[A,B]) extends Seq[B] {

  def apply(i: Int): B = map(arr(i+offset))
  def iterator: Iterator[B] = new JSArraySeqIterator
  def length: Int = (arr.length - offset).toInt

  class JSArraySeqIterator extends Iterator[B] {
    private[this] var nextIndex: Int = 0
    def next() = {
      val elem = JSArraySeq.this.apply(nextIndex)
      nextIndex += 1
      elem
    }

    def hasNext = nextIndex < JSArraySeq.this.length
  }

}
