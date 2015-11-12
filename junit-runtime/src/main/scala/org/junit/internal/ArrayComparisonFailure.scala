/*
 * Ported from https://github.com/junit-team/junit
 */
package org.junit.internal

object ArrayComparisonFailure

class ArrayComparisonFailure(fMessage: String) extends AssertionError {
  private var fIndices: List[Int] = Nil

  def this(message: String, cause: AssertionError, index: Int) = {
    this(message)
    initCause(cause)
    addDimension(index)
  }

  def addDimension(index: Int): Unit = {
    fIndices = index :: fIndices
  }

  override def getMessage(): String = {
    val message = if (fMessage != null) fMessage else ""
    val indices = fIndices.map(index => s"[$index]").mkString
    val causeMessage = getCause.getMessage
    s"${message}arrays first differed at element $indices; $causeMessage"
  }

  override def toString(): String = getMessage
}
