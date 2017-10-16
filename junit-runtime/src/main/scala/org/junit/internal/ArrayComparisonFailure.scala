/*
 * Ported from https://github.com/junit-team/junit
 */
package org.junit.internal

object ArrayComparisonFailure

/* Attention: AssertionError does not have a (String, Throwable) constructor
 * in JDK 6. If we try to call that one, compilation *succeeds* because of
 * auto-boxing, but it does not do the right thing. See #3154.
 */
class ArrayComparisonFailure(message: String, cause: AssertionError, index: Int)
    extends AssertionError(message) {

  initCause(cause)

  private var fIndices: List[Int] = index :: Nil

  @deprecated("This constructor is not used and will be removed", "0.6.21")
  def this(fMessage: String) =
    this(fMessage, new AssertionError, 0)

  def addDimension(index: Int): Unit = {
    fIndices = index :: fIndices
  }

  override def getMessage(): String = {
    val msg = if (message != null) message else ""
    val indices =
      if (fIndices == null) s"[$index]" // see #3148
      else fIndices.map(index => s"[$index]").mkString
    val causeMessage = cause.getMessage // do not use getCause(), see #3148
    s"${msg}arrays first differed at element $indices; $causeMessage"
  }

  override def toString(): String = getMessage
}
