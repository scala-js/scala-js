package scala.scalajs.runtime

import scala.annotation.switch

import LinkingInfo.Semantics._

/** Utilities to test for erroneous conditions depending on the Semantics
 *  configuration.
 */
object SemanticsUtils {

  @inline
  private def asInstanceOfs: Int =
    linkingInfo.semantics.asInstanceOfs

  /** Tests for an erroneous condition governed by the `asInstanceOfs`
   *  semantics.
   */
  @inline
  def asInstanceOfCheck(shouldThrow: => Boolean,
      exception: => Throwable): Unit = {
    genericCheck(asInstanceOfs, shouldThrow, exception)
  }

  @inline
  private def arrayIndexOutOfBounds: Int =
    linkingInfo.semantics.arrayIndexOutOfBounds

  /** Tests for an erroneous condition governed by the `arrayIndexOutOfBounds`
   *  semantics.
   */
  @inline
  def arrayIndexOutOfBoundsCheck(shouldThrow: => Boolean,
      exception: => Throwable): Unit = {
    genericCheck(arrayIndexOutOfBounds, shouldThrow, exception)
  }

  @inline
  private def genericCheck(complianceLevel: Int, shouldThrow: => Boolean,
      exception: => Throwable): Unit = {
    if (complianceLevel != Unchecked && shouldThrow) {
      if (complianceLevel == Compliant)
        throw exception
      else
        throw new UndefinedBehaviorError(exception)
    }
  }

}
