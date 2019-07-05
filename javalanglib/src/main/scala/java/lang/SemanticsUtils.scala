/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package java.lang

import scala.scalajs.js
import scala.scalajs.runtime.{linkingInfo, UndefinedBehaviorError}

/** Utilities to test for erroneous conditions depending on the Semantics
 *  configuration.
 */
private[lang] object SemanticsUtils {

  private final val Compliant = 0
  private final val Fatal = 1
  private final val Unchecked = 2

  /** Tests for an erroneous condition governed by the `asInstanceOfs`
   *  semantics.
   */
  @inline
  def asInstanceOfCheck(shouldThrow: js.Function0[scala.Boolean],
      exception: js.Function0[ClassCastException]): Unit = {
    genericCheck(linkingInfo.semantics.asInstanceOfs, shouldThrow, exception)
  }

  /** Tests for an erroneous condition governed by the `arrayIndexOutOfBounds`
   *  semantics.
   */
  @inline
  def arrayIndexOutOfBoundsCheck(shouldThrow: js.Function0[scala.Boolean],
      exception: js.Function0[ArrayIndexOutOfBoundsException]): Unit = {
    genericCheck(linkingInfo.semantics.arrayIndexOutOfBounds, shouldThrow,
        exception)
  }

  @inline
  private def genericCheck(complianceLevel: Int,
      shouldThrow: js.Function0[scala.Boolean],
      exception: js.Function0[Throwable]): Unit = {
    if (complianceLevel != Unchecked && shouldThrow()) {
      if (complianceLevel == Compliant)
        throw exception()
      else
        throw new UndefinedBehaviorError(exception())
    }
  }

}
