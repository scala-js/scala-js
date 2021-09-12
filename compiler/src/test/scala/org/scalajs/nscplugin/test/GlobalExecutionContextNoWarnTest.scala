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

package org.scalajs.nscplugin.test

import org.scalajs.nscplugin.test.util._
import org.scalajs.nscplugin.test.util.VersionDependentUtils.scalaSupportsNoWarn

import org.junit.Assume._
import org.junit.Test

class GlobalExecutionContextNoWarnTest extends DirectTest with TestHelpers {

  override def extraArgs: List[String] =
    super.extraArgs ::: List("-P:scalajs:nowarnGlobalExecutionContext")

  @Test
  def noWarnOnUsage: Unit = {
    """
    import scala.concurrent.ExecutionContext.global

    object Enclosing {
      global
    }
    """.hasNoWarns()
  }

  @Test
  def noWarnOnImplicitUsage: Unit = {
    """
    import scala.concurrent.ExecutionContext.Implicits.global

    object Enclosing {
      scala.concurrent.Future { }
    }
    """.hasNoWarns()
  }
}
