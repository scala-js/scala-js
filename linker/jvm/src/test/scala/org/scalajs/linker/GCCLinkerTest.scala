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

package org.scalajs.linker

import org.junit.Test

import org.scalajs.junit.async._

import org.scalajs.linker.interface.StandardConfig

import org.scalajs.linker.testutils.LinkingUtils._

class GCCLinkerTest {
  import scala.concurrent.ExecutionContext.Implicits.global

  @Test
  def linkEmpty(): AsyncResult = await {
    /* Check a degenerate case where there are not public modules at all.
     * See the special check on ModuleSplitter for details.
     */
    testLink(Nil, Nil, config = StandardConfig().withClosureCompiler(true))
  }
}
