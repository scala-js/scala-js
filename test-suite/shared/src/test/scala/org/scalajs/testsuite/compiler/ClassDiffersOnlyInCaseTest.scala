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

package org.scalajs.testsuite.compiler

import org.junit.Test
import org.junit.Assert._

class ClassDiffersOnlyInCaseTest {
  @Test
  def testClassesThatDifferOnlyInCase_Issue4855(): Unit = {
    val fromMain = new ClassdiffersOnlyinCase()
    assertEquals("main", fromMain.provenance)

    val fromTest = new ClassDiffersOnlyIncase()
    assertEquals("test", fromTest.provenance)
  }
}
