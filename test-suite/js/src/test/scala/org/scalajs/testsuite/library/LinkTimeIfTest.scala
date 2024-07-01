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

package org.scalajs.testsuite.library

import scala.scalajs.LinkingInfo._

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._

import org.scalajs.testsuite.utils.Platform

class LinkTimeIfTest {
  @Test def linkTimeIfConst(): Unit = {
    // boolean const
    linkTimeIf(true) { /* ok */ } { fail() }
    linkTimeIf(false) { fail() } { /* ok */ }
  }

  @Test def linkTimeIfProp(): Unit = {
    locally {
      val cond = Platform.isInProductionMode
      linkTimeIf(productionMode) {
        assertTrue(cond)
      } {
        assertFalse(cond)
      }
    }

    locally {
      val cond = !Platform.isInProductionMode
      linkTimeIf(!productionMode) {
        assertTrue(cond)
      } {
        assertFalse(cond)
      }
    }
  }

  @Test def linkTimIfIntProp(): Unit = {
    locally {
      val cond = Platform.assumedESVersion >= ESVersion.ES2015
      linkTimeIf(esVersion >= ESVersion.ES2015) {
        assertTrue(cond)
      } {
        assertFalse(cond)
      }
    }

    locally {
      val cond = !(Platform.assumedESVersion < ESVersion.ES2015)
      linkTimeIf(!(esVersion < ESVersion.ES2015)) {
        assertTrue(cond)
      } {
        assertFalse(cond)
      }
    }
  }

  @Test def linkTimeIfNested(): Unit = {
    locally {
      val cond =
        Platform.isInProductionMode &&
        Platform.assumedESVersion >= ESVersion.ES2015
      linkTimeIf(productionMode && esVersion >= ESVersion.ES2015) {
        assertTrue(cond)
      } {
        assertFalse(cond)
      }
    }

    locally {
      val cond =
        Platform.assumedESVersion >= ESVersion.ES2015 &&
        Platform.assumedESVersion < ESVersion.ES2019 &&
        Platform.isInProductionMode

      linkTimeIf(esVersion >= ESVersion.ES2015 &&
          esVersion < ESVersion.ES2019 && productionMode) {
        assertTrue(cond)
      } {
        assertFalse(cond)
      }
    }
  }
}
