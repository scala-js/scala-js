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

import scala.scalajs.js
import scala.scalajs.LinkingInfo._

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._

import org.scalajs.testsuite.utils.Platform

class LinkTimeIfTest {
  @Test def linkTimeIfConst(): Unit = {
    // boolean const
    assertEquals(1, linkTimeIf(true) { 1 } { 2 })
    assertEquals(2, linkTimeIf(false) { 1 } { 2 })
  }

  @Test def linkTimeIfProp(): Unit = {
    locally {
      val cond = Platform.isInProductionMode
      assertEquals(cond, linkTimeIf(productionMode) { true } { false })
    }

    locally {
      val cond = !Platform.isInProductionMode
      assertEquals(cond, linkTimeIf(!productionMode) { true } { false })
    }
  }

  @Test def linkTimIfIntProp(): Unit = {
    locally {
      val cond = Platform.assumedESVersion >= ESVersion.ES2015
      assertEquals(cond, linkTimeIf(esVersion >= ESVersion.ES2015) { true } { false })
    }

    locally {
      val cond = !(Platform.assumedESVersion < ESVersion.ES2015)
      assertEquals(cond, linkTimeIf(!(esVersion < ESVersion.ES2015)) { true } { false })
    }
  }

  @Test def linkTimeIfNested(): Unit = {
    locally {
      val cond = {
        Platform.isInProductionMode &&
        Platform.assumedESVersion >= ESVersion.ES2015
      }
      assertEquals(if (cond) 53 else 78,
          linkTimeIf(productionMode && esVersion >= ESVersion.ES2015) { 53 } { 78 })
    }

    locally {
      val cond = {
        Platform.assumedESVersion >= ESVersion.ES2015 &&
        Platform.assumedESVersion < ESVersion.ES2019 &&
        Platform.isInProductionMode
      }
      val result = linkTimeIf(esVersion >= ESVersion.ES2015 &&
          esVersion < ESVersion.ES2019 && productionMode) {
        53
      } {
        78
      }
      assertEquals(if (cond) 53 else 78, result)
    }
  }

  @Test def exponentOp(): Unit = {
    def pow(x: Double, y: Double): Double = {
      linkTimeIf(esVersion >= ESVersion.ES2016) {
        (x.asInstanceOf[js.Dynamic] ** y.asInstanceOf[js.Dynamic]).asInstanceOf[Double]
      } {
        Math.pow(x, y)
      }
    }
    assertEquals(pow(2.0, 8.0), 256.0, 0)
  }
}
