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
    assertEquals(1, linkTimeIf(true)(1)(2))
    assertEquals(2, linkTimeIf(false)(1)(2))
  }

  @Test def linkTimeIfProp(): Unit = {
    locally {
      val cond = Platform.isInProductionMode
      assertEquals(cond, linkTimeIf(productionMode)(true)(false))
    }

    locally {
      val cond = !Platform.isInProductionMode
      assertEquals(cond, linkTimeIf(!productionMode)(true)(false))
    }
  }

  @Test def linkTimIfIntProp(): Unit = {
    locally {
      val cond = Platform.assumedESVersion >= ESVersion.ES2015
      assertEquals(cond, linkTimeIf(esVersion >= ESVersion.ES2015)(true)(false))
    }

    locally {
      val cond = !(Platform.assumedESVersion < ESVersion.ES2015)
      assertEquals(cond, linkTimeIf(!(esVersion < ESVersion.ES2015))(true)(false))
    }
  }

  @Test def linkTimeIfNested(): Unit = {
    locally {
      val cond = {
        Platform.isInProductionMode &&
        Platform.assumedESVersion >= ESVersion.ES2015
      }
      assertEquals(if (cond) 53 else 78,
          linkTimeIf(productionMode && esVersion >= ESVersion.ES2015)(53)(78))
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
        assertTrue("Took the wrong branch of linkTimeIf when linking for ES 2016+",
            esVersion >= ESVersion.ES2016)
        (x.asInstanceOf[js.Dynamic] ** y.asInstanceOf[js.Dynamic]).asInstanceOf[Double]
      } {
        assertFalse("Took the wrong branch of linkTimeIf when linking for ES 2015-",
            esVersion >= ESVersion.ES2016)
        Math.pow(x, y)
      }
    }
    assertEquals(pow(2.0, 8.0), 256.0, 0)
  }

  // This test verifies that the compiler can safely compile surrounding
  // implicit asInstanceOf casts to a supertype.
  @Test def subtyping(): Unit = {
    trait A { def value: Int }
    class B(val value: Int) extends A
    class C(val value: Int) extends A

    val b = new B(1)
    val c = new C(2)

    val result1 = linkTimeIf[A](productionMode)(b)(c)
    assertEquals(if (Platform.isInProductionMode) b.value else c.value, result1.value)

    val result2 = linkTimeIf[A](productionMode)(c)(b)
    assertEquals(if (Platform.isInProductionMode) c.value else b.value, result2.value)
  }

  @Test def implPattern(): Unit = {
    import LinkTimeIfTest._
    val impl = linkTimeIf[ArrayImpl](productionMode) {
      JSArrayImpl
    } {
      ScalaArrayImpl
    }
    assertEquals(0, impl.length(impl.empty()))
  }
}

object LinkTimeIfTest {
  sealed private abstract class ArrayImpl {
    type Repr
    def empty(): Repr
    def length(v: Repr): Int
  }

  private object JSArrayImpl extends ArrayImpl {
    type Repr = js.Array[AnyRef]
    def empty(): Repr = js.Array[AnyRef]()
    def length(v: Repr): Int = v.length
  }

  private object ScalaArrayImpl extends ArrayImpl {
    type Repr = Array[AnyRef]
    def empty(): Repr = new Array[AnyRef](0)
    def length(v: Repr): Int = v.length
  }
}
