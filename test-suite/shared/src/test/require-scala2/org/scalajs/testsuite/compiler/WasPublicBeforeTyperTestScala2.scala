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

import org.scalajs.testsuite.utils.AssertThrows._

class WasPublicBeforeTyperTestScala2 {

  /** This is a Scala 2.x-only test because it is a bug compatibility test,
   *  and dotty does not expose the bug.
   */
  @Test def wasPublicBeforeTyperDoesNotApplyToScalaClasses(): Unit = {
    def getObj(): AnyRef = new {
      val x1 = "x1"
      var y1 = "y1"
      def z1() = "z1"
      private val x2 = "x2"
      private var y2 = "y2"
      private def z2() = "z2"
      private[this] val x3 = "x3"
      private[this] var y3 = "y3"
      private[this] def z3() = "z3"
    }

    import scala.language.reflectiveCalls

    val obj2 = getObj().asInstanceOf[{ val x1: String; var y1: String; def z1(): String }]

    assertThrows(classOf[Throwable], obj2.x1)
    assertThrows(classOf[Throwable], obj2.y1)
    assertThrows(classOf[Throwable], obj2.y1 = "y1+")
    assertThrows(classOf[Throwable], obj2.z1)
  }

}
