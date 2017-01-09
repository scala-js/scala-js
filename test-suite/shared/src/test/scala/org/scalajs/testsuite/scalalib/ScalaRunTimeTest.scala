package org.scalajs.testsuite.scalalib

import org.junit.Test
import org.junit.Assert.{assertFalse, assertTrue}

import scala.concurrent.Future

class ScalaRunTimeTest {

  @Test def ScalaRunTime_isArray(): Unit = {
    def isScalaArray(x: Any): Boolean = {
      x match {
        case _: Array[_] => true
        case _           => false
      }
    }

    assertTrue(isScalaArray(Array(1, 2, 3)))
    assertFalse(isScalaArray(42))
    assertFalse(isScalaArray(Future.successful(42)))
  }

}
