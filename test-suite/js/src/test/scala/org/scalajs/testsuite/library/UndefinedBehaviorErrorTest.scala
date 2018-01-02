/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2017, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.library

import scala.util.control.NonFatal

import org.junit.Assert._
import org.junit.Test

import scala.scalajs.runtime.UndefinedBehaviorError

class UndefinedBehaviorErrorTest {

  @Test def ubeIsAFatalError(): Unit = {
    val error: Throwable = new UndefinedBehaviorError("test")
    assertFalse(NonFatal(error))
    assertTrue(error match {
      case NonFatal(_) => false
      case _           => true
    })
  }

}
