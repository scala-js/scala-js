/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013--2018, LAMP/EPFL  **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */

package org.scalajs.testsuite.javalib.lang

import scala.scalajs.js
import scala.scalajs.LinkingInfo.assumingES6

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._

class ThrowableJSTest {

  @Test def throwablesAreJSErrors(): Unit = {
    val t: Any = new Throwable("foo")
    assertTrue(t.isInstanceOf[js.Error])
  }

  @Test def throwablesAreTrueErrors(): Unit = {
    assumeTrue("Requires ECMAScript 2015", assumingES6)

    val t: Any = new Throwable("foo")
    val str = js.constructorOf[js.Object].prototype
      .selectDynamic("toString")
      .call(t.asInstanceOf[js.Any])
    assertEquals("[object Error]", str)
  }

}
