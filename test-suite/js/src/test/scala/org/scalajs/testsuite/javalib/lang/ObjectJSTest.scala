/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.lang

import org.junit.Test
import org.junit.Assert._

import scala.scalajs.js

// scalastyle:off disallow.space.before.token

class ObjectJSTest {

  @Test def everything_but_null_should_be_an_Object(): Unit = {
    assertTrue((new js.Object: Any).isInstanceOf[Object])
    assertTrue((js.Array(5)  : Any).isInstanceOf[Object])
  }

  @Test def everything_should_cast_to_Object_successfully_including_null(): Unit = {
    (new js.Object: Any).asInstanceOf[Object]
    (js.Array(5)  : Any).asInstanceOf[Object]
  }
}
