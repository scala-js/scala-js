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
    val str = js
      .constructorOf[js.Object]
      .prototype
      .selectDynamic("toString")
      .call(t.asInstanceOf[js.Any])
    assertEquals("[object Error]", str)
  }

}
