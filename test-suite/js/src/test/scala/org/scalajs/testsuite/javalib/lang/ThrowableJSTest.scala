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

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._

import org.scalajs.testsuite.utils.Platform._

class ThrowableJSTest {

  @Test def throwablesAreJSErrors(): Unit = {
    assumeFalse("Not supported on WebAssembly", executingInWebAssembly)

    val t: Any = new Throwable("foo")
    assertTrue(t.isInstanceOf[js.Error])
  }

  @Test def throwablesAreTrueErrors(): Unit = {
    assumeFalse("Not supported on WebAssembly", executingInWebAssembly)
    assumeTrue("Requires ECMAScript 2015 semantics", useECMAScript2015Semantics)

    def coreToString(x: Any): String = {
      js.constructorOf[js.Object].prototype
        .selectDynamic("toString")
        .call(x.asInstanceOf[js.Any])
        .asInstanceOf[String]
    }

    assertEquals("[object Error]", coreToString(new Throwable("foo")))
    assertEquals("[object Error]",
        coreToString(new IllegalArgumentException("foo")))
  }

}
