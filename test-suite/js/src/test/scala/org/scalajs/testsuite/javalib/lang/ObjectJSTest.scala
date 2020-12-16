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

import org.junit.Test
import org.junit.Assert._

import org.scalajs.testsuite.utils.AssertThrows._

import scala.scalajs.js

// scalastyle:off disallow.space.before.token

class ObjectJSTest {

  @Test def everythingButNullIsAnObject(): Unit = {
    assertTrue((new js.Object: Any).isInstanceOf[Object])
    assertTrue((js.Array(5)  : Any).isInstanceOf[Object])
  }

  @Test def everythingCanCastToObjectSuccessfullyIncludingNull(): Unit = {
    (new js.Object: Any).asInstanceOf[Object]
    (js.Array(5)  : Any).asInstanceOf[Object]
  }

  @Test def cloneOnNonScalaObject(): Unit = {
    class CloneOnNonScalaObject extends js.Object {
      def boom(): Any = this.clone()
    }

    val obj = new CloneOnNonScalaObject()
    assertThrows(classOf[CloneNotSupportedException], obj.boom())
  }
}
