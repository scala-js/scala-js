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

package org.scalajs.testsuite.jsinterop

import org.junit.Test
import org.junit.Assert._

import scala.scalajs.js
import scala.scalajs.js.annotation._

/** Additional tests for non-native JS classes that have to be in a
 *  separate codebase than testSuite to be meaningful.
 *
 *  If moved to testSuite, those tests "fail to fail" due to mass effects
 *  produced by the immensity of the testSuite codebase.
 */
class NonNativeJSTypeTestEx {

  @Test def constructor_property_on_the_prototype_issue_1963(): Unit = {
    class ParentClass extends js.Object

    class ChildClass extends ParentClass

    val child = new ChildClass().asInstanceOf[js.Dynamic]
    assertSame(js.constructorOf[ChildClass], child.constructor)
  }
}
