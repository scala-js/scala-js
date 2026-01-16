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

import scala.collection.mutable

import scala.scalajs.js
import scala.scalajs.js.annotation._

import org.junit.Assert._
import org.junit.Test

class NonNativeJSTypeTestScala2 {
  import NonNativeJSTypeTestScala2._

  /** Complements NonNativeJSTypeTest.default_values_for_fields(). */
  @Test def defaultValuesForFields(): Unit = {
    val obj = new DefaultFieldValues

    /* Value class fields are initialized to null, instead of a boxed
     * representation of the zero of their underlying types, as for a
     * Scala class.
     *
     * Scala 3 correctly unboxes `null` as the zero of the underlying type of
     * value classes, which means that this test "fails" in Scala 3, although
     * it is in fact an improvement.
     */
    assertNull(obj.asInstanceOf[js.Dynamic].valueClass)
  }

  @Test def callSuperConstructorWithDefaultParamsAndEarlyInitializers_Issue4929(): Unit = {
    import ConstructorSuperCallWithDefaultParamsAndEarlyInitializers._

    sideEffects.clear()

    val child = new Child(4, "hello", 23)
    assertEquals(4, child.foo)
    assertEquals(23, child.bar)
    assertEquals(27, child.xyz)
    assertEquals(29, child.yz)

    assertEquals(
      List(
          "27",
          "4",
          "Parent constructor; param1, 27, param1-27",
          "Child constructor; 4, hello, 23",
          "27, 29"
      ),
      sideEffects.toList
    )
  }

}

object NonNativeJSTypeTestScala2 {

  class DefaultFieldValues extends js.Object {
    var valueClass: SomeValueClass = _
  }

  class SomeValueClass(val i: Int) extends AnyVal

  object ConstructorSuperCallWithDefaultParamsAndEarlyInitializers {
    val sideEffects = mutable.ListBuffer.empty[String]

    class Parent(parentParam1: Any = "param1", parentParam2: Any = "param2")(
        dependentParam: String = s"$parentParam1-$parentParam2")
        extends js.Object {
      sideEffects += s"Parent constructor; $parentParam1, $parentParam2, $dependentParam"
    }

    class Child(val foo: Int, parentParam2: Any, val bar: Int) extends {
          val xyz = foo + bar
          val yz = { sideEffects += xyz.toString(); xyz + 2 }
        } with Parent(parentParam2 = { sideEffects += foo.toString(); foo + bar })() {
      sideEffects += s"Child constructor; $foo, $parentParam2, $bar"
      sideEffects += s"$xyz, $yz"
    }
  }

}
