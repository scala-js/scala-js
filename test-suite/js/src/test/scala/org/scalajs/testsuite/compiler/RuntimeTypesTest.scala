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

import java.lang.Cloneable
import java.io.Serializable

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._

import org.scalajs.testsuite.utils.AssertThrows._
import org.scalajs.testsuite.utils.Platform._

import scala.util.{ Try, Failure }

class RuntimeTypesTest {
  import RuntimeTypesTest._

  @Test def scalaArraysAreInstancesOfSerializableAndCloneable_Issue2094(): Unit = {
    assertTrue((Array(3): Any).isInstanceOf[Serializable])
    assertTrue((Array(3): Any).isInstanceOf[Cloneable])
    assertTrue((Array("hello"): Any).isInstanceOf[Serializable])
    assertTrue((Array("hello"): Any).isInstanceOf[Cloneable])
  }

  @Test def scalaArraysCastToSerializableAndCloneable_Issue2094(): Unit = {
    (Array(3): Any).asInstanceOf[Serializable] // should not throw
    (Array(3): Any).asInstanceOf[Cloneable] // should not throw
    (Array("hello"): Any).asInstanceOf[Serializable] // should not throw
    (Array("hello"): Any).asInstanceOf[Cloneable] // should not throw
  }

  @Test def scalaNothingCastsToScalaNothingThrows(): Unit = {
    assumeTrue("Assumed compliant asInstanceOf", hasCompliantAsInstanceOfs)

    def test(x: Any): Unit =
      assertThrows(classOf[ClassCastException], x.asInstanceOf[Nothing])

    test("a")
    test(null)
  }

  @Test def scalaNothingReflectedCastsToScalaNothingThrows(): Unit = {
    assumeTrue("Assumed compliant asInstanceOf", hasCompliantAsInstanceOfs)
    def test(x: Any): Unit = {
      try {
        val result = classOf[Nothing].cast(x)
        fail("casting " + x + " to Nothing did not fail")
        identity(result) // discard `result` without warning
      } catch {
        case th: ClassCastException =>
          // ok
      }
    }
    test("a")
    test(null)
  }

  @Test def scalaNullCastsToScalaNullThrowsForEverythingButNull(): Unit = {
    assumeTrue("Assumed compliant asInstanceOf", hasCompliantAsInstanceOfs)
    assertThrows(classOf[ClassCastException], "a".asInstanceOf[Null])
  }

  @Test def scalaNullCastsToScalaNullWorksOnNull(): Unit = {
    null.asInstanceOf[Null]
  }

  @Test def scalaArraysOfJSTypes(): Unit = {
    val arrayOfParentJSType = new Array[ParentJSType](0)
    val arrayOfJSInterface = new Array[SomeJSInterface](0)
    val arrayOfJSClass = new Array[SomeJSClass](0)

    assertTrue(arrayOfParentJSType.isInstanceOf[Array[AnyRef]])
    assertTrue(arrayOfJSInterface.isInstanceOf[Array[AnyRef]])
    assertTrue(arrayOfJSClass.isInstanceOf[Array[AnyRef]])

    assertTrue(arrayOfParentJSType.isInstanceOf[Array[ParentJSType]])
    assertTrue(arrayOfJSInterface.isInstanceOf[Array[SomeJSInterface]])
    assertTrue(arrayOfJSClass.isInstanceOf[Array[SomeJSClass]])

    assertTrue(arrayOfJSInterface.isInstanceOf[Array[ParentJSType]])
    assertTrue(arrayOfJSClass.isInstanceOf[Array[ParentJSType]])

    assertFalse(arrayOfParentJSType.isInstanceOf[Array[SomeJSInterface]])
    assertFalse(arrayOfParentJSType.isInstanceOf[Array[SomeJSClass]])

    assertFalse(arrayOfJSInterface.isInstanceOf[Array[js.Object]])
    assertTrue(arrayOfJSClass.isInstanceOf[Array[js.Object]])
  }

}

object RuntimeTypesTest {

  @js.native
  trait ParentJSType extends js.Object

  @js.native
  trait SomeJSInterface extends ParentJSType

  @JSGlobal("SomeJSClass")
  @js.native
  class SomeJSClass extends ParentJSType

}
