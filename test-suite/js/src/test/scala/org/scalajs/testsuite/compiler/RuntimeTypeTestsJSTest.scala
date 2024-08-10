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

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

import org.junit.Test
import org.junit.Assert._

import org.scalajs.testsuite.utils.AssertThrows.assertThrows
import org.scalajs.testsuite.utils.Platform._

class RuntimeTypeTestsJSTest {
  import RuntimeTypeTestsJSTest._

  // Import the helpers from the non-JS test
  import RuntimeTypeTestsTest._

  @Test def objectType(): Unit = {
    @inline def testObject(expected: Boolean, value: Any): Unit =
      test(expected, value, classOf[Object], _.isInstanceOf[Object], _.asInstanceOf[Object])

    testObject(true, new ParentJSClass)
    testObject(true, new ChildJSClass)
    testObject(true, new js.Object)
  }

  @Test def jsObject(): Unit = {
    @inline def testJSObject(expected: Boolean, value: Any): Unit =
      testJS(expected, value, classOf[js.Object], _.isInstanceOf[js.Object], _.asInstanceOf[js.Object])

    testJSObject(true, new ParentJSClass)
    testJSObject(true, new ChildJSClass)
    testJSObject(true, new js.Object)

    /* Testing an implementation detail -- we do not actually give a guarantee
     * about whether Scala objects are instances of Object.
     */
    val expectedForScalaObj = !executingInWebAssembly
    testJSObject(expectedForScalaObj, List(5))
    testJSObject(expectedForScalaObj, new Throwable)
    testJSObject(expectedForScalaObj, new Exception)

    testJSObject(false, 5)
    testJSObject(false, ())
    testJSObject(false, true)

    testNullValue(classOf[js.Object], _.isInstanceOf[js.Object], _.asInstanceOf[js.Object])
  }

  @Test def jsError(): Unit = {
    @inline def testJSError(expected: Boolean, value: Any): Unit =
      testJS(expected, value, classOf[js.Error], _.isInstanceOf[js.Error], _.asInstanceOf[js.Error])

    testJSError(true, new js.Error)
    testJSError(true, new js.EvalError)

    /* Testing an implementation detail -- we do not actually give a guarantee
     * about whether Scala objects are instances of Error.
     */
    val expectedForScalaException = !executingInWebAssembly
    testJSError(expectedForScalaException, new Throwable)
    testJSError(expectedForScalaException, new Exception)

    testJSError(false, 5)
    testJSError(false, ())
    testJSError(false, true)

    testNullValue(classOf[js.Error], _.isInstanceOf[js.Error], _.asInstanceOf[js.Error])
  }

  @Test def jsClass(): Unit = {
    @inline def testParentJSClass(expected: Boolean, value: Any): Unit =
      testJS(expected, value, classOf[ParentJSClass], _.isInstanceOf[ParentJSClass], _.asInstanceOf[ParentJSClass])

    testParentJSClass(true, new ParentJSClass)
    testParentJSClass(true, new ChildJSClass)

    testParentJSClass(false, new js.Object)
    testParentJSClass(false, List(5))

    testNullValue(classOf[ParentJSClass], _.isInstanceOf[ParentJSClass], _.asInstanceOf[ParentJSClass])
  }

  @Test def jsTrait(): Unit = {
    assertThrows(classOf[Exception], classOf[ChildJSInterface].isInstance(5))
    assertThrows(classOf[Exception], classOf[ChildJSInterface].isInstance(new ParentJSClass))
    assertThrows(classOf[Exception], classOf[ChildJSInterface].isInstance(null))
  }

  @Test def scalaArraysOfJSTypes(): Unit = {
    val arrayOfParentJSInterface = new Array[ParentJSInterface](0)
    val arrayOfJSInterface = new Array[ChildJSInterface](0)
    val arrayOfJSClass = new Array[ChildJSClass](0)

    @inline def testArrayObject(expected: Boolean, value: Any): Unit =
      test(expected, value, classOf[Array[Object]], _.isInstanceOf[Array[Object]], _.asInstanceOf[Array[Object]])

    @inline def testArrayParentJSInterface(expected: Boolean, value: Any): Unit = {
      test(expected, value, classOf[Array[ParentJSInterface]],
          _.isInstanceOf[Array[ParentJSInterface]], _.asInstanceOf[Array[ParentJSInterface]])
    }

    @inline def testArrayChildJSInterface(expected: Boolean, value: Any): Unit = {
      test(expected, value, classOf[Array[ChildJSInterface]],
          _.isInstanceOf[Array[ChildJSInterface]], _.asInstanceOf[Array[ChildJSInterface]])
    }

    @inline def testArrayChildJSClass(expected: Boolean, value: Any): Unit = {
      test(expected, value, classOf[Array[ChildJSClass]],
          _.isInstanceOf[Array[ChildJSClass]], _.asInstanceOf[Array[ChildJSClass]])
    }

    @inline def testArrayJSObject(expected: Boolean, value: Any): Unit = {
      test(expected, value, classOf[Array[js.Object]],
          _.isInstanceOf[Array[js.Object]], _.asInstanceOf[Array[js.Object]])
    }

    testArrayObject(true, arrayOfParentJSInterface)
    testArrayObject(true, arrayOfJSInterface)
    testArrayObject(true, arrayOfJSClass)

    testArrayParentJSInterface(true, arrayOfParentJSInterface)
    testArrayChildJSInterface(true, arrayOfJSInterface)
    testArrayChildJSClass(true, arrayOfJSClass)

    testArrayParentJSInterface(true, arrayOfJSInterface)
    testArrayParentJSInterface(true, arrayOfJSClass)

    testArrayChildJSInterface(false, arrayOfParentJSInterface)
    testArrayChildJSClass(false, arrayOfParentJSInterface)

    testArrayJSObject(false, arrayOfParentJSInterface)
    testArrayJSObject(false, arrayOfJSInterface)
    testArrayJSObject(true, arrayOfJSClass)
  }

}

object RuntimeTypeTestsJSTest {
  trait ParentJSInterface extends js.Object

  trait ChildJSInterface extends ParentJSInterface

  class ParentJSClass extends js.Object

  class ChildJSClass extends ParentJSClass with ParentJSInterface
}
