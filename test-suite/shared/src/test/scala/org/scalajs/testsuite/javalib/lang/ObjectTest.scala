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
import org.scalajs.testsuite.utils.Platform._

// scalastyle:off disallow.space.before.token

class ObjectTest {

  @Test def testGetClass(): Unit = {
    @noinline
    def testNoInline(expected: Class[_], x: Any): Unit =
      assertSame(expected, x.getClass())

    @inline
    def test(expected: Class[_], x: Any): Unit = {
      testNoInline(expected, x)
      assertSame(expected, x.getClass())
    }

    test(if (executingInJVM) classOf[scala.runtime.BoxedUnit] else classOf[java.lang.Void], ())
    test(classOf[java.lang.Boolean], true)
    test(classOf[java.lang.Character], 'A')
    test(classOf[java.lang.Byte], 0.toByte)
    test(classOf[java.lang.Byte], 5.toByte)
    test(classOf[java.lang.Short], 300.toShort)
    test(classOf[java.lang.Integer], 100000)
    test(classOf[java.lang.Long], Long.MaxValue)
    test(classOf[java.lang.Float], -0.0f)
    test(classOf[java.lang.Float], 1.5f)
    test(classOf[java.lang.Float], Float.NaN)
    test(if (hasStrictFloats) classOf[java.lang.Double] else classOf[java.lang.Float], 1.4)
    test(classOf[java.lang.String], "hello")
    test(classOf[java.lang.Object], new Object)
    test(classOf[Some[_]], Some(5))
    test(classOf[ObjectTest], this)

    test(classOf[Array[Array[Int]]], new Array[Array[Int]](1))
    test(classOf[Array[Array[Array[String]]]], new Array[Array[Array[String]]](1))
  }

  @Test def equals(): Unit = {
    case class XY(x: Int, y: Int)

    val l = List(XY(1, 2), XY(2, 1))
    val xy12 = XY(1, 2)

    assertTrue(l.contains(xy12))
    assertTrue(l.exists(_ == xy12)) // the workaround
  }

  @Test def everything_but_null_should_be_an_Object(): Unit = {
    assertTrue((()             : Any).isInstanceOf[Object])
    assertTrue((true           : Any).isInstanceOf[Object])
    assertTrue(('a'            : Any).isInstanceOf[Object])
    assertTrue((1.toByte       : Any).isInstanceOf[Object])
    assertTrue((658.toShort    : Any).isInstanceOf[Object])
    assertTrue((60000          : Any).isInstanceOf[Object])
    assertTrue((12345678910112L: Any).isInstanceOf[Object])
    assertTrue((6.5f           : Any).isInstanceOf[Object])
    assertTrue((12.4           : Any).isInstanceOf[Object])
    assertTrue((new Object     : Any).isInstanceOf[Object])
    assertTrue(("hello"        : Any).isInstanceOf[Object])
    assertTrue((List(1)        : Any).isInstanceOf[Object])
    assertTrue((Array(1)       : Any).isInstanceOf[Object])
    assertTrue((Array(Nil)     : Any).isInstanceOf[Object])
  }

  @Test def null_should_not_be_an_Object(): Unit = {
    assertFalse((null: Any).isInstanceOf[Object])
  }

  @Test def everything_should_cast_to_Object_successfully_including_null(): Unit = {
    (()             : Any).asInstanceOf[Object]
    (true           : Any).asInstanceOf[Object]
    ('a'            : Any).asInstanceOf[Object]
    (1.toByte       : Any).asInstanceOf[Object]
    (658.toShort    : Any).asInstanceOf[Object]
    (60000          : Any).asInstanceOf[Object]
    (12345678910112L: Any).asInstanceOf[Object]
    (6.5f           : Any).asInstanceOf[Object]
    (12.4           : Any).asInstanceOf[Object]
    (new Object     : Any).asInstanceOf[Object]
    ("hello"        : Any).asInstanceOf[Object]
    (List(1)        : Any).asInstanceOf[Object]
    (Array(1)       : Any).asInstanceOf[Object]
    (Array(Nil)     : Any).asInstanceOf[Object]
    (null           : Any).asInstanceOf[Object]
  }

  @Test def cloneCtorSideEffects_issue_3192(): Unit = {
    var ctorInvokeCount = 0

    // This class has an inlineable init
    class CloneCtorSideEffectsBug(val x: Int) extends java.lang.Cloneable {
      ctorInvokeCount += 1

      override def clone(): CloneCtorSideEffectsBug =
        super.clone().asInstanceOf[CloneCtorSideEffectsBug]
    }

    val o = new CloneCtorSideEffectsBug(54)
    assertEquals(54, o.x)
    assertEquals(1, ctorInvokeCount)

    val o2 = o.clone()
    assertEquals(54, o2.x)
    assertEquals(1, ctorInvokeCount)
  }
}
