/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.compiler

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._

import org.scalajs.testsuite.utils.AssertThrows._
import org.scalajs.testsuite.utils.Platform._

class InstanceTestsHijackedBoxedClassesTest {

  @Test def should_support_isInstanceOf_positive(): Unit = {
    assertTrue(((): Any).isInstanceOf[Unit])
    assertTrue((false: Any).isInstanceOf[Boolean])
    assertTrue(('a': Any).isInstanceOf[Char])
    assertTrue((65.toByte: Any).isInstanceOf[Byte])
    assertTrue((654.toShort: Any).isInstanceOf[Short])
    assertTrue((-4321: Any).isInstanceOf[Int])
    assertTrue((684321L: Any).isInstanceOf[Long])
    assertTrue((3.14f: Any).isInstanceOf[Float])
    assertTrue((3.14: Any).isInstanceOf[Double])

    assertTrue((45: Any).isInstanceOf[Float])
    assertTrue((45: Any).isInstanceOf[Double])
    assertTrue((3.0f: Any).isInstanceOf[Int])
    assertTrue((3.0f: Any).isInstanceOf[Double])
    assertTrue((5.0: Any).isInstanceOf[Int])
    assertTrue((5.0: Any).isInstanceOf[Float])

    assertTrue((0.0: Any).isInstanceOf[Int])
    assertTrue((0.0: Any).isInstanceOf[Float])
    assertTrue((-0.0: Any).isInstanceOf[Float])
  }

  @Test def should_support_isInstanceOf_negative(): Unit = {
    assertFalse((12345: Any).isInstanceOf[Unit])
    assertFalse((12345: Any).isInstanceOf[Boolean])
    assertFalse((12345: Any).isInstanceOf[Char])
    assertFalse(('a': Any).isInstanceOf[Byte])
    assertFalse(('b': Any).isInstanceOf[Short])
    assertFalse(('c': Any).isInstanceOf[Int])
    assertFalse(('d': Any).isInstanceOf[Long])
    assertFalse(('f': Any).isInstanceOf[Float])
    assertFalse(('g': Any).isInstanceOf[Double])

    assertFalse((-0.0: Any).isInstanceOf[Int])
  }

  @Test def isInstanceOf_Float_with_strict_floats(): Unit = {
    assumeTrue("Assumed strict floats", hasStrictFloats)
    assertFalse((1.2: Any).isInstanceOf[Float])
  }

  @Test def isInstanceOf_Float_with_non_strict_floats(): Unit = {
    assumeFalse("Assumed strict floats", hasStrictFloats)
    assertTrue((1.2: Any).isInstanceOf[Float])

    // from the bug report
    def test(x: Any): String = x match {
      case f: Float => "ok"
    }
    assertEquals("ok", test(0.2))
  }

  @Test def should_support_asInstanceOf_positive(): Unit = {
    def swallow(x: Any): Unit = ()
    swallow(((): Any).asInstanceOf[Unit])
    swallow((false: Any).asInstanceOf[Boolean])
    swallow(('a': Any).asInstanceOf[Char])
    swallow((65.toByte: Any).asInstanceOf[Byte])
    swallow((654.toShort: Any).asInstanceOf[Short])
    swallow((-4321: Any).asInstanceOf[Int])
    swallow((684321L: Any).asInstanceOf[Long])
    swallow((3.14f: Any).asInstanceOf[Float])
    swallow((3.14: Any).asInstanceOf[Double])
    if (scalaVersion.startsWith("2.12."))
      (12345: Any).asInstanceOf[Unit]
  }

  @Test def should_support_asInstanceOf_negative(): Unit = {
    assumeTrue("Assumed compliant asInstanceOf", hasCompliantAsInstanceOfs)
    if (!scalaVersion.startsWith("2.12."))
      assertThrows(classOf[Exception], (12345: Any).asInstanceOf[Unit])
    assertThrows(classOf[Exception], (12345: Any).asInstanceOf[Boolean])
    assertThrows(classOf[Exception], (12345: Any).asInstanceOf[Char])
    assertThrows(classOf[Exception], ('a': Any).asInstanceOf[Byte])
    assertThrows(classOf[Exception], ('b': Any).asInstanceOf[Short])
    assertThrows(classOf[Exception], ('c': Any).asInstanceOf[Int])
    assertThrows(classOf[Exception], ('d': Any).asInstanceOf[Long])
    assertThrows(classOf[Exception], ('f': Any).asInstanceOf[Float])
    assertThrows(classOf[Exception], ('g': Any).asInstanceOf[Double])

    assertThrows(classOf[Exception], (-0.0: Any).asInstanceOf[Int])
  }

  @Test def asInstanceOf_Float_with_strict_floats(): Unit = {
    assumeTrue("Assumed strict floats", hasStrictFloats)
    assumeTrue("Assumed compliant asInstanceOf", hasCompliantAsInstanceOfs)
    assertThrows(classOf[Exception], (1.2: Any).asInstanceOf[Float])
  }

  @Test def asInstanceOf_Float_with_non_strict_floats(): Unit = {
    assumeFalse("Assumed strict floats", hasStrictFloats)
    assertEquals(1.2, (1.2: Any).asInstanceOf[Float])
  }

  @Test def should_support_isInstanceOf_via_java_lang_Class_positive(): Unit = {
    def test(x: Any, clazz: Class[_]): Unit =
      assertTrue(clazz.isInstance(x))

    test((), classOf[scala.runtime.BoxedUnit])
    test(false, classOf[java.lang.Boolean])
    test('a', classOf[java.lang.Character])
    test(65.toByte, classOf[java.lang.Byte])
    test(654.toShort, classOf[java.lang.Short])
    test(-4321, classOf[java.lang.Integer])
    test(684321L, classOf[java.lang.Long])
    test(3.14f, classOf[java.lang.Float])
    test(3.14, classOf[java.lang.Double])

    test(0.0, classOf[java.lang.Integer])
    test(0.0, classOf[java.lang.Double])
    test(-0.0, classOf[java.lang.Double])
  }

  @Test def should_support_isInstanceOf_via_java_lang_Class_negative(): Unit = {
    def test(x: Any, clazz: Class[_]): Unit =
      assertFalse(clazz.isInstance(x))

    test(12345, classOf[scala.runtime.BoxedUnit])
    test(12345, classOf[java.lang.Boolean])
    test(12345, classOf[java.lang.Character])
    test('a', classOf[java.lang.Byte])
    test('b', classOf[java.lang.Short])
    test('c', classOf[java.lang.Integer])
    test('d', classOf[java.lang.Long])
    test('e', classOf[java.lang.Float])
    test('f', classOf[java.lang.Double])

    test(-0.0, classOf[java.lang.Integer])
  }

  @Test def classOf_Float_isInstance_with_strict_floats(): Unit = {
    assumeTrue("Assumed strict floats", hasStrictFloats)
    assertFalse(classOf[java.lang.Float].isInstance(1.2))
  }

  @Test def classOf_Float_isInstance_with_non_strict_floats(): Unit = {
    assumeFalse("Assumed strict floats", hasStrictFloats)
    assertTrue(classOf[java.lang.Float].isInstance(1.2))
  }
}
