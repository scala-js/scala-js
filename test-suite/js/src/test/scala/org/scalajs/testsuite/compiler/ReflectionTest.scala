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
import js.annotation.JSGlobal

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._

/** Tests the little reflection we support */
class ReflectionTest {
  import ReflectionTest._

  @Test def javaLangClassGetNameUnderNormalCircumstances(): Unit = {
    @noinline
    def testNoInline(expected: String, cls: Class[_]): Unit =
      assertEquals(expected, cls.getName())

    @inline
    def test(expected: String, cls: Class[_]): Unit = {
      testNoInline(expected, cls)
      assertEquals(expected, cls.getName())
    }

    test("scala.Some", classOf[scala.Some[_]])
  }

  @Test def appendDollarSignToClassNameOfObjects(): Unit = {
    assertEquals("org.scalajs.testsuite.compiler.ReflectionTest$TestObject$",
      TestObject.getClass.getName)
  }

  @Test def javaLangClassGetNameRenamedThroughSemantics(): Unit = {
    @noinline
    def testNoInline(expected: String, cls: Class[_]): Unit =
      assertEquals(expected, cls.getName())

    @inline
    def test(expected: String, cls: Class[_]): Unit = {
      testNoInline(expected, cls)
      assertEquals(expected, cls.getName())
    }

    test("renamed.test.Class", classOf[RenamedTestClass])
    test("renamed.test.byprefix.RenamedTestClass1",
        classOf[PrefixRenamedTestClass1])
    test("renamed.test.byprefix.RenamedTestClass2",
        classOf[PrefixRenamedTestClass2])
    test("renamed.test.byotherprefix.RenamedTestClass",
        classOf[OtherPrefixRenamedTestClass])
  }

  @Test def javaLangObjectGetClassGetNameRenamedThroughSemantics(): Unit = {
    // x.getClass().getName() is subject to optimizations

    @noinline
    def getClassOfNoInline(x: Any): Class[_] =
      x.getClass()

    @noinline
    def testNoInline(expected: String, x: Any): Unit = {
      assertEquals(expected, getClassOfNoInline(x).getName())
      assertEquals(expected, x.getClass().getName())
    }

    @inline
    def test(expected: String, x: Any): Unit = {
      testNoInline(expected, x)
      assertEquals(expected, x.getClass().getName())
    }

    test("renamed.test.Class", new RenamedTestClass)
    test("renamed.test.byprefix.RenamedTestClass1",
        new PrefixRenamedTestClass1)
    test("renamed.test.byprefix.RenamedTestClass2",
        new PrefixRenamedTestClass2)
    test("renamed.test.byotherprefix.RenamedTestClass",
        new OtherPrefixRenamedTestClass)
  }

  @Test def getClassForNormalTypes(): Unit = {
    class Foo {
      def bar(): Class[_] = super.getClass()
    }
    val foo = new Foo
    assertSame(foo.getClass(), classOf[Foo])
    assertSame(foo.bar(), classOf[Foo])
  }

  @Test def getClassForAntiBoxedPrimitiveTypes(): Unit = {
    assertEquals(classOf[java.lang.Boolean], (false: Any).getClass)
    assertEquals(classOf[java.lang.Character], ('a': Any).getClass)
    assertEquals(classOf[java.lang.Byte], (1.toByte: Any).getClass)
    assertEquals(classOf[java.lang.Byte], (1.toShort: Any).getClass)
    assertEquals(classOf[java.lang.Byte], (1: Any).getClass)
    assertEquals(classOf[java.lang.Long], (1L: Any).getClass)
    assertEquals(classOf[java.lang.Float], (1.5f: Any).getClass)
    assertEquals(classOf[java.lang.Float], (1.5: Any).getClass)
    assertEquals(classOf[scala.runtime.BoxedUnit], ((): Any).getClass)
  }
}

object ReflectionTest {
  object TestObject

  class RenamedTestClass

  class PrefixRenamedTestClass1
  class PrefixRenamedTestClass2

  class OtherPrefixRenamedTestClass

}
