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

  @Test def getClassForJSTypes(): Unit = {
    @noinline
    def getClassOfNoInline(x: Any): Class[_] =
      x.getClass()

    @noinline
    def hide(x: Any): Any = x

    val jsObj = new js.Object()

    assertNull(jsObj.getClass())
    assertNull(getClassOfNoInline(jsObj))

    if (jsObj.getClass() != null)
      fail("optimizer thought that jsObj.getClass() was non-null")

    val hiddenJSObj = hide(jsObj)
    if (hiddenJSObj.getClass() != null)
      fail("optimizer thought that hiddenJSObj.getClass() was non-null")
  }

  @Test def jsTypesKeptOnlyForTheirData_Issue4850(): Unit = {
    import JSTypesKeptOnlyForTheirData._

    @noinline
    def nameOf(cls: Class[_]): String = cls.getName()

    @inline
    def testName(expectedShortName: String, cls: Class[_]): Unit = {
      val prefix = "org.scalajs.testsuite.compiler.ReflectionTest$JSTypesKeptOnlyForTheirData$"
      val expectedName = prefix + expectedShortName

      assertEquals(expectedName, cls.getName()) // constant-folded
      assertEquals(expectedName, nameOf(cls)) // evaluated at run-time
    }

    testName("NativeClass", classOf[NativeClass])
    testName("NativeObject$", classOf[Array[NativeObject.type]].getComponentType())
    testName("NativeTrait", classOf[NativeTrait])
    testName("NonNativeClass", classOf[NonNativeClass])
    testName("NonNativeObject$", classOf[Array[NonNativeObject.type]].getComponentType())
    testName("NonNativeTrait", classOf[NonNativeTrait])

    @noinline
    def isInterfaceOf(cls: Class[_]): Boolean = cls.isInterface()

    @inline
    def testIsInterface(expected: Boolean, cls: Class[_]): Unit = {
      assertEquals(expected, cls.isInterface()) // could be constant-folded in the future
      assertEquals(expected, isInterfaceOf(cls)) // evaluated at run-time
    }

    // Consistent with isInterfaceForInstantiatedJSTypes()
    testIsInterface(false, classOf[NativeClass])
    testIsInterface(false, classOf[Array[NativeObject.type]].getComponentType())
    testIsInterface(false, classOf[NativeTrait])
    testIsInterface(false, classOf[NonNativeClass])
    testIsInterface(false, classOf[Array[NonNativeObject.type]].getComponentType())
    testIsInterface(false, classOf[NonNativeTrait])
  }

  @Test def isInterfaceForInstantiatedJSTypes(): Unit = {
    // Make sure the instantiated non-native things are actually instantiated
    assertEquals("function", js.typeOf(js.constructorOf[InstantiatedNonNativeClass]))
    assertEquals("object", js.typeOf(InstantiatedNonNativeObject))

    @noinline
    def isInterfaceOf(cls: Class[_]): Boolean = cls.isInterface()

    @inline
    def testIsInterface(expected: Boolean, cls: Class[_]): Unit = {
      assertEquals(expected, cls.isInterface()) // could be constant-folded in the future
      assertEquals(expected, isInterfaceOf(cls)) // evaluated at run-time
    }

    // Consistent with jsTypesKeptOnlyForTheirData_Issue4850()
    testIsInterface(false, classOf[js.Date]) // native class
    testIsInterface(false, classOf[Array[js.Math.type]].getComponentType()) // native object
    testIsInterface(false, classOf[js.Function0[Any]]) // native trait
    testIsInterface(false, classOf[InstantiatedNonNativeClass])
    testIsInterface(false, classOf[Array[InstantiatedNonNativeObject.type]].getComponentType())
    testIsInterface(false, classOf[PseudoInstantiatedNonNativeTrait])
  }
}

object ReflectionTest {
  object TestObject

  class RenamedTestClass

  class PrefixRenamedTestClass1
  class PrefixRenamedTestClass2

  class OtherPrefixRenamedTestClass

  object JSTypesKeptOnlyForTheirData {
    @js.native
    @JSGlobal("NativeClass")
    class NativeClass extends js.Object

    @js.native
    @JSGlobal("NativeObject")
    object NativeObject extends js.Object

    @js.native
    trait NativeTrait extends js.Object

    class NonNativeClass extends js.Object

    object NonNativeObject extends js.Object

    trait NonNativeTrait extends js.Object
  }

  trait PseudoInstantiatedNonNativeTrait extends js.Object

  class InstantiatedNonNativeClass extends js.Object with PseudoInstantiatedNonNativeTrait

  object InstantiatedNonNativeObject extends js.Object with PseudoInstantiatedNonNativeTrait
}
