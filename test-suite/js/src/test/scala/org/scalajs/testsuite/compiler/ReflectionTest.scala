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

import org.scalajs.testsuite.utils.AssertThrows.assertThrows

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
    /* Note: it is not possible to write `classOf[SomeObject.type]`. In order
     * to get the class data of module classes (`object`s) without
     * instantiating them or reaching anything else about them, we go through
     * the `classOf` of an `Array[SomeObject.type]` then extract its
     * `getComponentType()`.
     */

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

    @noinline
    def isInstance(cls: Class[_], x: Any): Boolean = cls.isInstance(x)

    @inline
    def testIsInstance(expected: Boolean, cls: Class[_], x: Any): Unit = {
      assertEquals(expected, cls.isInstance(x))
      assertEquals(expected, isInstance(cls, x))
    }

    @inline
    def testIsInstanceThrows(expected: js.Dynamic, cls: Class[_], x: Any): Unit = {
      val e1 = assertThrows(classOf[js.JavaScriptException], cls.isInstance(x))
      assertTrue(e1.toString(), js.special.instanceof(e1.exception, expected))

      val e2 = assertThrows(classOf[js.JavaScriptException], isInstance(cls, x))
      assertTrue(e2.toString(), js.special.instanceof(e2.exception, expected))
    }

    val jsDate: Any = new js.Date(1684246473882.0)

    testIsInstance(false, classOf[NonNativeClass], jsDate)
    testIsInstance(true, classOf[JSDateForIsInstance], jsDate)

    // NativeClass is not actually defined, so isInstance will throw a ReferenceError
    testIsInstanceThrows(js.constructorOf[js.ReferenceError], classOf[NativeClass], jsDate)

    // isInstance is not supported on JS objects and traits; it throws a TypeError by spec
    testIsInstanceThrows(js.constructorOf[js.TypeError],
        classOf[Array[NativeObject.type]].getComponentType(), jsDate)
    testIsInstanceThrows(js.constructorOf[js.TypeError], classOf[NativeTrait], jsDate)
    testIsInstanceThrows(js.constructorOf[js.TypeError],
        classOf[Array[NonNativeObject.type]].getComponentType(), jsDate)
    testIsInstanceThrows(js.constructorOf[js.TypeError], classOf[NonNativeTrait], jsDate)
    testIsInstanceThrows(js.constructorOf[js.TypeError],
        classOf[Array[JSDateForIsInstance.type]].getComponentType(), jsDate)
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

    @js.native
    @JSGlobal("Date")
    class JSDateForIsInstance extends js.Object

    @js.native
    @JSGlobal("Date")
    object JSDateForIsInstance extends js.Object
  }

  trait PseudoInstantiatedNonNativeTrait extends js.Object

  class InstantiatedNonNativeClass extends js.Object with PseudoInstantiatedNonNativeTrait

  object InstantiatedNonNativeObject extends js.Object with PseudoInstantiatedNonNativeTrait
}
