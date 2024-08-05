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

import scala.scalajs.js
import scala.scalajs.js.annotation._

import org.junit.Assert._
import org.junit.Assume._
import org.junit.Test

import org.scalajs.testsuite.utils.AssertThrows.assertThrows
import org.scalajs.testsuite.utils.Platform._

class MiscInteropTest {
  import MiscInteropTest._

  // scala.scalajs.js.package

  @Test def equivalentToTypeOf(): Unit = {
    import js.typeOf
    assertEquals("number", typeOf(5))
    assertEquals("boolean", typeOf(false))
    assertEquals("string", typeOf("hello"))
    assertEquals("object", typeOf(null))
    assertEquals("object", typeOf(new js.Object))
    assertEquals("undefined", typeOf(()))
    assertEquals("function", typeOf((() => 42): js.Function))
  }

  @Test def testTypeOfWithGlobalRefs_Issue3822(): Unit = {
    assumeFalse(
        "GCC wrongly optimizes this code, " +
        "see https://github.com/google/closure-compiler/issues/3498",
        usesClosureCompiler)

    @noinline def nonExistentGlobalVarNoInline(): Any =
      js.Dynamic.global.thisGlobalVarDoesNotExist

    @inline def nonExistentGlobalVarInline(): Any =
      js.Dynamic.global.thisGlobalVarDoesNotExist

    assertEquals("undefined",
        js.typeOf(js.Dynamic.global.thisGlobalVarDoesNotExist))
    assertThrows(classOf[js.JavaScriptException],
        js.typeOf(nonExistentGlobalVarNoInline()))
    assertThrows(classOf[js.JavaScriptException],
        js.typeOf(nonExistentGlobalVarInline()))
  }

  @Test def jsConstructorOfTForNativeClasses(): Unit = {
    assertSame(js.Dynamic.global.RegExp, js.constructorOf[js.RegExp])
    assertSame(js.Dynamic.global.Array, js.constructorOf[js.Array[_]])
    assertSame(js.Dynamic.global.Array, js.constructorOf[js.Array[Int]])
  }

  @Test def jsConstructorOfTForScalaJSDefinedJSClasses(): Unit = {
    val concreteCtor = (new ConcreteJSClass).asInstanceOf[js.Dynamic].constructor
    val concreteProto = concreteCtor.prototype.asInstanceOf[js.Object]
    val abstractProto = js.Object.getPrototypeOf(concreteProto)
    val abstractCtor = abstractProto.asInstanceOf[js.Dynamic].constructor

    assertSame(concreteCtor, js.constructorOf[ConcreteJSClass])
    assertSame(abstractCtor, js.constructorOf[AbstractJSClass])

    val concreteInstance = js.Dynamic.newInstance(js.constructorOf[ConcreteJSClass])()
    assertTrue((concreteInstance: Any).isInstanceOf[ConcreteJSClass])

    val instance = js.Dynamic.newInstance(
      js.constructorOf[OtherwiseUnreferencedJSClass])(35)
    assertEquals(35, instance.x)
  }

  @Test def jsConstructorTagTForNativeClasses(): Unit = {
    def test[T <: js.Any: js.ConstructorTag](expected: js.Dynamic): Unit =
      assertSame(expected, js.constructorTag[T].constructor)

    test[js.RegExp](js.Dynamic.global.RegExp)
    test[js.Array[_]](js.Dynamic.global.Array)
    test[js.Array[Int]](js.Dynamic.global.Array)
  }

  @Test def jsConstructorTagTForScalaJSDefinedJSClasses(): Unit = {
    def test[T <: js.Any: js.ConstructorTag](expected: js.Dynamic): Unit =
      assertSame(expected, js.constructorTag[T].constructor)

    val concreteCtor = (new ConcreteJSClass).asInstanceOf[js.Dynamic].constructor
    val concreteProto = concreteCtor.prototype.asInstanceOf[js.Object]
    val abstractProto = js.Object.getPrototypeOf(concreteProto)
    val abstractCtor = abstractProto.asInstanceOf[js.Dynamic].constructor

    test[ConcreteJSClass](concreteCtor)
    test[AbstractJSClass](abstractCtor)

    val concreteInstance = {
      val tag = js.constructorTag[ConcreteJSClass]
      tag.newInstance()
    }
    assertTrue((concreteInstance: Any).isInstanceOf[ConcreteJSClass])

    val instance = {
      val tag = js.constructorTag[OtherwiseUnreferencedJSClassForTag]
      tag.newInstance(35)
    }
    assertEquals(35, instance.x)
  }

  // scala.scalajs.js.Object

  @Test def equivalentToPInO(): Unit = {
    val o = js.Dynamic.literal(foo = 5, bar = "foobar")
    assertTrue(js.Object.hasProperty(o, "foo"))
    assertFalse(js.Object.hasProperty(o, "foobar"))
    assertTrue(js.Object.hasProperty(o, "toString")) // in prototype
  }

  @Test def evaluationOrderForHasProperty(): Unit = {
    var indicator = 3
    def o(): js.Object = {
      indicator += 4
      js.Dynamic.literal(x = 5)
    }
    def p(): String = {
      indicator *= 2
      "x"
    }
    assertTrue(js.Object.hasProperty(o(), p()))
    assertEquals(14, indicator)
  }

  @Test def equivalentOfJSForInLoopOf_Issue13(): Unit = {
    val obj = js.eval("var dictionaryTest13 = { a: 'Scala.js', b: 7357 }; dictionaryTest13;")
    val dict = obj.asInstanceOf[js.Dictionary[js.Any]]
    var propCount = 0
    var propString = ""

    for (prop <- js.Object.properties(dict)) {
      propCount += 1
      propString += dict(prop)
    }

    assertEquals(2, propCount)
    assertEquals("Scala.js7357", propString)
  }

  @Test def equivalentOfJSForInLoop2Of_Issue13(): Unit = {
    val obj = js.eval("var arrayTest13 = [ 7, 3, 5, 7 ]; arrayTest13;")
    val array = obj.asInstanceOf[js.Dictionary[js.Any]]
    var propCount = 0
    var propString = ""

    for (prop <- js.Object.properties(array)) {
      propCount += 1
      propString += array(prop)
    }

    assertEquals(4, propCount)
    assertEquals("7357", propString)
  }

  @Test def compileJSUndefined(): Unit = {
    assertThrows(classOf[Exception], js.undefined.asInstanceOf[js.Dynamic].toFixed())
  }

  @Test def defineDirectSubtraitsOfJSAny(): Unit = {
    val f = js.Dynamic.literal(
      foo = (x: Int) => x + 1
    ).asInstanceOf[DirectSubtraitOfJSAny]

    assertEquals(6, f.foo(5))
  }

  @Test def defineDirectSubclassesOfJSAny(): Unit = {
    val f = js.Dynamic.literal(
      bar = (x: Int) => x + 2
    ).asInstanceOf[DirectSubclassOfJSAny]

    assertEquals(7, f.bar(5))
  }

  // Global scope

  @Test def canReadUndefinedInGlobalScope_Issue3821(): Unit = {
    assertEquals((), js.Dynamic.global.undefined)
  }

  // Emitted classes

  @Test def meaningfulNameProperty(): Unit = {
    assumeFalse("Not supported on WebAssembly", executingInWebAssembly)
    assumeFalse("Need non-minified names", hasMinifiedNames)

    def nameOf(obj: Any): js.Any =
      obj.asInstanceOf[js.Dynamic].constructor.name

    assertTrue(nameOf(new SomeScalaClass).toString.contains("SomeScalaClass"))
    assertTrue(nameOf(new SomeJSClass).toString.contains("SomeJSClass"))
  }

}

object MiscInteropTest {

  abstract class AbstractJSClass extends js.Object

  class ConcreteJSClass extends AbstractJSClass

  class OtherwiseUnreferencedJSClass(val x: Int) extends js.Object

  class OtherwiseUnreferencedJSClassForTag(val x: Int) extends js.Object

  @js.native
  trait DirectSubtraitOfJSAny extends js.Any {
    def foo(x: Int): Int = js.native
  }

  @JSGlobal("DirectSubclassOfJSAny")
  @js.native
  class DirectSubclassOfJSAny extends js.Any {
    def bar(x: Int): Int = js.native
  }

  class SomeScalaClass

  class SomeJSClass extends js.Object

}
