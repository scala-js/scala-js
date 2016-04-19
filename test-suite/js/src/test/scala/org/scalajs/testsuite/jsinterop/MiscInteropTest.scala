/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.jsinterop

import scala.scalajs.js
import scala.scalajs.js.annotation._

import org.junit.Assert._
import org.junit.Assume._
import org.junit.Test

import org.scalajs.testsuite.utils.AssertThrows._
import org.scalajs.testsuite.utils.Platform._

class MiscInteropTest {
  import MiscInteropTest._

  // scala.scalajs.js.package

  @Test def should_provide_an_equivalent_to_typeof_x(): Unit = {
    import js.typeOf
    assertEquals("number", typeOf(5))
    assertEquals("boolean", typeOf(false))
    assertEquals("string", typeOf("hello"))
    assertEquals("object", typeOf(null))
    assertEquals("object", typeOf(new js.Object))
    assertEquals("undefined", typeOf(()))
    assertEquals("function", typeOf(() => 42))
  }

  @Test def js_constructorOf_T_for_native_classes(): Unit = {
    assertSame(js.Dynamic.global.RegExp, js.constructorOf[js.RegExp])
    assertSame(js.Dynamic.global.Array, js.constructorOf[js.Array[_]])
    assertSame(js.Dynamic.global.Array, js.constructorOf[js.Array[Int]])
  }

  @Test def js_constructorOf_T_for_Scala_js_defined_JS_classes(): Unit = {
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

  @Test def js_constructorTag_T_for_native_classes(): Unit = {
    def test[T <: js.Any: js.ConstructorTag](expected: js.Dynamic): Unit =
      assertSame(expected, js.constructorTag[T].constructor)

    test[js.RegExp](js.Dynamic.global.RegExp)
    test[js.Array[_]](js.Dynamic.global.Array)
    test[js.Array[Int]](js.Dynamic.global.Array)
  }

  @Test def js_constructorTag_T_for_Scala_js_defined_JS_classes(): Unit = {
    def test[T <: js.Any: js.ConstructorTag](expected: js.Dynamic): Unit =
      assertSame(expected, js.constructorTag[T].constructor)

    val concreteCtor = (new ConcreteJSClass).asInstanceOf[js.Dynamic].constructor
    val concreteProto = concreteCtor.prototype.asInstanceOf[js.Object]
    val abstractProto = js.Object.getPrototypeOf(concreteProto)
    val abstractCtor = abstractProto.asInstanceOf[js.Dynamic].constructor

    test[ConcreteJSClass](concreteCtor)
    test[AbstractJSClass](abstractCtor)

    /* TODO When targeting ES6, we cannot yet use indirect calls (with
     * actual varargs) to `js.Dynamic.newInstance` because of
     *   TypeError: Class constructors cannot be invoked without 'new'
     * This will be fixed when we can use ...spread calls with `new`, which
     * we can't yet do because the latest io.js does not support them yet.
     */
    import scala.scalajs.runtime.assumingES6

    val concreteInstance = {
      val tag = js.constructorTag[ConcreteJSClass]
      if (assumingES6)
        js.Dynamic.newInstance(tag.constructor)().asInstanceOf[ConcreteJSClass]
      else
        tag.newInstance()
    }
    assertTrue((concreteInstance: Any).isInstanceOf[ConcreteJSClass])

    val instance = {
      val tag = js.constructorTag[OtherwiseUnreferencedJSClassForTag]
      if (assumingES6) {
        js.Dynamic.newInstance(tag.constructor)(35)
            .asInstanceOf[OtherwiseUnreferencedJSClassForTag]
      } else {
        tag.newInstance(35)
      }
    }
    assertEquals(35, instance.x)
  }

  // scala.scalajs.js.Object

  @Test def should_provide_an_equivalent_to_p_in_o(): Unit = {
    import js.Object.{hasProperty => hasProp}
    val o = js.Dynamic.literal(foo = 5, bar = "foobar").asInstanceOf[js.Object]
    assertTrue(hasProp(o, "foo"))
    assertFalse(hasProp(o, "foobar"))
    assertTrue(hasProp(o, "toString")) // in prototype
  }

  @Test def should_respect_evaluation_order_for_hasProperty(): Unit = {
    import js.Object.{hasProperty => hasProp}
    var indicator = 3
    def o(): js.Object = {
      indicator += 4
      js.Dynamic.literal(x = 5).asInstanceOf[js.Object]
    }
    def p(): String = {
      indicator *= 2
      "x"
    }
    assertTrue(hasProp(o(), p()))
    assertEquals(14, indicator)
  }

  @Test def should_provide_equivalent_of_JS_for_in_loop_of_issue_13(): Unit = {
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

  @Test def should_provide_equivalent_of_JS_for_in_loop2_of_issue_13(): Unit = {
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

  @Test def should_compile_js_undefined(): Unit = {
    assertThrows(classOf[Exception], js.undefined.asInstanceOf[js.Dynamic].toFixed())
  }

  @Test def should_allow_to_define_direct_subtraits_of_js_Any(): Unit = {
    val f = js.Dynamic.literal(
      foo = (x: Int) => x + 1
    ).asInstanceOf[DirectSubtraitOfJSAny]

    assertEquals(6, f.foo(5))
  }

  @Test def should_allow_to_define_direct_subclasses_of_js_Any(): Unit = {
    val f = js.Dynamic.literal(
      bar = (x: Int) => x + 2
    ).asInstanceOf[DirectSubclassOfJSAny]

    assertEquals(7, f.bar(5))
  }

  // Emitted classes

  @Test def should_have_a_meaningful_name_property(): Unit = {
    assumeFalse("Assumed not executing in Rhino", executingInRhino)
    assumeFalse("Assumed not executing in FullOpt", isInFullOpt)

    def nameOf(obj: Any): js.Any =
      obj.asInstanceOf[js.Dynamic].constructor.name

    assertTrue(nameOf(new SomeScalaClass).toString.contains("SomeScalaClass"))
    assertTrue(nameOf(new SomeJSClass).toString.contains("SomeJSClass"))
  }

}

object MiscInteropTest {

  @ScalaJSDefined
  abstract class AbstractJSClass extends js.Object

  @ScalaJSDefined
  class ConcreteJSClass extends AbstractJSClass

  @ScalaJSDefined
  class OtherwiseUnreferencedJSClass(val x: Int) extends js.Object

  @ScalaJSDefined
  class OtherwiseUnreferencedJSClassForTag(val x: Int) extends js.Object

  @js.native
  trait DirectSubtraitOfJSAny extends js.Any {
    def foo(x: Int): Int = js.native
  }

  @JSName("DirectSubclassOfJSAny")
  @js.native
  class DirectSubclassOfJSAny extends js.Any {
    def bar(x: Int): Int = js.native
  }

  class SomeScalaClass

  @ScalaJSDefined
  class SomeJSClass extends js.Object

}
