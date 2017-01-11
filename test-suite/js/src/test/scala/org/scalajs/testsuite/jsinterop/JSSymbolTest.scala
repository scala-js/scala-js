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
import org.junit.{Test, BeforeClass}

class JSSymbolTest {
  import JSSymbolTest._
  import SJSDefinedWithSyms._

  @Test def native_with_defs_that_are_properties(): Unit = {
    val obj = mkObject(sym1 -> 1)

    assertEquals(1, obj.asInstanceOf[PropDefClass].internalDef)
    assertEquals(1, obj.asInstanceOf[PropDefTrait].internalDef)
    assertEquals(1, selectSymbol(obj, sym1))
  }

  @Test def sjsdefined_with_defs_that_are_properties(): Unit = {
    val obj = new SJSDefinedPropDef
    assertEquals(456, obj.internalDef)

    val asTrait: PropDefTrait = obj
    assertEquals(456, asTrait.internalDef)

    assertEquals(456, selectSymbol(obj, sym1))
  }

  @Test def native_with_vals(): Unit = {
    val obj = mkObject(sym1 -> "hi")

    assertEquals("hi", obj.asInstanceOf[PropValClass].internalVal)
    assertEquals("hi", obj.asInstanceOf[PropValTrait].internalVal)
    assertEquals("hi", selectSymbol(obj, sym1))
  }

  @Test def sjsdefined_with_vals(): Unit = {
    val obj = new SJSDefinedPropVal
    assertEquals("hello", obj.internalVal)

    val asTrait: PropValTrait = obj
    assertEquals("hello", asTrait.internalVal)

    assertEquals("hello", selectSymbol(obj, sym1))
  }

  @Test def native_with_vars(): Unit = {
    val obj0 = mkObject(sym1 -> 0.1).asInstanceOf[PropVarClass]
    assertEquals(0.1, obj0.internalVar)
    obj0.internalVar = 0.2
    assertEquals(0.2, obj0.internalVar)

    val obj1 = mkObject(sym1 -> 8.0).asInstanceOf[PropVarTrait]
    assertEquals(8.0, obj1.internalVar)
    obj1.internalVar = 8.2
    assertEquals(8.2, obj1.internalVar)

    val obj2 = mkObject(sym1 -> 8.0)
    assertEquals(8.0, selectSymbol(obj2, sym1))
    updateSymbol(obj2, sym1, 8.2)
    assertEquals(8.2, selectSymbol(obj2, sym1))
  }

  @Test def sjsdefined_with_vars(): Unit = {
    val obj0 = new SJSDefinedPropVar
    assertEquals(1511.1989, obj0.internalVar)
    obj0.internalVar = 0.2
    assertEquals(0.2, obj0.internalVar)

    val obj1: PropVarTrait = new SJSDefinedPropVar
    assertEquals(1511.1989, obj1.internalVar)
    obj1.internalVar = 8.2
    assertEquals(8.2, obj1.internalVar)

    val obj2 = new SJSDefinedPropVar
    assertEquals(1511.1989, selectSymbol(obj2, sym1))
    updateSymbol(obj2, sym1, 8.2)
    assertEquals(8.2, selectSymbol(obj2, sym1))
  }

  @Test def sjsdefined_with_inner_object(): Unit = {
    val obj0 = new SJSDefinedInnerObject
    assertEquals("object", js.typeOf(obj0.innerObject.asInstanceOf[js.Any]))
    assertEquals("SJSDefinedInnerObject.innerObject", obj0.innerObject.toString())

    val obj1: InnerObjectTrait = new SJSDefinedInnerObject
    assertEquals("object", js.typeOf(obj1.innerObject.asInstanceOf[js.Any]))
    assertEquals("SJSDefinedInnerObject.innerObject", obj1.innerObject.toString())

    assertEquals("object",
        js.typeOf(selectSymbol(obj1, sym1).asInstanceOf[js.Any]))
    assertEquals("SJSDefinedInnerObject.innerObject",
        selectSymbol(obj1, sym1).toString())
  }

  @Test def native_with_methods(): Unit = {
    val obj = mkObject(
        sym1 -> ((x: Int) => x + 2),
        sym2 -> ((x: String) => "Hello " + x)
    )

    assertEquals(4, obj.asInstanceOf[MethodClass].foo(2))
    assertEquals("Hello World", obj.asInstanceOf[MethodClass].bar("World"))

    assertEquals(6, obj.asInstanceOf[MethodTrait].foo(4))
    assertEquals("Hello Moon", obj.asInstanceOf[MethodTrait].bar("Moon"))

    assertEquals(6, callSymbol(obj, sym1)(4))
    assertEquals("Hello Moon", callSymbol(obj, sym2)("Moon"))
  }

  @Test def sjsdefined_with_methods(): Unit = {
    val obj = new SJSDefinedMethod
    assertEquals(4, obj.foo(2))
    assertEquals("Hello World", obj.bar("World"))

    val asTrait: MethodTrait = obj
    assertEquals(6, asTrait.foo(4))
    assertEquals("Hello Moon", asTrait.bar("Moon"))

    assertEquals(6, callSymbol(obj, sym1)(4))
    assertEquals("Hello Moon", callSymbol(obj, sym2)("Moon"))
  }

  @Test def native_with_overloaded_methods(): Unit = {
    val obj = mkObject(
        sym1 -> ((x: Int) => x + 3),
        sym2 -> ((x: String) => "Hello " + x)
    )

    assertEquals(5, obj.asInstanceOf[OverloadedMethodClass].foo(2))
    assertEquals("Hello World", obj.asInstanceOf[OverloadedMethodClass].foo("World"))

    assertEquals(6, obj.asInstanceOf[OverloadedMethodTrait].foo(3))
    assertEquals("Hello Moon", obj.asInstanceOf[OverloadedMethodTrait].foo("Moon"))

    assertEquals(6, callSymbol(obj, sym1)(3))
    assertEquals("Hello Moon", callSymbol(obj, sym2)("Moon"))
  }

  @Test def sjsdefined_with_overloaded_methods(): Unit = {
    val obj = new SJSDefinedOverloadedMethod
    assertEquals(5, obj.foo(2))
    assertEquals("Hello World", obj.foo("World"))

    val asTrait: OverloadedMethodTrait = obj
    assertEquals(6, asTrait.foo(3))
    assertEquals("Hello Moon", asTrait.foo("Moon"))

    assertEquals(6, callSymbol(obj, sym1)(3))
    assertEquals("Hello Moon", callSymbol(obj, sym2)("Moon"))
  }

  @Test def native_with_overloaded_runtime_dispatch_methods(): Unit = {
    val obj = mkObject(
        sym1 -> { (x: Any) =>
          x match {
            case x: Int    => x + 3
            case x: String => "Hello " + x
          }
        }
    )

    assertEquals(5,
        obj.asInstanceOf[OverloadedRuntimeDispatchMethodClass].foo(2))
    assertEquals("Hello World",
        obj.asInstanceOf[OverloadedRuntimeDispatchMethodClass].foo("World"))

    assertEquals(6,
        obj.asInstanceOf[OverloadedRuntimeDispatchMethodTrait].foo(3))
    assertEquals("Hello Moon",
        obj.asInstanceOf[OverloadedRuntimeDispatchMethodTrait].foo("Moon"))

    assertEquals(6, callSymbol(obj, sym1)(3))
    assertEquals("Hello Moon", callSymbol(obj, sym1)("Moon"))
  }

  @Test def sjsdefined_with_overloaded_runtime_dispatch_methods(): Unit = {
    val obj = new SJSDefinedOverloadedRuntimeDispatchMethod
    assertEquals(5, obj.foo(2))
    assertEquals("Hello World", obj.foo("World"))

    val asTrait: OverloadedRuntimeDispatchMethodTrait = obj
    assertEquals(6, asTrait.foo(3))
    assertEquals("Hello Moon", asTrait.foo("Moon"))

    assertEquals(6, callSymbol(obj, sym1)(3))
    assertEquals("Hello Moon", callSymbol(obj, sym1)("Moon"))
  }

  @Test def native_with_symbols_in_sjsdefined_object(): Unit = {
    val obj = mkObject(
        sym3 -> ((x: Int) => x + 2)
    )

    assertEquals(65,
        obj.asInstanceOf[ClassWithSymsInSJSDefinedObject].symInSJSDefinedObject(63))
    assertEquals(65,
        obj.asInstanceOf[TraitWithSymsInSJSDefinedObject].symInSJSDefinedObject(63))

    assertEquals(65, callSymbol(obj, sym3)(63))
  }

  @Test def sjsdefined_with_symbols_in_sjsdefined_object(): Unit = {
    val obj = new SJSDefinedWithSymsInSJSDefinedObject
    assertEquals(65, obj.symInSJSDefinedObject(63))

    val asTrait: TraitWithSymsInSJSDefinedObject = obj
    assertEquals(65, asTrait.symInSJSDefinedObject(63))

    assertEquals(65, callSymbol(obj, sym3)(63))
  }

  @Test def native_iterable(): Unit = {
    val obj = mkObject(
        js.Symbol.iterator -> (() => singletonIterator(653))
    )

    assertArrayEquals(Array(653),
        iterableToArray(obj.asInstanceOf[ClassJSIterable[Int]]).toArray)
    assertArrayEquals(Array(653),
        iterableToArray(obj.asInstanceOf[JSIterable[Int]]).toArray)

    val content = Array.newBuilder[Int]
    iterateIterable(obj.asInstanceOf[JSIterable[Int]])(content += _)
    assertArrayEquals(Array(653), content.result())
  }

  @Test def sjsdefined_iterable(): Unit = {
    val obj = new SJSDefinedIterable

    assertArrayEquals(Array(532), iterableToArray(obj).toArray)

    val content = Array.newBuilder[Int]
    iterateIterable(obj)(content += _)
    assertArrayEquals(Array(532), content.result())
  }
}

object JSSymbolTest {

  @BeforeClass
  def beforeClass(): Unit = {
    assumeTrue("Assuming JavaScript symbols are supported",
        org.scalajs.testsuite.utils.Platform.areJSSymbolsSupported)
  }

  /* These need to be lazy vals, so that they do not blow up if there is no
   * symbol support at all.
   */
  lazy val sym1 = js.Symbol()
  lazy val sym2 = js.Symbol()

  @ScalaJSDefined
  object SJSDefinedWithSyms extends js.Object {
    val sym3 = js.Symbol()
  }

  import SJSDefinedWithSyms._

  def mkObject(members: (js.Symbol, js.Any)*): js.Object = {
    val obj = (new js.Object).asInstanceOf[ObjectCreator]
    for ((sym, member) <- members) {
      obj(sym) = member
    }
    obj
  }

  private def selectSymbol(obj: js.Any, sym: js.Symbol): Any =
    obj.asInstanceOf[SymDynamic].selectSymbol(sym)

  private def updateSymbol(obj: js.Any, sym: js.Symbol, value: Any): Unit =
    obj.asInstanceOf[SymDynamic].updateSymbol(sym, value)

  private def callSymbol(obj: js.Any, sym: js.Symbol)(args: js.Any*): Any =
    obj.asInstanceOf[SymDynamic].callSymbol(sym)(args: _*)

  @js.native
  private trait SymDynamic extends js.Any {
    @JSBracketAccess
    def selectSymbol(sym: js.Symbol): Any = js.native

    @JSBracketAccess
    def updateSymbol(sym: js.Symbol, value: Any): Unit = js.native

    @JSBracketCall
    def callSymbol(sym: js.Symbol)(args: js.Any*): Any = js.native
  }

  def singletonIterator(singleton: Any): js.Dynamic = {
    var first = true
    js.Dynamic.literal(
        next = { () =>
          if (first) {
            first = false
            js.Dynamic.literal(value = singleton.asInstanceOf[js.Any],
                done = false)
          } else {
            js.Dynamic.literal(value = (), done = true)
          }
        }
    )
  }

  def iterableToArray[A](iterable: JSIterable[A]): js.Array[A] =
    js.constructorOf[js.Array[_]].from(iterable).asInstanceOf[js.Array[A]]

  def iterateIterable[A](iterable: JSIterable[A])(f: A => Any): Unit = {
    val iterator = iterable.iterator()
    def loop(): Unit = {
      val entry = iterator.next()
      // 2.10 didn't like the use of DynamicImplicits on the following line
      if (!entry.done.asInstanceOf[Boolean]) {
        f(entry.value.asInstanceOf[A])
        loop()
      }
    }
    loop()
  }

  @js.native
  private trait ObjectCreator extends js.Object {
    @JSBracketAccess
    def update(s: js.Symbol, v: js.Any): Unit = js.native
  }

  @ScalaJSDefined
  trait PropDefTrait extends js.Any {
    @JSName(sym1)
    def internalDef: Int
  }

  @js.native
  @JSName("dummy")
  class PropDefClass extends js.Any {
    @JSName(sym1)
    def internalDef: Int = js.native
  }

  @ScalaJSDefined
  class SJSDefinedPropDef extends js.Object with PropDefTrait {
    @JSName(sym1)
    def internalDef: Int = 456
  }

  @ScalaJSDefined
  trait PropValTrait extends js.Any {
    @JSName(sym1)
    val internalVal: String
  }

  @js.native
  @JSName("dummy")
  class PropValClass extends js.Any {
    @JSName(sym1)
    val internalVal: String = js.native
  }

  @ScalaJSDefined
  class SJSDefinedPropVal extends js.Object with PropValTrait {
    @JSName(sym1)
    val internalVal: String = "hello"
  }

  @ScalaJSDefined
  trait PropVarTrait extends js.Any {
    @JSName(sym1)
    var internalVar: Double
  }

  @js.native
  @JSName("dummy")
  class PropVarClass extends js.Any {
    @JSName(sym1)
    var internalVar: Double = js.native
  }

  @ScalaJSDefined
  class SJSDefinedPropVar extends js.Object with PropVarTrait {
    @JSName(sym1)
    var internalVar: Double = 1511.1989
  }

  @ScalaJSDefined
  trait InnerObjectTrait extends js.Any {
    @JSName(sym1)
    val innerObject: AnyRef
  }

  @ScalaJSDefined
  class SJSDefinedInnerObject extends js.Object with InnerObjectTrait {
    @JSName(sym1)
    object innerObject { // scalastyle:ignore
      override def toString(): String = "SJSDefinedInnerObject.innerObject"
    }
  }

  @ScalaJSDefined
  trait MethodTrait extends js.Any {
    @JSName(sym1)
    def foo(x: Int): Int

    @JSName(sym2)
    def bar(x: String): String
  }

  @js.native
  @JSName("dummy")
  class MethodClass extends js.Any {
    @JSName(sym1)
    def foo(x: Int): Int = js.native

    @JSName(sym2)
    def bar(x: String): String = js.native
  }

  @ScalaJSDefined
  class SJSDefinedMethod extends js.Object with MethodTrait {
    @JSName(sym1)
    def foo(x: Int): Int = x + 2

    @JSName(sym2)
    def bar(x: String): String = "Hello " + x
  }

  @ScalaJSDefined
  trait OverloadedMethodTrait extends js.Any {
    @JSName(sym1)
    def foo(x: Int): Int

    @JSName(sym2)
    def foo(x: String): String
  }

  @js.native
  @JSName("dummy")
  class OverloadedMethodClass extends js.Any {
    @JSName(sym1)
    def foo(x: Int): Int = js.native

    @JSName(sym2)
    def foo(x: String): String = js.native
  }

  @ScalaJSDefined
  class SJSDefinedOverloadedMethod extends js.Object with OverloadedMethodTrait {
    @JSName(sym1)
    def foo(x: Int): Int = x + 3

    @JSName(sym2)
    def foo(x: String): String = "Hello " + x
  }

  @ScalaJSDefined
  trait OverloadedRuntimeDispatchMethodTrait extends js.Any {
    @JSName(sym1)
    def foo(x: Int): Int

    @JSName(sym1)
    def foo(x: String): String
  }

  @js.native
  @JSName("dummy")
  class OverloadedRuntimeDispatchMethodClass extends js.Any {
    @JSName(sym1)
    def foo(x: Int): Int = js.native

    @JSName(sym1)
    def foo(x: String): String = js.native
  }

  @ScalaJSDefined
  class SJSDefinedOverloadedRuntimeDispatchMethod
      extends js.Object with OverloadedRuntimeDispatchMethodTrait {
    @JSName(sym1)
    def foo(x: Int): Int = x + 3

    @JSName(sym1)
    def foo(x: String): String = "Hello " + x
  }

  @ScalaJSDefined
  trait TraitWithSymsInSJSDefinedObject extends js.Object {
    @JSName(sym3)
    def symInSJSDefinedObject(x: Int): Int
  }

  @js.native
  @JSName("dummy")
  class ClassWithSymsInSJSDefinedObject extends js.Object {
    @JSName(sym3)
    def symInSJSDefinedObject(x: Int): Int = js.native
  }

  @ScalaJSDefined
  class SJSDefinedWithSymsInSJSDefinedObject
      extends TraitWithSymsInSJSDefinedObject {
    @JSName(sym3)
    def symInSJSDefinedObject(x: Int): Int = x + 2
  }

  @ScalaJSDefined
  trait JSIterable[+A] extends js.Object {
    @JSName(js.Symbol.iterator)
    def iterator(): js.Dynamic
  }

  @js.native
  @JSName("dummy")
  class ClassJSIterable[+A] extends JSIterable[A] {
    @JSName(js.Symbol.iterator)
    def iterator(): js.Dynamic = js.native
  }

  @ScalaJSDefined
  class SJSDefinedIterable extends JSIterable[Int] {
    @JSName(js.Symbol.iterator)
    def iterator(): js.Dynamic = singletonIterator(532)
  }
}
