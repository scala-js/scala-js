/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.jsinterop

import scala.scalajs.js
import js.annotation._

import org.scalajs.testsuite.utils.AssertThrows._
import org.scalajs.testsuite.utils.JSAssert._
import org.scalajs.testsuite.utils.Platform._

import scala.annotation.meta

import org.junit.Assert._
import org.junit.Assume._
import org.junit.Test

import org.scalajs.testsuite.utils.{JSUtils, Platform}
import org.scalajs.testsuite.utils.AssertThrows.assertThrows

class ExportsTest {

  /** The namespace in which top-level exports are stored. */
  val exportsNamespace = scala.scalajs.runtime.environmentInfo.exportsNamespace

  /** This package in the JS (export) namespace */
  val jsPackage = exportsNamespace.org.scalajs.testsuite.jsinterop

  // @JSExport

  @Test def exports_for_methods_with_implicit_name(): Unit = {
    class Foo {
      @JSExport
      def bar(): Int = 42
      @JSExport
      def double(x: Int): Int = x*2
    }

    val foo = (new Foo).asInstanceOf[js.Dynamic]
    assertEquals("function", js.typeOf(foo.bar))
    assertEquals(42, foo.bar())
    assertEquals(6, foo.double(3))
  }

  @Test def exports_for_methods_with_explicit_name(): Unit = {
    class Foo {
      @JSExport("theAnswer")
      def bar(): Int = 42
      @JSExport("doubleTheParam")
      def double(x: Int): Int = x*2
    }

    val foo = (new Foo).asInstanceOf[js.Dynamic]
    assertJSUndefined(foo.bar)
    assertEquals("function", js.typeOf(foo.theAnswer))
    assertEquals(42, foo.theAnswer())
    assertEquals(6, foo.doubleTheParam(3))
  }

  @Test def exports_for_methods_with_constant_folded_name(): Unit = {
    class Foo {
      @JSExport(ExportNameHolder.methodName)
      def bar(): Int = 42
    }

    val foo = (new Foo).asInstanceOf[js.Dynamic]
    assertJSUndefined(foo.bar)
    assertEquals(42, foo.myMethod())
  }

  @Test def exports_for_protected_methods(): Unit = {
    class Foo {
      @JSExport
      protected def bar(): Int = 42

      @JSExport
      protected[testsuite] def foo(): Int = 100
    }

    val foo = (new Foo).asInstanceOf[js.Dynamic]
    assertEquals("function", js.typeOf(foo.bar))
    assertEquals(42, foo.bar())
    assertEquals("function", js.typeOf(foo.foo))
    assertEquals(100, foo.foo())
  }

  @Test def exports_for_properties_with_implicit_name(): Unit = {
    class Foo {
      private[this] var myY: String = "hello"
      @JSExport
      val answer: Int = 42
      @JSExport
      var x: Int = 3
      @JSExport
      def doubleX: Int = x*2
      @JSExport
      def y: String = myY + " get"
      @JSExport
      def y_=(v: String): Unit = myY = v + " set"
    }

    val foo = (new Foo).asInstanceOf[js.Dynamic]
    assertEquals("number", js.typeOf(foo.answer))
    assertEquals(42, foo.answer)
    assertEquals(3, foo.x)
    assertEquals(6, foo.doubleX)
    foo.x = 23
    assertEquals(23, foo.x)
    assertEquals(46, foo.doubleX)
    assertEquals("hello get", foo.y)
    foo.y = "world"
    assertEquals("world set get", foo.y)
  }

  @Test def exports_for_properties_with_explicit_name(): Unit = {
    class Foo {
      private[this] var myY: String = "hello"
      @JSExport("answer")
      val answerScala: Int = 42
      @JSExport("x")
      var xScala: Int = 3
      @JSExport("doubleX")
      def doubleXScala: Int = xScala*2
      @JSExport("y")
      def yGetter: String = myY + " get"
      @JSExport("y")
      def ySetter_=(v: String): Unit = myY = v + " set"
    }

    val foo = (new Foo).asInstanceOf[js.Dynamic]
    assertJSUndefined(foo.answerScala)
    assertEquals("number", js.typeOf(foo.answer))
    assertEquals(42, foo.answer)
    assertEquals(3, foo.x)
    assertEquals(6, foo.doubleX)
    foo.x = 23
    assertEquals(23, foo.x)
    assertEquals(46, foo.doubleX)
    assertEquals("hello get", foo.y)
    foo.y = "world"
    assertEquals("world set get", foo.y)
  }

  @Test def exports_for_protected_properties(): Unit = {
    class Foo {
      @JSExport
      protected val x: Int = 42
      @JSExport
      protected[testsuite] val y: Int = 43
    }

    val foo = (new Foo).asInstanceOf[js.Dynamic]
    assertEquals(42, foo.x)
    assertEquals(43, foo.y)
  }

  @Test def exports_for_abstract_properties_in_class_issue_2513(): Unit = {
    abstract class Foo {
      @JSExport
      val x: Int
      @JSExport
      var y: Int
    }

    class Bar extends Foo {
      val x: Int = 5
      var y: Int = 6
    }

    val bar = (new Bar).asInstanceOf[js.Dynamic]
    assertEquals(5, bar.x)
    assertEquals(6, bar.y)
    bar.y = 7
    assertEquals(7, bar.y)
  }

  @Test def exports_for_abstract_properties_in_trait_issue_2513(): Unit = {
    trait Foo {
      @JSExport
      val x: Int
      @JSExport
      var y: Int
    }

    class Bar extends Foo {
      val x: Int = 5
      var y: Int = 6
    }

    val bar = (new Bar).asInstanceOf[js.Dynamic]
    assertEquals(5, bar.x)
    assertEquals(6, bar.y)
    bar.y = 7
    assertEquals(7, bar.y)
  }

  @Test def readonly_properties(): Unit = {
    assumeFalse(
        "Assuming strict mode semantics, which are not honored by Rhino",
        Platform.executingInRhino)

    class Foo {
      @JSExport
      val foo: Int = 1
      @JSExport
      def bar: Int = 1
    }

    val x: js.Dynamic = (new Foo()).asInstanceOf[js.Dynamic]

    assertThrows(classOf[js.JavaScriptException], {
      x.foo = 2
    })
    assertThrows(classOf[js.JavaScriptException], {
      x.bar = 2
    })
  }

  @Test def properties_are_not_enumerable(): Unit = {
    class Foo {
      @JSExport
      def myProp: Int = 1
    }

    val x: js.Any = (new Foo()).asInstanceOf[js.Any]
    assertFalse(js.Object.properties(x).contains("myProp"))
  }

  @Test def overloaded_exports_for_methods(): Unit = {
    class Foo {
      @JSExport("foobar")
      def foo(): Int = 42
      @JSExport("foobar")
      def bar(x: Int): Int = x*2
    }

    val foo = (new Foo).asInstanceOf[js.Dynamic]
    assertEquals("function", js.typeOf(foo.foobar))
    assertEquals(42, foo.foobar())
    assertEquals(6, foo.foobar(3))
  }

  @Test def multiple_exports_for_the_same_method(): Unit = {
    class Foo {
      @JSExport
      @JSExport("b")
      @JSExport("c")
      def a(): Int = 1
    }

    val foo = (new Foo).asInstanceOf[js.Dynamic]
    assertEquals("function", js.typeOf(foo.a))
    assertEquals("function", js.typeOf(foo.b))
    assertEquals("function", js.typeOf(foo.c))

    assertEquals(1, foo.a())
    assertEquals(1, foo.b())
    assertEquals(1, foo.c())
  }

  @Test def should_inherit_exports_from_traits(): Unit = {
    trait Foo {
      @JSExport
      def x: Int

      @JSExport
      def method(x: Int): Int
    }

    class Bar extends Foo {
      val x = 1
      def method(x: Int): Int = 2 * x
    }

    val bar = (new Bar).asInstanceOf[js.Dynamic]
    assertEquals(1, bar.x)
    assertEquals("function", js.typeOf(bar.method))
    assertEquals(4, bar.method(2))
  }

  @Test def overloading_with_inherited_exports(): Unit = {
    class A {
      @JSExport
      def foo(x: Int): Int = 2*x
    }

    class B extends A{
      @JSExport("foo")
      def bar(x: String): String = s"Hello $x"
    }

    val b = (new B).asInstanceOf[js.Dynamic]
    assertEquals("function", js.typeOf(b.foo))
    assertEquals(2, b.foo(1))
    assertEquals("Hello World", b.foo("World"))
  }

  @Test def exports_for_generic_methods(): Unit = {
    class Foo {
      @JSExport
      def gen[T <: AnyRef](x: T): T = x
    }

    val x = (new Object).asInstanceOf[js.Any]

    val foo = (new Foo).asInstanceOf[js.Dynamic]
    assertEquals("function", js.typeOf(foo.gen))
    assertSame(x, foo.gen(x))
  }

  @Test def exports_for_lambda_return_types(): Unit = {
    class Foo {
      @JSExport
      def lambda(x: Int): Int => Int = (y: Int) => x + y
    }

    val foo = (new Foo).asInstanceOf[js.Dynamic]
    assertEquals("function", js.typeOf(foo.lambda))

    val lambda = foo.lambda(5).asInstanceOf[Function1[Int,Int]]

    assertEquals(9, lambda(4))
  }

  @Test def exports_for_multi_parameter_lists(): Unit = {
    class Foo {
      @JSExport
      def multiParam(x: Int)(y: Int): Int = x + y
    }

    val foo = (new Foo).asInstanceOf[js.Dynamic]
    assertEquals("function", js.typeOf(foo.multiParam))
    assertEquals(11, foo.multiParam(5,6))
  }

  @Test def exports_for_default_arguments(): Unit = {
    class Foo {
      @JSExport
      def defArg(x: Int = 1): Int = x
    }

    val foo = (new Foo).asInstanceOf[js.Dynamic]
    assertEquals("function", js.typeOf(foo.defArg))
    assertEquals(5, foo.defArg(5))
  }

  @Test def exports_for_weird_stuff(): Unit = {
    class UhOh {
      // Something no one should export
      @JSExport
      def ahem[T: Comparable](x: T)(implicit y: Int): Nothing = ???
    }

    val x = (new UhOh).asInstanceOf[js.Dynamic]
    assertEquals("function", js.typeOf(x.ahem))
  }

  @Test def exports_with_value_class_return_types(): Unit = {
    class Foo {
      @JSExport
      def vc(x: Int): SomeValueClass = new SomeValueClass(x)
    }

    val foo = (new Foo).asInstanceOf[js.Dynamic]
    assertEquals("function", js.typeOf(foo.vc))

    // The result should be a boxed SomeValueClass
    val result = foo.vc(5)
    assertEquals("object", js.typeOf(result))
    assertTrue((result: Any).isInstanceOf[SomeValueClass])
    assertTrue((result: Any) == (new SomeValueClass(5)))
  }

  @Test def should_allow_exports_with_Any_as_return_type(): Unit = {
    class A
    class Foo {
      @JSExport
      def foo(switch: Boolean): Any =
        if (switch) 1 else new A
    }

    val foo = (new Foo).asInstanceOf[js.Dynamic]
    assertTrue(foo.foo(true).isInstanceOf[Int])
    assertTrue(foo.foo(false).isInstanceOf[A])
  }

  @Test def boxed_value_classes_as_parameter(): Unit = {
    class Foo {
      @JSExport
      def vc(x: SomeValueClass): Int = x.i
    }

    val foo = (new Foo).asInstanceOf[js.Dynamic]
    assertEquals("function", js.typeOf(foo.vc))

    // The parameter should be a boxed SomeValueClass
    val valueCls = new SomeValueClass(7)
    val result = foo.vc(valueCls.asInstanceOf[js.Any])
    assertEquals("number", js.typeOf(result))
    assertEquals(7, result)
  }

  @Test def should_overload_on_boxed_value_classes_as_parameters(): Unit = {
    class Foo {
      @JSExport
      def foo(x: String): Int = x.length
      @JSExport
      def foo(x: SomeValueClass): Int = x.i
    }

    val foo = (new Foo).asInstanceOf[js.Dynamic]
    val valueCls = new SomeValueClass(7)
    assertEquals(7, foo.foo(valueCls.asInstanceOf[js.Any]))
    assertEquals(5, foo.foo("hello"))
  }

  @Test def exports_for_overridden_methods_with_refined_return_type(): Unit = {
    class A
    class B extends A

    class C1 {
      @JSExport
      def x: A = new A
    }

    class C2 extends C1 {
      override def x: B = new B
    }

    val c2 = (new C2).asInstanceOf[js.Dynamic]
    assertTrue(c2.x.isInstanceOf[B])
  }

  @Test def exports_for_methods_with_refined_types_as_return_type(): Unit = {
    class A {
      @JSExport
      def foo(x: String): js.Object with js.Dynamic =
        js.Dynamic.literal(arg = x)
    }

    val a = (new A).asInstanceOf[js.Dynamic]
    assertEquals(js.Dynamic.literal(arg = "hello").toMap, a.foo("hello").toMap)
  }

  @Test def exports_for_polytype_nullary_method_issue_2445(): Unit = {
    class ExportPolyTypeNullaryMethod {
      @JSExport def emptyArray[T]: js.Array[T] = js.Array()
    }

    val obj = (new ExportPolyTypeNullaryMethod).asInstanceOf[js.Dynamic]
    val a = obj.emptyArray
    assertTrue((a: Any).isInstanceOf[js.Array[_]])
    assertEquals(0, a.length)
  }

  @Test def exports_for_variable_argument_methods_issue_393(): Unit = {
    class A {
      @JSExport
      def foo(i: String*): String = i.mkString("|")
    }

    val a = (new A).asInstanceOf[js.Dynamic]

    assertEquals("", a.foo())
    assertEquals("a|b|c", a.foo("a", "b", "c"))
    assertEquals("a|b|c|d", a.foo("a", "b", "c", "d"))
  }

  @Test def overload_in_view_of_difficult_repeated_parameter_lists(): Unit = {
    class A {
      @JSExport
      def foo(a: String, b: String, i: Int, c: String): Int = 1

      @JSExport
      def foo(a: String*): Int = 2

      @JSExport
      def foo(x: Int)(a: Int*): Int = x * 100000 + a.sum
    }

    val a = (new A).asInstanceOf[js.Dynamic]

    assertEquals(2, a.foo())
    assertEquals(2, a.foo("asdf"))
    assertEquals(2, a.foo("asdf", "foo"))
    assertEquals(2, a.foo("asdf", "foo", "bar"))
    assertEquals(1, a.foo("asdf", "foo", 1, "bar"))
    assertEquals(2, a.foo("asdf", "foo", "foo", "bar"))
    assertEquals(500016, a.foo(5, 1, 2, 3, 10))
    assertEquals(100000, a.foo(1))
  }

  @Test def exports_with_default_arguments(): Unit = {
    class A {
      var oneCount: Int = 0
      def one: Int = {
        oneCount += 1
        1
      }
      @JSExport
      def foo(a: Int = one)(b: Int = a + one)(c: Int = b + one): Int =
        a + b + c
    }

    val a = new A
    val jsa = a.asInstanceOf[js.Dynamic]

    assertEquals(6, jsa.foo())
    assertEquals(3, a.oneCount)

    assertEquals(9, jsa.foo(2))
    assertEquals(5, a.oneCount)

    assertEquals(11, jsa.foo(2,4))
    assertEquals(6, a.oneCount)

    assertEquals(16, jsa.foo(2,4,10))
    assertEquals(6, a.oneCount)

    assertEquals(15, jsa.foo((),4,10))
    assertEquals(7, a.oneCount)

    assertEquals(10, jsa.foo((),4))
    assertEquals(9, a.oneCount)
  }

  @Test def overload_methods_in_presence_of_default_parameters(): Unit = {
    class A {
      @JSExport
      def foo(a: Int)(b: Int = 5)(c: Int = 7): Int = 1000 + a + b + c

      @JSExport
      def foo(a: Int, b: String): Int = 2

      @JSExport
      def foo(a: Int, b: Int, c: String): Int = 3
    }

    val a = (new A).asInstanceOf[js.Dynamic]

    assertEquals(1013, a.foo(1))
    assertEquals(1012, a.foo(1, 4))
    assertEquals(1010, a.foo(1, 4, 5))
    assertEquals(2, a.foo(1, "foo"))
    assertEquals(3, a.foo(1, 2, "foo"))

  }

  @Test def should_prefer_overloads_taking_a_Unit_over_methods_with_default_parameters(): Unit = {
    class A {
      @JSExport
      def foo(a: Int)(b: String = "asdf"): String = s"$a $b"

      @JSExport
      def foo(a: Int, b: Unit): String = "woot"
    }

    val a = (new A).asInstanceOf[js.Dynamic]

    assertEquals("1 asdf", a.foo(1))
    assertEquals("2 omg", a.foo(2, "omg"))
    assertEquals("woot", a.foo(1, ()))

  }

  @Test def overload_methods_in_presence_of_default_parameters_and_repeated_parameters(): Unit = {
    class A {
      @JSExport
      def foo(x: Int, y: Int = 1): Int = x + y
      @JSExport
      def foo(x: String*): String = x.mkString("|")
    }

    val a = (new A).asInstanceOf[js.Dynamic]

    assertEquals(2, a.foo(1))
    assertEquals(3, a.foo(1, 2))
    assertEquals("", a.foo())
    assertEquals("foo", a.foo("foo"))
    assertEquals("foo|bar", a.foo("foo","bar"))

  }

  @Test def overload_exports_called_toString(): Unit = {
    class A {
      override def toString(): String = "no arg"
      @JSExport
      def toString(x: Int): String = s"with arg: $x"
    }

    val a = (new A).asInstanceOf[js.Dynamic]
    assertEquals("no arg", a.applyDynamic("toString")())
    assertEquals("with arg: 1", a.applyDynamic("toString")(1))
  }

  @Test def should_allow_to_explicitly_export_toString(): Unit = {
    class A {
      @JSExport("toString")
      override def toString(): String = "called"
    }

    val a = (new A).asInstanceOf[js.Dynamic]
    assertEquals("called", a.applyDynamic("toString")())
  }

  @Test def box_repeated_parameter_lists_with_value_classes(): Unit = {
    class A {
      @JSExport
      def foo(vcs: SomeValueClass*): Int = vcs.map(_.i).sum
    }

    val vc1 = new SomeValueClass(1)
    val vc2 = new SomeValueClass(2)
    val a = (new A).asInstanceOf[js.Dynamic]

    assertEquals(3, a.foo(vc1.asInstanceOf[js.Any], vc2.asInstanceOf[js.Any]))
  }

  @Test def exports_for_objects_with_implicit_name(): Unit = {
    val accessor = jsPackage.ExportedObject
    assertJSNotUndefined(accessor)
    assertEquals("function", js.typeOf(accessor))
    val obj = accessor()
    assertJSNotUndefined(obj)
    assertEquals("object", js.typeOf(obj))
    assertEquals("witness", obj.witness)
  }

  @Test def exports_for_Scala_js_defined_JS_objects_with_implicit_name(): Unit = {
    val accessor = jsPackage.SJSDefinedExportedObject
    assertJSNotUndefined(accessor)
    assertEquals("function", js.typeOf(accessor))
    val obj = accessor()
    assertJSNotUndefined(obj)
    assertEquals("object", js.typeOf(obj))
    assertEquals("witness", obj.witness)
  }

  @Test def exports_for_objects_with_explicit_name(): Unit = {
    val accessor = exportsNamespace.TheExportedObject
    assertJSNotUndefined(accessor)
    assertEquals("function", js.typeOf(accessor))
    val obj = accessor()
    assertJSNotUndefined(obj)
    assertEquals("object", js.typeOf(obj))
    assertEquals("witness", obj.witness)
  }

  @Test def toplevel_exports_for_objects(): Unit = {
    val obj = exportsNamespace.TopLevelExportedObject
    assertJSNotUndefined(obj)
    assertEquals("object", js.typeOf(obj))
    assertEquals("witness", obj.witness)
  }

  @Test def exports_for_Scala_js_defined_JS_objects_with_explicit_name(): Unit = {
    val accessor = exportsNamespace.TheSJSDefinedExportedObject
    assertJSNotUndefined(accessor)
    assertEquals("function", js.typeOf(accessor))
    val obj = accessor()
    assertJSNotUndefined(obj)
    assertEquals("object", js.typeOf(obj))
    assertEquals("witness", obj.witness)
  }

  @Test def toplevel_exports_for_Scala_js_defined_JS_objects(): Unit = {
    val obj1 = exportsNamespace.SJSDefinedTopLevelExportedObject
    assertJSNotUndefined(obj1)
    assertEquals("object", js.typeOf(obj1))
    assertEquals("witness", obj1.witness)

    val obj2 = exportsNamespace.TheSJSDefinedTopLevelExportedObject
    assertSame(obj1, obj2)
  }

  @Test def exports_for_objects_with_qualified_name(): Unit = {
    val accessor = exportsNamespace.qualified.testobject.ExportedObject
    assertJSNotUndefined(accessor)
    assertEquals("function", js.typeOf(accessor))
    val obj = accessor()
    assertJSNotUndefined(obj)
    assertEquals("object", js.typeOf(obj))
    assertEquals("witness", obj.witness)
  }

  @Test def toplevel_exports_for_objects_with_qualified_name(): Unit = {
    val obj = exportsNamespace.qualified.testobject.TopLevelExportedObject
    assertJSNotUndefined(obj)
    assertEquals("object", js.typeOf(obj))
    assertEquals("witness", obj.witness)
  }

  @Test def exports_for_nested_objects(): Unit = {
    val accessor = exportsNamespace.qualified.nested.ExportedObject
    assertJSNotUndefined(accessor)
    assertEquals("function", js.typeOf(accessor))
    val obj = accessor()
    assertJSNotUndefined(obj)
    assertEquals("object", js.typeOf(obj))
    assertSame(obj, ExportHolder.ExportedObject)
  }

  @Test def toplevel_exports_for_nested_objects(): Unit = {
    val obj = exportsNamespace.qualified.nested.TopLevelExportedObject
    assertJSNotUndefined(obj)
    assertEquals("object", js.typeOf(obj))
    assertSame(obj, ExportHolder.TopLevelExportedObject)
  }

  @Test def exports_for_objects_with_constant_folded_name(): Unit = {
    val accessor = exportsNamespace.ConstantFoldedObjectExport
    assertJSNotUndefined(accessor)
    assertEquals("function", js.typeOf(accessor))
    val obj = accessor()
    assertJSNotUndefined(obj)
    assertEquals("object", js.typeOf(obj))
    assertEquals("witness", obj.witness)
  }

  @Test def exports_for_protected_objects(): Unit = {
    val accessor = jsPackage.ProtectedExportedObject
    assertJSNotUndefined(accessor)
    assertEquals("function", js.typeOf(accessor))
    val obj = accessor()
    assertJSNotUndefined(obj)
    assertEquals("object", js.typeOf(obj))
    assertEquals("witness", obj.witness)
  }

  @Test def exports_for_classes_with_implicit_name(): Unit = {
    val constr = jsPackage.ExportedClass
    assertJSNotUndefined(constr)
    assertEquals("function", js.typeOf(constr))
    val obj = js.Dynamic.newInstance(constr)(5)
    assertEquals(5, obj.x)
  }

  @Test def exports_for_Scala_js_defined_JS_classes_with_implicit_name(): Unit = {
    val constr = jsPackage.SJSDefinedExportedClass
    assertJSNotUndefined(constr)
    assertEquals("function", js.typeOf(constr))
    val obj = js.Dynamic.newInstance(constr)(5)
    assertTrue((obj: Any).isInstanceOf[SJSDefinedExportedClass])
    assertEquals(5, obj.x)
  }

  @Test def exports_for_classes_with_explicit_name(): Unit = {
    val constr = exportsNamespace.TheExportedClass
    assertJSNotUndefined(constr)
    assertEquals("function", js.typeOf(constr))
    val obj = js.Dynamic.newInstance(constr)(5)
    assertEquals(5, obj.x)
  }

  @Test def toplevel_exports_for_classes(): Unit = {
    val constr = exportsNamespace.TopLevelExportedClass
    assertJSNotUndefined(constr)
    assertEquals("function", js.typeOf(constr))
    val obj = js.Dynamic.newInstance(constr)(5)
    assertEquals(5, obj.x)
  }

  @Test def exports_for_Scala_js_defined_JS_classes_with_explicit_name(): Unit = {
    val constr = exportsNamespace.TheSJSDefinedExportedClass
    assertJSNotUndefined(constr)
    assertEquals("function", js.typeOf(constr))
    val obj = js.Dynamic.newInstance(constr)(5)
    assertTrue((obj: Any).isInstanceOf[SJSDefinedExportedClass])
    assertEquals(5, obj.x)
  }

  @Test def toplevel_exports_for_Scala_js_defined_JS_classes(): Unit = {
    val constr = exportsNamespace.SJSDefinedTopLevelExportedClass
    assertJSNotUndefined(constr)
    assertEquals("function", js.typeOf(constr))
    val obj = js.Dynamic.newInstance(constr)(5)
    assertTrue((obj: Any).isInstanceOf[SJSDefinedTopLevelExportedClass])
    assertEquals(5, obj.x)

    val constr2 = exportsNamespace.TheSJSDefinedTopLevelExportedClass
    assertSame(constr, constr2)
  }

  @Test def exports_for_classes_with_qualified_name_ExportedClass(): Unit = {
    val constr = exportsNamespace.qualified.testclass.ExportedClass
    assertJSNotUndefined(constr)
    assertEquals("function", js.typeOf(constr))
    val obj = js.Dynamic.newInstance(constr)(5)
    assertEquals(5, obj.x)
  }

  @Test def toplevel_exports_for_classes_with_qualified_name(): Unit = {
    val constr = exportsNamespace.qualified.testclass.TopLevelExportedClass
    assertJSNotUndefined(constr)
    assertEquals("function", js.typeOf(constr))
    val obj = js.Dynamic.newInstance(constr)(5)
    assertEquals(5, obj.x)
  }

  @Test def exports_for_nested_classes(): Unit = {
    val constr = exportsNamespace.qualified.nested.ExportedClass
    assertJSNotUndefined(constr)
    assertEquals("function", js.typeOf(constr))
    val obj = js.Dynamic.newInstance(constr)()
    assertTrue((obj: Any).isInstanceOf[ExportHolder.ExportedClass])
  }

  @Test def toplevel_exports_for_nested_classes(): Unit = {
    val constr = exportsNamespace.qualified.nested.TopLevelExportedClass
    assertJSNotUndefined(constr)
    assertEquals("function", js.typeOf(constr))
    val obj = js.Dynamic.newInstance(constr)()
    assertTrue((obj: Any).isInstanceOf[ExportHolder.TopLevelExportedClass])
  }

  @Test def exports_for_classes_with_qualified_name_SJSDefinedExportedClass(): Unit = {
    val constr = exportsNamespace.qualified.testclass.SJSDefinedExportedClass
    assertJSNotUndefined(constr)
    assertEquals("function", js.typeOf(constr))
    val obj = js.Dynamic.newInstance(constr)(5)
    assertTrue((obj: Any).isInstanceOf[SJSDefinedExportedClass])
    assertEquals(5, obj.x)
  }

  @Test def exports_for_nested_sjs_defined_classes(): Unit = {
    val constr = exportsNamespace.qualified.nested.SJSDefinedExportedClass
    assertJSNotUndefined(constr)
    assertEquals("function", js.typeOf(constr))
    val obj = js.Dynamic.newInstance(constr)()
    assertTrue((obj: Any).isInstanceOf[ExportHolder.SJSDefinedExportedClass])
  }

  @Test def exports_for_classes_with_constant_folded_name(): Unit = {
    val constr = exportsNamespace.ConstantFoldedClassExport
    assertJSNotUndefined(constr)
    assertEquals("function", js.typeOf(constr))
    val obj = js.Dynamic.newInstance(constr)(5)
    assertEquals(5, obj.x)
  }

  @Test def exports_for_protected_classes(): Unit = {
    val constr = jsPackage.ProtectedExportedClass
    assertJSNotUndefined(constr)
    assertEquals("function", js.typeOf(constr))
    val obj = js.Dynamic.newInstance(constr)(5)
    assertEquals(5, obj.x)
  }

  @Test def export_for_classes_with_repeated_parameters_in_ctor(): Unit = {
    val constr = jsPackage.ExportedVarArgClass
    assertEquals("", js.Dynamic.newInstance(constr)().result)
    assertEquals("a", js.Dynamic.newInstance(constr)("a").result)
    assertEquals("a|b", js.Dynamic.newInstance(constr)("a", "b").result)
    assertEquals("a|b|c", js.Dynamic.newInstance(constr)("a", "b", "c").result)
    assertEquals("Number: <5>|a", js.Dynamic.newInstance(constr)(5, "a").result)
  }

  @Test def export_for_classes_with_default_parameters_in_ctor(): Unit = {
    val constr = jsPackage.ExportedDefaultArgClass
    assertEquals(6, js.Dynamic.newInstance(constr)(1,2,3).result)
    assertEquals(106, js.Dynamic.newInstance(constr)(1).result)
    assertEquals(103, js.Dynamic.newInstance(constr)(1,2).result)
  }

  @Test def disambiguate_overloads_involving_longs(): Unit = {

    class Foo {
      @JSExport
      def foo(x: Int): Int = 1
      @JSExport
      def foo(x: Long): Int = 2
    }

    val foo = (new Foo).asInstanceOf[js.Dynamic]

    // Create a long factory we can call dynamically to retrieve an unboxed
    // long which is typed as a js.Any
    object LongFactory {
      @JSExport
      def aLong: Long = 1L
    }
    val trueJsLong = LongFactory.asInstanceOf[js.Dynamic].aLong

    assertEquals(1, foo.foo(1))
    assertEquals(2, foo.foo(trueJsLong))
  }

  @Test def should_return_boxed_Chars(): Unit = {
    class Foo {
      @JSExport
      def bar(x: Int): Char = x.toChar
    }
    val foo = (new Foo).asInstanceOf[js.Dynamic]

    val funs = js.eval("""
        var funs = {
          testIsChar: function(JSUtils, foo) { return JSUtils.isChar(foo.bar(65)); },
          testCharValue: function(JSUtils, foo) { return JSUtils.charToString(foo.bar(65)); }
        }; funs;
        """).asInstanceOf[js.Dynamic]

    assertTrue(funs.testIsChar(JSUtils, foo).asInstanceOf[Boolean])
    assertEquals("A", funs.testCharValue(JSUtils, foo))
  }

  @Test def should_take_boxed_Chars_as_parameter(): Unit = {
    class Foo {
      @JSExport
      def bar(x: Char): Int = x.toInt
    }
    val foo = (new Foo).asInstanceOf[js.Dynamic]

    val f = js.eval("""
        var f = function(JSUtils, foo) { return foo.bar(JSUtils.stringToChar('e')); };
        f;
        """).asInstanceOf[js.Dynamic]

    assertEquals('e'.toInt, f(JSUtils, foo))
  }

  @Test def should_be_able_to_disambiguate_an_Int_from_a_Char(): Unit = {
    class Foo {
      @JSExport
      def bar(x: Char): String = "char: "+x
      @JSExport
      def bar(x: Int): String = "int: "+x
    }
    val foo = (new Foo).asInstanceOf[js.Dynamic]

    val funs = js.eval("""
        var funs = {
          testChar: function(JSUtils, foo) { return foo.bar(JSUtils.stringToChar('S')); },
          testInt: function(foo) { return foo.bar(68); }
        }; funs;
        """).asInstanceOf[js.Dynamic]

    assertEquals("char: S", funs.testChar(JSUtils, foo))
    assertEquals("int: 68", funs.testInt(foo))
  }

  @Test def exporting_constructor_parameter_fields_issue_970(): Unit = {
    class Foo(@JSExport val x: Int, @JSExport var y: Int)

    val foo = new Foo(5, 6).asInstanceOf[js.Dynamic]
    assertEquals(5, foo.x)
    assertEquals(6, foo.y)
    foo.y = 7
    assertEquals(7, foo.y)
  }

  @Test def exporting_case_class_fields_issue_970(): Unit = {
    case class Bar(@JSExport x: Int, @JSExport var y: Int)

    val bar = Bar(5, 6).asInstanceOf[js.Dynamic]
    assertEquals(5, bar.x)
    assertEquals(6, bar.y)
    bar.y = 7
    assertEquals(7, bar.y)
  }

  @Test def exporting_constructor_parameter_fields_issue_970_old(): Unit = {
    class Foo(@(JSExport @meta.field) val x: Int)
    val foo = (new Foo(1)).asInstanceOf[js.Dynamic]
    assertEquals(1, foo.x)
  }

  @Test def exporting_case_class_fields_issue_970_old(): Unit = {
    case class Foo(@(JSExport @meta.field) x: Int)
    val foo = (new Foo(1)).asInstanceOf[js.Dynamic]
    assertEquals(1, foo.x)
  }

  @Test def exporting_lazy_values_issue_977(): Unit = {
    class Foo {
      @JSExport
      lazy val x = 1
    }
    val foo = (new Foo).asInstanceOf[js.Dynamic]
    assertEquals(1, foo.x)
  }

  @Test def exporting_all_members_of_a_class(): Unit = {
    @JSExportAll
    class Foo {
      val a = 1

      @JSExport // double annotation allowed
      def b: Int = 2

      lazy val c = 3

      class Bar // not exported, but should not fail
    }

    val foo = (new Foo).asInstanceOf[js.Dynamic]

    assertEquals(1, foo.a)
    assertEquals(2, foo.b)
    assertEquals(3, foo.c)
  }

  @Test def should_not_export_synthetic_members_with_atJSExportAll_issue_1195(): Unit = {
    @JSExportAll
    case class Foo(x: Int)

    val foo = Foo(1).asInstanceOf[js.Dynamic]

    assertEquals(1, foo.x)
    assertJSUndefined(foo.copy)
  }

  @Test def should_allow_mutliple_equivalent_JSExport_annotations(): Unit = {
    class Foo {
      @JSExport
      @JSExport("a")
      @JSExport
      @JSExport("a")
      def b: Int = 1
    }

    val foo = (new Foo).asInstanceOf[js.Dynamic]

    assertEquals(1, foo.b)
  }

  @Test def named_exports(): Unit = {
    import js.Dynamic.{literal => lit}

    class FooNamed {
      @JSExportNamed("bar1")
      def bar(x: Int, y: Int): Int = x + y

      @JSExportNamed("bar2")
      @JSExport
      def bar(x: Int = 1)(y: Int = x)(z: Int = y): Int = x + y + z
    }

    val foo = (new FooNamed).asInstanceOf[js.Dynamic]

    assertEquals(3, foo.bar1(lit(x = 1, y = 2)))
    if (hasCompliantAsInstanceOfs)
      assertThrows(classOf[Exception], foo.bar1(lit(x = 1)))// missing arg
    assertEquals(3, foo.bar2(lit()))
    assertEquals(6, foo.bar2(lit(x = 2)))
    assertEquals(5, foo.bar2(lit(y = 2)))
    assertEquals(4, foo.bar2(lit(y = 2, z = 1)))
    assertEquals(6, foo.bar(2))
    assertEquals(8, foo.bar(2,3))
  }

  @Test def named_constructor_exports(): Unit = {
    import js.Dynamic.{literal => lit}

    val constr = jsPackage.ExportedNamedArgClass
    val result1 = js.Dynamic.newInstance(constr)(lit(x = 2)).result
    assertEquals("22true", result1)
    val result2 = js.Dynamic.newInstance(constr)(lit(y = "foo")).result
    assertEquals("1foofalse", result2)
    val result3 = js.Dynamic.newInstance(constr)(lit(z = true, y = "foo")).result
    assertEquals("1footrue", result3)
  }

  @Test def exporting_under_org_namespace_issue_364(): Unit = {
    val accessor = exportsNamespace.org.ExportedUnderOrgObject
    assertEquals("function", js.typeOf(accessor))
    val obj = accessor()
    assertSame(ExportedUnderOrgObject.asInstanceOf[js.Any], obj)
  }

  @Test def null_for_arguments_of_primitive_value_type_issue_1719(): Unit = {
    @JSExportAll
    class Foo {
      def doBool(x: Boolean): Unit = assertTrue((x: Any) == false) // scalastyle:ignore
      def doChar(x: Char): Unit = assertTrue(x.equals('\0'))
      def doByte(x: Byte): Unit = assertEquals(0, x)
      def doShort(x: Short): Unit = assertEquals(0, x)
      def doInt(x: Int): Unit = assertEquals(0, x)
      def doLong(x: Long): Unit = assertTrue(x.equals(0L))
      def doFloat(x: Float): Unit = assertEquals(0.0f, x)
      def doDouble(x: Double): Unit = assertEquals(0.0, x)
      def doUnit(x: Unit): Unit = assertTrue((x: Any) == null)
    }

    val foo = (new Foo).asInstanceOf[js.Dynamic]

    foo.doBool(null)
    foo.doChar(null)
    foo.doByte(null)
    foo.doShort(null)
    foo.doInt(null)
    foo.doLong(null)
    foo.doFloat(null)
    foo.doDouble(null)
    foo.doUnit(null)
  }

  @Test def should_reject_bad_values_for_arguments_of_primitive_value_type(): Unit = {
    assumeTrue("Assumed compliant asInstanceOf", hasCompliantAsInstanceOfs)

    @JSExportAll
    class Foo {
      def doBool(x: Boolean): Boolean = x
      def doChar(x: Char): Char = x
      def doByte(x: Byte): Byte = x
      def doShort(x: Short): Short = x
      def doInt(x: Int): Int = x
      def doLong(x: Long): Long = x
      def doFloat(x: Float): Float = x
      def doDouble(x: Double): Double = x
      def doUnit(x: Unit): Unit = x
    }

    val foo = (new Foo).asInstanceOf[js.Dynamic]

    // Class type
    assertThrows(classOf[Exception], foo.doBool(foo))
    assertThrows(classOf[Exception], foo.doChar(foo))
    assertThrows(classOf[Exception], foo.doByte(foo))
    assertThrows(classOf[Exception], foo.doShort(foo))
    assertThrows(classOf[Exception], foo.doInt(foo))
    assertThrows(classOf[Exception], foo.doLong(foo))
    assertThrows(classOf[Exception], foo.doFloat(foo))
    assertThrows(classOf[Exception], foo.doDouble(foo))
    assertThrows(classOf[Exception], foo.doUnit(foo))

    // Bad values
    assertThrows(classOf[Exception], foo.doBool(1))
    assertThrows(classOf[Exception], foo.doBool("a"))

    assertThrows(classOf[Exception], foo.doChar(1))
    assertThrows(classOf[Exception], foo.doChar("a"))

    assertThrows(classOf[Exception], foo.doByte(300))
    assertThrows(classOf[Exception], foo.doByte("a"))

    assertThrows(classOf[Exception], foo.doShort(32768))
    assertThrows(classOf[Exception], foo.doShort("a"))

    assertThrows(classOf[Exception], foo.doInt(3.2))
    assertThrows(classOf[Exception], foo.doInt("a"))

    assertThrows(classOf[Exception], foo.doLong(3.2))
    assertThrows(classOf[Exception], foo.doLong(3))
    assertThrows(classOf[Exception], foo.doLong("a"))

    assertThrows(classOf[Exception], foo.doFloat("a"))
  }

  @Test def should_reject_bad_values_for_arguments_of_value_class_type_issue_613(): Unit = {
    assumeTrue("Assumed compliant asInstanceOf", hasCompliantAsInstanceOfs)

    class Foo {
      @JSExport
      def doVC(x: SomeValueClass): SomeValueClass = x
    }

    val foo = (new Foo).asInstanceOf[js.Dynamic]

    assertThrows(classOf[Exception], foo.doVC(null))
    assertThrows(classOf[Exception], foo.doVC(foo))
    assertThrows(classOf[Exception], foo.doVC(1))
    assertThrows(classOf[Exception], foo.doVC("a"))
  }

  @Test def should_reject_bad_values_for_arguments_of_class_type(): Unit = {
    assumeTrue("Assumed compliant asInstanceOf", hasCompliantAsInstanceOfs)

    class A
    class B

    class Foo {
      @JSExport
      def doA(x: A): A = x
    }

    val foo = (new Foo).asInstanceOf[js.Dynamic]

    assertThrows(classOf[Exception], foo.doA(1))
    assertThrows(classOf[Exception], foo.doA((new B).asInstanceOf[js.Any]))
    assertThrows(classOf[Exception], foo.doA("a"))
  }

  @Test def `exports_for_classes_ending_in__=_issue_1090`(): Unit = {
    val constr = jsPackage.ExportClassSetterNamed_=
    val obj = js.Dynamic.newInstance(constr)()
    assertEquals(obj.x, 1)
  }

  @Test def `exports_for_objects_ending_in__=_issue_1090`(): Unit = {
    assertEquals(jsPackage.ExportObjSetterNamed_=().x, 1)
  }

  @Test def should_expose_public_members_of_new_js_Object_issue_1899(): Unit = {

    // Test that the bug is fixed for js.Any classes.

    def testExposure(obj: js.Object): Unit = {
      assertJSNotUndefined(obj)
      assertTrue(obj.hasOwnProperty("x1"))
      assertTrue(obj.hasOwnProperty("y1"))
      assertFalse(obj.hasOwnProperty("x2"))
      assertFalse(obj.hasOwnProperty("y2"))
      assertFalse(obj.hasOwnProperty("x3"))
      assertFalse(obj.hasOwnProperty("y3"))

      val dynObj = obj.asInstanceOf[js.Dynamic]
      assertEquals("x1", dynObj.x1)
      assertJSUndefined(dynObj.x2)
      assertJSUndefined(dynObj.x3)

      assertEquals("y1", dynObj.y1)
      assertJSUndefined(dynObj.y2)
      assertJSUndefined(dynObj.y3)

      assertEquals("z1", dynObj.z1())
      assertJSUndefined(dynObj.z2)
      assertJSUndefined(dynObj.z2)
      assertJSUndefined(dynObj.z3)

      dynObj.y1 = "y1+"
      dynObj.y2 = "y2+"
      dynObj.y3 = "y3+"
      assertEquals("y1+", dynObj.y1)
      assertEquals("y2+", dynObj.y2)
      assertEquals("y3+", dynObj.y3)
      assertEquals("y1+", dynObj.checkOriginalY1())
      assertEquals("y2", dynObj.checkOriginalY2())
      assertEquals("y3", dynObj.checkOriginalY3())
    }

    def getJSObj(): js.Object = new js.Object {
      val x1 = "x1"
      var y1 = "y1"
      def z1() = "z1"
      private val x2 = "x2"
      private var y2 = "y2"
      private def z2() = "z2"
      private[this] val x3 = "x3"
      private[this] var y3 = "y3"
      private[this] def z3() = "z3"
      def checkOriginalY1() = y1
      def checkOriginalY2() = y2
      def checkOriginalY3() = y3
    }

    @ScalaJSDefined
    class JSClass extends js.Object

    def getJSObj2(): js.Object = new JSClass {
      val x1 = "x1"
      var y1 = "y1"
      def z1() = "z1"
      private val x2 = "x2"
      private var y2 = "y2"
      private def z2() = "z2"
      private[this] val x3 = "x3"
      private[this] var y3 = "y3"
      private[this] def z3() = "z3"
      def checkOriginalY1() = y1
      def checkOriginalY2() = y2
      def checkOriginalY3() = y3
    }

    @ScalaJSDefined
    abstract class JSAbstractClass extends js.Object

    def getJSObj3(): js.Object = new JSAbstractClass {
      val x1 = "x1"
      var y1 = "y1"
      def z1() = "z1"
      private val x2 = "x2"
      private var y2 = "y2"
      private def z2() = "z2"
      private[this] val x3 = "x3"
      private[this] var y3 = "y3"
      private[this] def z3() = "z3"
      def checkOriginalY1() = y1
      def checkOriginalY2() = y2
      def checkOriginalY3() = y3
    }

    @ScalaJSDefined
    abstract class JSTrait extends js.Object

    def getJSObj4(): js.Object = new JSTrait {
      val x1 = "x1"
      var y1 = "y1"
      def z1() = "z1"
      private val x2 = "x2"
      private var y2 = "y2"
      private def z2() = "z2"
      private[this] val x3 = "x3"
      private[this] var y3 = "y3"
      private[this] def z3() = "z3"
      def checkOriginalY1() = y1
      def checkOriginalY2() = y2
      def checkOriginalY3() = y3
    }

    testExposure(getJSObj())
    testExposure(getJSObj2())
    testExposure(getJSObj3())
    testExposure(getJSObj4())

    // Test that non js.Any classes were unaffected by the fix.

    def getObj(): AnyRef = new {
      val x1 = "x1"
      var y1 = "y1"
      def z1() = "z1"
      private val x2 = "x2"
      private var y2 = "y2"
      private def z2() = "z2"
      private[this] val x3 = "x3"
      private[this] var y3 = "y3"
      private[this] def z3() = "z3"
    }

    import scala.language.reflectiveCalls

    val obj2 = getObj().asInstanceOf[{ val x1: String; var y1: String; def z1(): String }]

    assertThrows(classOf[Throwable], obj2.x1)
    assertThrows(classOf[Throwable], obj2.y1)
    assertThrows(classOf[Throwable], obj2.y1 = "y1+")
    assertThrows(classOf[Throwable], obj2.z1)
  }

  // @JSExportTopLevel

  @Test def basic_top_level_export(): Unit = {
    assertEquals(1, jsPackage.toplevel.basic())
  }

  @Test def overloaded_top_level_export(): Unit = {
    assertEquals("Hello World", jsPackage.toplevel.overload("World"))
    assertEquals(2, jsPackage.toplevel.overload(2))
    assertEquals(9, jsPackage.toplevel.overload(2, 7))
    assertEquals(10, jsPackage.toplevel.overload(1, 2, 3, 4))
  }

  @Test def top_level_export_uses_unique_object(): Unit = {
    jsPackage.toplevel.set(3)
    assertEquals(3, TopLevelExports.myVar)
    jsPackage.toplevel.set(7)
    assertEquals(7, TopLevelExports.myVar)
  }

  @Test def top_level_export_from_nested_object(): Unit = {
    jsPackage.toplevel.setNested(28)
    assertEquals(28, TopLevelExports.Nested.myVar)
  }

  @Test def top_level_export_is_always_reachable(): Unit = {
    assertEquals("Hello World", jsPackage.toplevel.reachability())
  }

  // @JSExportTopLevel fields

  @Test def top_level_export_basic_field(): Unit = {
    // Initialization
    assertEquals(5, jsPackage.toplevel.basicVal)
    assertEquals("hello", jsPackage.toplevel.basicVar)

    // Scala modifies var
    TopLevelFieldExports.basicVar = "modified"
    assertEquals("modified", TopLevelFieldExports.basicVar)
    assertEquals("modified", jsPackage.toplevel.basicVar)

    // Reset var
    TopLevelFieldExports.basicVar = "hello"
  }

  @Test def top_level_export_field_twice(): Unit = {
    // Initialization
    assertEquals(5, jsPackage.toplevel.valExportedTwice1)
    assertEquals("hello", jsPackage.toplevel.varExportedTwice1)
    assertEquals("hello", jsPackage.toplevel.varExportedTwice2)

    // Scala modifies var
    TopLevelFieldExports.varExportedTwice = "modified"
    assertEquals("modified", TopLevelFieldExports.varExportedTwice)
    assertEquals("modified", jsPackage.toplevel.varExportedTwice1)
    assertEquals("modified", jsPackage.toplevel.varExportedTwice2)

    // Reset var
    TopLevelFieldExports.varExportedTwice = "hello"
  }

  @Test def top_level_export_write_val_var_causes_typeerror(): Unit = {
    assumeFalse("Assuming strict mode, not supported by Rhino",
        Platform.executingInRhino)

    assertThrows(classOf[js.JavaScriptException], {
      jsPackage.toplevel.basicVal = 54
    })

    assertThrows(classOf[js.JavaScriptException], {
      jsPackage.toplevel.basicVar = 54
    })
  }

  @Test def top_level_export_uninitialized_fields(): Unit = {
    assertEquals(0, TopLevelFieldExports.uninitializedVarInt)
    assertEquals(null, jsPackage.toplevel.uninitializedVarInt)

    assertEquals(0L, TopLevelFieldExports.uninitializedVarLong)
    assertEquals(null, jsPackage.toplevel.uninitializedVarLong)

    assertEquals(null, TopLevelFieldExports.uninitializedVarString)
    assertEquals(null, jsPackage.toplevel.uninitializedVarString)

    assertEquals('\0', TopLevelFieldExports.uninitializedVarChar)
    assertEquals(null, jsPackage.toplevel.uninitializedVarChar)
  }

  @Test def top_level_export_field_is_always_reachable_and_initialized(): Unit = {
    assertEquals("Hello World", jsPackage.toplevel.fieldreachability)
  }

  // @JSExportDescendentObjects

  @Test def auto_exports_for_objects_extending_a_trait(): Unit = {
    val accessor =
      exportsNamespace.org.scalajs.testsuite.jsinterop.AutoExportedTraitObject
    assertJSNotUndefined(accessor)
    assertEquals("function", js.typeOf(accessor))
    val obj = accessor()
    assertJSNotUndefined(obj)
    assertSame(AutoExportedTraitObject.asInstanceOf[js.Any], obj)
  }

  @Test def auto_exports_for_objects_extending_a_class(): Unit = {
    val accessor =
      exportsNamespace.org.scalajs.testsuite.jsinterop.AutoExportedClassObject
    assertJSNotUndefined(accessor)
    assertEquals("function", js.typeOf(accessor))
    val obj = accessor()
    assertJSNotUndefined(obj)
    assertSame(AutoExportedClassObject.asInstanceOf[js.Any], obj)
  }

  @Test def auto_exports_for_Scala_js_defined_JS_objects_extending_a_trait(): Unit = {
    val accessor =
      exportsNamespace.org.scalajs.testsuite.jsinterop.SJSDefinedAutoExportedTraitObject
    assertJSNotUndefined(accessor)
    assertEquals("function", js.typeOf(accessor))
    val obj = accessor()
    assertJSNotUndefined(obj)
    assertSame(SJSDefinedAutoExportedTraitObject, obj)
  }

  @Test def auto_exports_for_Scala_js_defined_JS_objects_extending_a_class(): Unit = {
    val accessor =
      exportsNamespace.org.scalajs.testsuite.jsinterop.SJSDefinedAutoExportedClassObject
    assertJSNotUndefined(accessor)
    assertEquals("function", js.typeOf(accessor))
    val obj = accessor()
    assertJSNotUndefined(obj)
    assertSame(SJSDefinedAutoExportedClassObject, obj)
  }

  @Test def auto_exports_for_objects_extending_a_trait_with_ignoreInvalidDescendants(): Unit = {
    val accessor =
      exportsNamespace.org.scalajs.testsuite.jsinterop.AutoExportIgnoreTraitObject
    assertJSNotUndefined(accessor)
    assertEquals("function", js.typeOf(accessor))
    val obj = accessor()
    assertJSNotUndefined(obj)
    assertSame(AutoExportIgnoreTraitObject.asInstanceOf[js.Any], obj)
  }

  @Test def auto_exports_for_objects_extending_a_class_with_ignoreInvalidDescendants(): Unit = {
    val accessor =
      exportsNamespace.org.scalajs.testsuite.jsinterop.AutoExportIgnoreClassObject
    assertJSNotUndefined(accessor)
    assertEquals("function", js.typeOf(accessor))
    val obj = accessor()
    assertJSNotUndefined(obj)
    assertSame(AutoExportIgnoreClassObject.asInstanceOf[js.Any], obj)
  }

  @Test def auto_exports_for_Scala_js_defined_JS_objects_extending_a_class_with_ignoreInvalidDescendants(): Unit = {
    val accessor =
      exportsNamespace.org.scalajs.testsuite.jsinterop.SJSDefinedAutoExportedIgnoreClassObject
    assertJSNotUndefined(accessor)
    assertEquals("function", js.typeOf(accessor))
    val obj = accessor()
    assertJSNotUndefined(obj)
    assertSame(SJSDefinedAutoExportedIgnoreClassObject, obj)
  }

  @Test def should_ignore_invalid_descendants(): Unit = {
    // This is just to check that everything here compiles
    object A extends AutoExportIgnoreTrait { var x = 1 }
    object B extends AutoExportIgnoreClass { var x = 2 }

    @ScalaJSDefined
    object C extends SJSDefinedAutoExportIgnoreClass { var x = 3 }

    // Check that the objects are usable
    assertEquals(1, A.x)
    assertEquals(2, B.x)
    assertEquals(3, C.x)

    A.x = 3
    B.x = 4
    C.x = 2

    assertEquals(3, A.x)
    assertEquals(4, B.x)
    assertEquals(2, C.x)
  }

  // @JSExportDescendentClasses

  @Test def auto_exports_for_classes_extending_a_trait(): Unit = {
    val ctor =
      exportsNamespace.org.scalajs.testsuite.jsinterop.AutoExportedTraitClass
    assertJSNotUndefined(ctor)
    assertEquals("function", js.typeOf(ctor))

    val obj1 = js.Dynamic.newInstance(ctor)()
    assertJSNotUndefined(obj1)
    assertEquals(obj1.x, 5)

    val obj2 = js.Dynamic.newInstance(ctor)(100)
    assertJSNotUndefined(obj2)
    assertEquals(obj2.x, 100)
  }

  @Test def auto_exports_for_classes_extending_a_class(): Unit = {
    val ctor =
      exportsNamespace.org.scalajs.testsuite.jsinterop.AutoExportedClassClass
    assertJSNotUndefined(ctor)
    assertEquals("function", js.typeOf(ctor))

    val obj1 = js.Dynamic.newInstance(ctor)()
    assertJSNotUndefined(obj1)
    assertEquals(obj1.x, 5)

    val obj2 = js.Dynamic.newInstance(ctor)(100)
    assertJSNotUndefined(obj2)
    assertEquals(obj2.x, 100)
  }

  @Test def auto_exports_for_Scala_js_defined_JS_classes_extending_a_trait(): Unit = {
    val ctor =
      exportsNamespace.org.scalajs.testsuite.jsinterop.SJSDefinedAutoExportedTraitClass
    assertJSNotUndefined(ctor)
    assertEquals("function", js.typeOf(ctor))

    val obj = js.Dynamic.newInstance(ctor)(100)
    assertTrue((obj: Any).isInstanceOf[SJSDefinedAutoExportedTraitClass])
    assertJSNotUndefined(obj)
    assertEquals(obj.x, 100)
  }

  @Test def auto_exports_for_Scala_js_defined_JS_classes_extending_a_class(): Unit = {
    val ctor =
      exportsNamespace.org.scalajs.testsuite.jsinterop.SJSDefinedAutoExportedClassClass
    assertJSNotUndefined(ctor)
    assertEquals("function", js.typeOf(ctor))

    val obj = js.Dynamic.newInstance(ctor)(100)
    assertTrue((obj: Any).isInstanceOf[SJSDefinedAutoExportedClassClass])
    assertJSNotUndefined(obj)
    assertEquals(obj.x, 100)
  }

  @Test def auto_exports_for_classes_extending_a_trait_with_ignoreInvalidDescendants(): Unit = {
    val ctor =
      exportsNamespace.org.scalajs.testsuite.jsinterop.AutoExportIgnoreTraitClass
    assertJSNotUndefined(ctor)
    assertEquals("function", js.typeOf(ctor))

    val obj1 = js.Dynamic.newInstance(ctor)()
    assertJSNotUndefined(obj1)
    assertEquals(obj1.x, 5)

    val obj2 = js.Dynamic.newInstance(ctor)(100)
    assertJSNotUndefined(obj2)
    assertEquals(obj2.x, 100)
  }

  @Test def auto_exports_for_classes_extending_a_class_with_ignoreInvalidDescendants(): Unit = {
    val ctor =
      exportsNamespace.org.scalajs.testsuite.jsinterop.AutoExportIgnoreClassClass
    assertJSNotUndefined(ctor)
    assertEquals("function", js.typeOf(ctor))

    val obj1 = js.Dynamic.newInstance(ctor)()
    assertJSNotUndefined(obj1)
    assertEquals(obj1.x, 5)

    val obj2 = js.Dynamic.newInstance(ctor)(100)
    assertJSNotUndefined(obj2)
    assertEquals(obj2.x, 100)
  }

  @Test def auto_exports_for_Scala_js_defined_JS_classes_extending_a_class_with_ignoreInvalidDescendants(): Unit = {
    val ctor =
      exportsNamespace.org.scalajs.testsuite.jsinterop.SJSDefinedAutoExportedIgnoreClassClass
    assertJSNotUndefined(ctor)
    assertEquals("function", js.typeOf(ctor))

    val obj = js.Dynamic.newInstance(ctor)(100)
    assertTrue((obj: Any).isInstanceOf[SJSDefinedAutoExportedIgnoreClassClass])
    assertJSNotUndefined(obj)
    assertEquals(obj.x, 100)
  }

  @Test def should_ignore_invalid_descendants2(): Unit = {
    trait HasBar { def bar: Int }

    @ScalaJSDefined
    trait SJSDefinedHasBar extends js.Any { def bar: Int }

    // This is just to check that everything here compiles
    class A extends AutoExportIgnoreTrait { def foo: Int = 1 }
    class B extends AutoExportIgnoreClass { def foo: Int = 2 }

    @ScalaJSDefined
    class C extends SJSDefinedAutoExportIgnoreClass { def foo: Int = 3 }

    val a = new A { override def foo: Int = 3 }
    val b = new B { override def foo: Int = 4 }
    val c = new C { override def foo: Int = 5 }
    val d = new AutoExportIgnoreClass with HasBar { def bar: Int = 1 }
    val e = new AutoExportIgnoreTrait with HasBar { def bar: Int = 1 }
    val f = new SJSDefinedAutoExportIgnoreClass with SJSDefinedHasBar { def bar: Int = 1 }

    // Check the classes are usable
    assertEquals(1, (new A).foo)
    assertEquals(2, (new B).foo)
    assertEquals(3, (new C).foo)
    assertEquals(3, a.foo)
    assertEquals(4, b.foo)
    assertEquals(5, c.foo)
    assertEquals(1, d.bar)
    assertEquals(1, e.bar)
    assertEquals(1, f.bar)
  }

}

object ExportNameHolder {
  final val className = "ConstantFoldedClassExport"
  final val objectName = "ConstantFoldedObjectExport"
  final val methodName = "myMethod"
}

@JSExport
@JSExport("TheExportedObject")
@JSExport("qualified.testobject.ExportedObject") // purposefully halfway the same as ExportedClass
@JSExport(ExportNameHolder.objectName)
object ExportedObject {
  @JSExport
  def witness: String = "witness"
}

@JSExportTopLevel("TopLevelExportedObject")
@JSExportTopLevel("qualified.testobject.TopLevelExportedObject")
object TopLevelExportedObject {
  @JSExport
  val witness: String = "witness"
}

@JSExport
@JSExport("TheSJSDefinedExportedObject")
@ScalaJSDefined
object SJSDefinedExportedObject extends js.Object {
  def witness: String = "witness"
}

@JSExportTopLevel("SJSDefinedTopLevelExportedObject")
@JSExportTopLevel("TheSJSDefinedTopLevelExportedObject")
@ScalaJSDefined
object SJSDefinedTopLevelExportedObject extends js.Object {
  val witness: String = "witness"
}

@JSExport
protected object ProtectedExportedObject {
  @JSExport
  def witness: String = "witness"
}

@JSExport
@JSExport("TheExportedClass")
@JSExport("qualified.testclass.ExportedClass") // purposefully halfway the same as ExportedObject
@JSExport(ExportNameHolder.className)
class ExportedClass(_x: Int) {
  @JSExport
  val x = _x
}

@JSExportTopLevel("TopLevelExportedClass")
@JSExportTopLevel("qualified.testclass.TopLevelExportedClass")
class TopLevelExportedClass(_x: Int) {
  @JSExport
  val x = _x
}

@JSExport
@JSExport("TheSJSDefinedExportedClass")
@JSExport("qualified.testclass.SJSDefinedExportedClass")
@ScalaJSDefined
class SJSDefinedExportedClass(val x: Int) extends js.Object

@JSExportTopLevel("SJSDefinedTopLevelExportedClass")
@JSExportTopLevel("TheSJSDefinedTopLevelExportedClass")
@ScalaJSDefined
class SJSDefinedTopLevelExportedClass(val x: Int) extends js.Object

@JSExport
protected class ProtectedExportedClass(_x: Int) {
  @JSExport
  val x = _x
}

@JSExport
class ExportedVarArgClass(x: String*) {

  @JSExport
  def this(x: Int, y: String) = this(s"Number: <$x>", y)

  @JSExport
  def result: String = x.mkString("|")
}

@JSExport
class ExportedDefaultArgClass(x: Int, y: Int, z: Int) {

  @JSExport
  def this(x: Int, y: Int = 5) = this(x, y, 100)

  @JSExport
  def result: Int = x + y + z
}

@JSExport("org.ExportedUnderOrgObject")
object ExportedUnderOrgObject

@JSExportDescendentClasses
@JSExportDescendentObjects
trait AutoExportTrait

object AutoExportedTraitObject extends AutoExportTrait
class AutoExportedTraitClass(_x: Int) extends AutoExportTrait {
  def this() = this(5)
  @JSExport
  def x: Int = _x
}

@JSExportDescendentClasses
@JSExportDescendentObjects
class AutoExportClass

object AutoExportedClassObject extends AutoExportClass
class AutoExportedClassClass(_x: Int) extends AutoExportTrait {
  def this() = this(5)
  @JSExport
  def x: Int = _x
}

@JSExportDescendentClasses
@JSExportDescendentObjects
@ScalaJSDefined
trait SJSDefinedAutoExportTrait extends js.Object

@ScalaJSDefined
object SJSDefinedAutoExportedTraitObject extends SJSDefinedAutoExportTrait
@ScalaJSDefined
class SJSDefinedAutoExportedTraitClass(val x: Int) extends SJSDefinedAutoExportTrait

@JSExportDescendentClasses
@JSExportDescendentObjects
@ScalaJSDefined
class SJSDefinedAutoExportClass extends js.Object

@ScalaJSDefined
object SJSDefinedAutoExportedClassObject extends SJSDefinedAutoExportClass
@ScalaJSDefined
class SJSDefinedAutoExportedClassClass(val x: Int) extends SJSDefinedAutoExportClass

@JSExportDescendentClasses(ignoreInvalidDescendants = true)
@JSExportDescendentObjects(ignoreInvalidDescendants = true)
trait AutoExportIgnoreTrait

object AutoExportIgnoreTraitObject extends AutoExportIgnoreTrait
class AutoExportIgnoreTraitClass(_x: Int) extends AutoExportIgnoreTrait {
  def this() = this(5)
  @JSExport
  def x: Int = _x
}

@JSExportDescendentClasses(ignoreInvalidDescendants = true)
@JSExportDescendentObjects(ignoreInvalidDescendants = true)
class AutoExportIgnoreClass

object AutoExportIgnoreClassObject extends AutoExportIgnoreClass
class AutoExportIgnoreClassClass(_x: Int) extends AutoExportIgnoreTrait {
  def this() = this(5)
  @JSExport
  def x: Int = _x
}

@JSExportDescendentClasses(ignoreInvalidDescendants = true)
@JSExportDescendentObjects(ignoreInvalidDescendants = true)
@ScalaJSDefined
class SJSDefinedAutoExportIgnoreClass extends js.Object

@ScalaJSDefined
object SJSDefinedAutoExportedIgnoreClassObject extends SJSDefinedAutoExportIgnoreClass
@ScalaJSDefined
class SJSDefinedAutoExportedIgnoreClassClass(val x: Int) extends SJSDefinedAutoExportIgnoreClass

@js.native
object NativeInvalidExportObject extends SJSDefinedAutoExportIgnoreClass
@js.native
class NativeInvalidExportClass extends SJSDefinedAutoExportIgnoreClass
@ScalaJSDefined
class SJSDefinedInvalidExportClass private () extends SJSDefinedAutoExportIgnoreClass

class SomeValueClass(val i: Int) extends AnyVal

@JSExportNamed
class ExportedNamedArgClass(x: Int = 1)(y: String = x.toString)(z: Boolean = y != "foo") {
  @JSExport
  val result = x + y + z
}

@JSExport
class ExportClassSetterNamed_= { // scalastyle:ignore
  @JSExport
  val x = 1
}

@JSExport
object ExportObjSetterNamed_= { // scalastyle:ignore
  @JSExport
  val x = 1
}

object ExportHolder {
  @JSExport("qualified.nested.ExportedClass")
  class ExportedClass

  @JSExportTopLevel("qualified.nested.TopLevelExportedClass")
  class TopLevelExportedClass

  @JSExport("qualified.nested.ExportedObject")
  object ExportedObject

  @JSExportTopLevel("qualified.nested.TopLevelExportedObject")
  object TopLevelExportedObject

  @JSExport("qualified.nested.SJSDefinedExportedClass")
  @ScalaJSDefined
  class SJSDefinedExportedClass extends js.Object
}

object TopLevelExports {
  @JSExportTopLevel("org.scalajs.testsuite.jsinterop.toplevel.basic")
  def basic(): Int = 1

  @JSExportTopLevel("org.scalajs.testsuite.jsinterop.toplevel.overload")
  def overload(x: String): String = "Hello " + x

  @JSExportTopLevel("org.scalajs.testsuite.jsinterop.toplevel.overload")
  def overload(x: Int, y: Int*): Int = x + y.sum

  var myVar: Int = _

  @JSExportTopLevel("org.scalajs.testsuite.jsinterop.toplevel.set")
  def setMyVar(x: Int): Unit = myVar = x

  object Nested {
    var myVar: Int = _

    @JSExportTopLevel("org.scalajs.testsuite.jsinterop.toplevel.setNested")
    def setMyVar(x: Int): Unit = myVar = x
  }
}

/* This object is only reachable via the top level export to make sure the
 * analyzer behaves correctly.
 */
object TopLevelExportsReachability {
  private val name = "World"

  @JSExportTopLevel("org.scalajs.testsuite.jsinterop.toplevel.reachability")
  def basic(): String = "Hello " + name
}

object TopLevelFieldExports {
  @JSExportTopLevel("org.scalajs.testsuite.jsinterop.toplevel.basicVal")
  val basicVal: Int = 5

  @JSExportTopLevel("org.scalajs.testsuite.jsinterop.toplevel.basicVar")
  var basicVar: String = "hello"

  @JSExportTopLevel("org.scalajs.testsuite.jsinterop.toplevel.valExportedTwice1")
  @JSExportTopLevel("org.scalajs.testsuite.jsinterop.toplevel.valExportedTwice2")
  val valExportedTwice: Int = 5

  @JSExportTopLevel("org.scalajs.testsuite.jsinterop.toplevel.varExportedTwice1")
  @JSExportTopLevel("org.scalajs.testsuite.jsinterop.toplevel.varExportedTwice2")
  var varExportedTwice: String = "hello"

  @JSExportTopLevel("org.scalajs.testsuite.jsinterop.toplevel.uninitializedVarInt")
  var uninitializedVarInt: Int = _

  @JSExportTopLevel("org.scalajs.testsuite.jsinterop.toplevel.uninitializedVarLong")
  var uninitializedVarLong: Long = _

  @JSExportTopLevel("org.scalajs.testsuite.jsinterop.toplevel.uninitializedVarString")
  var uninitializedVarString: String = _

  @JSExportTopLevel("org.scalajs.testsuite.jsinterop.toplevel.uninitializedVarChar")
  var uninitializedVarChar: Char = _
}

/* This object and its static initializer are only reachable via the top-level
 * export of its field, to make sure the analyzer and the static initiliazer
 * behave correctly.
 */
object TopLevelFieldExportsReachability {
  private val name = "World"

  @JSExportTopLevel("org.scalajs.testsuite.jsinterop.toplevel.fieldreachability")
  val greeting = "Hello " + name
}
