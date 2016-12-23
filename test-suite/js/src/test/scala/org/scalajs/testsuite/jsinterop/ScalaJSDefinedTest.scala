/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
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

import org.scalajs.testsuite.utils.JSAssert._
import org.scalajs.testsuite.utils.Platform
import org.scalajs.testsuite.utils.AssertThrows.assertThrows

class ScalaJSDefinedTest {
  import org.scalajs.testsuite.jsinterop.{ScalaJSDefinedTestSeparateRun => SepRun}
  import ScalaJSDefinedTest._

  @Test def minimal_definition(): Unit = {
    val obj = new Minimal
    assertEquals("object", js.typeOf(obj))
    assertEquals(List[String](), js.Object.keys(obj).toList)
    assertEquals("[object Object]", obj.toString())
    assertNull(obj.getClass().asInstanceOf[js.Any])

    assertTrue((obj: Any).isInstanceOf[Minimal])
    assertTrue((obj: Any).isInstanceOf[js.Object])
    assertFalse((obj: Any).isInstanceOf[js.Error])
  }

  @Test def minimal_static_object_with_lazy_initialization(): Unit = {
    assertEquals(0, staticNonNativeObjectInitCount)
    val obj = StaticNonNativeObject
    assertEquals(1, staticNonNativeObjectInitCount)
    assertSame(obj, StaticNonNativeObject)
    assertEquals(1, staticNonNativeObjectInitCount)

    assertEquals("object", js.typeOf(obj))
    assertEquals(List[String](), js.Object.keys(obj).toList)
    assertEquals("[object Object]", obj.toString())
    assertNull(obj.getClass().asInstanceOf[js.Any])

    assertFalse((obj: Any).isInstanceOf[Minimal])
    assertTrue((obj: Any).isInstanceOf[js.Object])
    assertFalse((obj: Any).isInstanceOf[js.Error])
  }

  @Test def simple_method(): Unit = {
    val obj = new SimpleMethod
    assertEquals(8, obj.foo(5))
    assertEquals("hello42", obj.bar("hello", 42))

    val dyn = obj.asInstanceOf[js.Dynamic]
    assertEquals(8, dyn.foo(5))
    assertEquals("hello42", dyn.bar("hello", 42))
  }

  @Test def static_object_with_simple_method(): Unit = {
    val obj = StaticObjectSimpleMethod
    assertEquals(8, obj.foo(5))
    assertEquals("hello42", obj.bar("hello", 42))

    val dyn = obj.asInstanceOf[js.Dynamic]
    assertEquals(8, dyn.foo(5))
    assertEquals("hello42", dyn.bar("hello", 42))
  }

  @Test def simple_field(): Unit = {
    val obj = new SimpleField
    assertEquals(List("x", "y"), js.Object.keys(obj).toList)
    assertEquals(5, obj.x)
    assertEquals(10, obj.y)
    assertEquals(15, obj.sum())

    obj.y = 3
    assertEquals(3, obj.y)
    assertEquals(8, obj.sum())

    val dyn = obj.asInstanceOf[js.Dynamic]
    assertEquals(5, dyn.x)
    assertEquals(3, dyn.y)
    assertEquals(8, dyn.sum())

    dyn.y = 89
    assertEquals(89, dyn.y)
    assertEquals(89, obj.y)
    assertEquals(94, dyn.sum())
  }

  @Test def static_object_with_simple_field(): Unit = {
    val obj = StaticObjectSimpleField
    assertEquals(List("x", "y"), js.Object.keys(obj).toList)
    assertEquals(5, obj.x)
    assertEquals(10, obj.y)
    assertEquals(15, obj.sum())

    obj.y = 3
    assertEquals(3, obj.y)
    assertEquals(8, obj.sum())

    val dyn = obj.asInstanceOf[js.Dynamic]
    assertEquals(5, dyn.x)
    assertEquals(3, dyn.y)
    assertEquals(8, dyn.sum())

    dyn.y = 89
    assertEquals(89, dyn.y)
    assertEquals(89, obj.y)
    assertEquals(94, dyn.sum())
  }

  @Test def simple_accessors(): Unit = {
    val obj = new SimpleAccessors
    assertEquals(List("x"), js.Object.keys(obj).toList)
    assertEquals(1, obj.x)

    assertEquals(2, obj.readPlus1)
    assertEquals(-1, obj.neg)
    obj.neg = 4
    assertEquals(-4, obj.x)
    assertEquals(4, obj.neg)
    assertEquals(-3, obj.readPlus1)

    val dyn = obj.asInstanceOf[js.Dynamic]
    assertEquals(-4, dyn.x)

    assertEquals(-3, dyn.readPlus1)
    assertEquals(4, dyn.neg)
    dyn.neg = -9
    assertEquals(9, dyn.x)
    assertEquals(-9, dyn.neg)
    assertEquals(10, dyn.readPlus1)
  }

  @Test def simple_constructor(): Unit = {
    val obj = new SimpleConstructor(5, 10)
    assertEquals(List("x", "y"), js.Object.keys(obj).toList)
    assertEquals(5, obj.x)
    assertEquals(10, obj.y)
    assertEquals(15, obj.sum())

    obj.y = 3
    assertEquals(3, obj.y)
    assertEquals(8, obj.sum())

    val dyn = obj.asInstanceOf[js.Dynamic]
    assertEquals(5, dyn.x)
    assertEquals(3, dyn.y)
    assertEquals(8, dyn.sum())

    dyn.y = 89
    assertEquals(89, dyn.y)
    assertEquals(89, obj.y)
    assertEquals(94, dyn.sum())
  }

  @Test def simple_constructor_with_automatic_fields(): Unit = {
    val obj = new SimpleConstructorAutoFields(5, 10)
    assertEquals(List("x", "y"), js.Object.keys(obj).toList)
    assertEquals(5, obj.x)
    assertEquals(10, obj.y)
    assertEquals(15, obj.sum())

    obj.y = 3
    assertEquals(3, obj.y)
    assertEquals(8, obj.sum())

    val dyn = obj.asInstanceOf[js.Dynamic]
    assertEquals(5, dyn.x)
    assertEquals(3, dyn.y)
    assertEquals(8, dyn.sum())

    dyn.y = 89
    assertEquals(89, dyn.y)
    assertEquals(89, obj.y)
    assertEquals(94, dyn.sum())
  }

  @Test def simple_constructor_with_param_accessors(): Unit = {
    val obj = new SimpleConstructorParamAccessors(5, 10)
    assertNotEquals(Array("x", "y"), js.Object.keys(obj).toArray)
    assertEquals(15, obj.sum())

    val dyn = obj.asInstanceOf[js.Dynamic]
    assertEquals(15, dyn.sum())
  }

  @Test def default_values_for_fields(): Unit = {
    val obj = new DefaultFieldValues
    assertEquals(0, obj.int)
    assertEquals(false, obj.bool)
    assertEquals(0, obj.char.toInt)
    assertNull(obj.string)
    assertJSUndefined(obj.unit)

    /* Value class fields are initialized to null, instead of a boxed
     * representation of the zero of their underlying types, as for a
     * Scala class.
     */
    assertNull(obj.asInstanceOf[js.Dynamic].valueClass)
  }

  @Test def simple_inherited_from_a_native_class(): Unit = {
    val obj = new SimpleInheritedFromNative(3, 5)
    assertEquals(3, obj.x)
    assertEquals(5, obj.y)
    assertEquals(6, obj.bar)
    assertTrue(obj.isInstanceOf[SimpleInheritedFromNative])
    assertTrue(obj.isInstanceOf[NativeParentClass])
  }

  @Test def lambda_inside_a_method_issue_2220(): Unit = {
    @ScalaJSDefined
    class LambdaInsideMethod extends js.Object {
      def foo(): Int = {
        List(1, 2, 3).map(_ * 2).sum
      }
    }

    assertEquals(12, new LambdaInsideMethod().foo())
  }

  @Test def nested_inside_a_Scala_class(): Unit = {
    class OuterScalaClass(val x: Int) {
      @ScalaJSDefined
      class InnerJSClass(val y: Int) extends js.Object {
        def sum(z: Int): Int = x + y + z
      }
    }

    val outerObj = new OuterScalaClass(3)
    val obj = new outerObj.InnerJSClass(6)
    assertEquals(6, obj.y)
    assertEquals(20, obj.sum(11))
  }

  @Test def nested_inside_a_Scala_js_defined_JS_class(): Unit = {
    @ScalaJSDefined
    class OuterJSClass(val x: Int) extends js.Object {
      @ScalaJSDefined
      class InnerJSClass(val y: Int) extends js.Object {
        def sum(z: Int): Int = x + y + z
      }
    }

    val outerObj = new OuterJSClass(3)
    val obj = new outerObj.InnerJSClass(6)
    assertEquals(6, obj.y)
    assertEquals(20, obj.sum(11))
  }

  @Test def Scala_class_nested_inside_a_Scala_js_defined_JS_class(): Unit = {
    @ScalaJSDefined
    class OuterJSClass(val x: Int) extends js.Object {
      class InnerScalaClass(val y: Int) {
        def sum(z: Int): Int = x + y + z
      }
    }

    val outerObj = new OuterJSClass(3)
    val obj = new outerObj.InnerScalaClass(6)
    assertEquals(6, obj.y)
    assertEquals(20, obj.sum(11))
  }

  @Test def Scala_object_nested_inside_a_Scala_js_defined_JS_class(): Unit = {
    @ScalaJSDefined
    class Foo extends js.Object {
      var innerInitCount: Int = _

      object Inner {
        innerInitCount += 1
      }
    }

    val foo = new Foo
    assertEquals(0, foo.innerInitCount)
    val inner1 = foo.Inner
    assertEquals(1, foo.innerInitCount)
    assertTrue((foo.Inner: AnyRef) eq inner1)
    assertEquals(1, foo.innerInitCount)

    val dyn = (new Foo).asInstanceOf[js.Dynamic]
    assertEquals(0, dyn.innerInitCount)
    val inner2 = dyn.Inner
    assertEquals(1, dyn.innerInitCount)
    assertTrue((dyn.Inner: AnyRef) eq inner2)
    assertEquals(1, dyn.innerInitCount)

    assertFalse((inner2: AnyRef) eq inner1)
  }

  @Test def anonymous_class_with_captures(): Unit = {
    val x = (() => 5)()
    val obj = new js.Object {
      val y = 10
      def sum(z: Int): Int = x + y + z
    }

    val dyn = obj.asInstanceOf[js.Dynamic]
    assertEquals(10, dyn.y)
    assertEquals(26, dyn.sum(11))
  }

  @Test def anonymous_class_has_no_own_prototype(): Unit = {
    val obj = new js.Object {
      val x = 1
    }

    assertEquals(1, obj.asInstanceOf[js.Dynamic].x)
    assertSame(js.Object.getPrototypeOf(obj),
        js.constructorOf[js.Object].prototype)
  }

  @Test def local_class_has_own_prototype(): Unit = {
    @ScalaJSDefined
    class Local extends js.Object {
      val x = 1
    }

    val obj = new Local

    assertEquals(1, obj.asInstanceOf[js.Dynamic].x)

    val prototype = js.Object.getPrototypeOf(obj)

    assertNotSame(prototype, js.constructorOf[js.Object].prototype)
    assertSame(prototype, js.constructorOf[Local].prototype)
  }

  @Test def anonymous_class_non_trivial_supertype(): Unit = {
    val obj = new SimpleConstructor(1, 2) {
      val z = sum()
    }

    assertEquals(3, obj.asInstanceOf[js.Dynamic].z)
  }

  @Test def anonymous_class_using_own_method_in_ctor(): Unit = {
    val obj = new js.Object {
      val y = inc(0)
      def inc(x: Int) = x + 1
    }

    assertEquals(1, obj.asInstanceOf[js.Dynamic].y)
  }

  @Test def anonymous_class_uninitialized_fields(): Unit = {
    val obj = new js.Object {
      var x: String = _
    }

    assertNull(obj.asInstanceOf[js.Dynamic].x)
  }

  @Test def anonymous_class_field_init_order(): Unit = {
    val obj = new js.Object {
      val x = y
      val y = "Hello World"
    }.asInstanceOf[js.Dynamic]

    assertNull(obj.x)
    assertEquals("Hello World", obj.y)
  }

  @Test def anonymous_class_dependent_fields(): Unit = {
    val obj = new js.Object {
      val x = 1
      val y = x + 1
    }

    assertEquals(2, obj.asInstanceOf[js.Dynamic].y)
  }

  @Test def anonymous_class_use_this_in_ctor(): Unit = {
    var obj0: js.Object = null
    val obj1 = new js.Object {
      obj0 = this
    }

    assertSame(obj0, obj1)
  }

  @Test def nested_anonymous_classes(): Unit = {
    val outer = new js.Object {
      private var _x = 1
      def x = _x

      val inner = new js.Object {
        def inc() = _x += 1
      }
    }.asInstanceOf[js.Dynamic]

    val inner = outer.inner
    assertEquals(1, outer.x)
    inner.inc()
    assertEquals(2, outer.x)
  }

  @Test def nested_anonymous_classes_and_lambdas(): Unit = {
    def call(f: Int => js.Any) = f(1)

    // Also check that f's capture is properly transformed.
    val obj = call(x => new js.Object { val f: js.Any = (y: Int) => x + y })
    val res = obj.asInstanceOf[js.Dynamic].f(3)
    assertEquals(4, res)

    assertEquals(1, call(x => x))
  }

  @Test def local_object_is_lazy(): Unit = {
    var initCount: Int = 0

    @ScalaJSDefined
    object Obj extends js.Object {
      initCount += 1
    }

    assertEquals(0, initCount)
    val obj = Obj
    import js.DynamicImplicits.truthValue
    assertTrue(obj.asInstanceOf[js.Dynamic])
    assertEquals(1, initCount)
    assertSame(obj, Obj)
    assertEquals(1, initCount)
  }

  @Test def local_object_with_captures(): Unit = {
    val x = (() => 5)()

    @ScalaJSDefined
    object Obj extends js.Object {
      val y = 10
      def sum(z: Int): Int = x + y + z
    }

    assertEquals(10, Obj.y)
    assertEquals(26, Obj.sum(11))

    val dyn = Obj.asInstanceOf[js.Dynamic]
    assertEquals(10, dyn.y)
    assertEquals(26, dyn.sum(11))
  }

  @Test def object_in_Scala_js_defined_JS_class(): Unit = {
    @ScalaJSDefined
    class Foo extends js.Object {
      var innerInitCount: Int = _

      @ScalaJSDefined
      object Inner extends js.Object {
        innerInitCount += 1
      }
    }

    val foo = new Foo
    assertEquals(0, foo.innerInitCount)
    val inner1 = foo.Inner
    assertEquals(1, foo.innerInitCount)
    assertSame(inner1, foo.Inner)
    assertEquals(1, foo.innerInitCount)

    val dyn = (new Foo).asInstanceOf[js.Dynamic]
    assertEquals(0, dyn.innerInitCount)
    val inner2 = dyn.Inner
    assertEquals(1, dyn.innerInitCount)
    assertSame(inner2, dyn.Inner)
    assertEquals(1, dyn.innerInitCount)

    assertNotSame(inner1, inner2)
  }

  @Test def local_defs_must_not_be_exposed(): Unit = {
    @ScalaJSDefined
    class LocalDefsMustNotBeExposed extends js.Object {
      def foo(): String = {
        def bar(): String = "hello"
        bar()
      }
    }

    val obj = new LocalDefsMustNotBeExposed
    assertFalse(js.Object.properties(obj).exists(_.contains("bar")))
  }

  @Test def local_objects_must_not_be_exposed(): Unit = {
    @ScalaJSDefined
    class LocalObjectsMustNotBeExposed extends js.Object {
      def foo(): String = {
        object Bar
        Bar.toString()
      }
    }

    val obj = new LocalObjectsMustNotBeExposed
    assertFalse(js.Object.properties(obj).exists(_.contains("Bar")))
  }

  @Test def local_defs_with_captures_issue_1975(): Unit = {
    @ScalaJSDefined
    class LocalDefsWithCaptures extends js.Object {
      def foo(suffix: String): String = {
        def bar(): String = "hello " + suffix
        bar()
      }
    }

    val obj = new LocalDefsWithCaptures
    assertEquals("hello world", obj.foo("world"))
  }

  @Test def methods_with_explicit_name(): Unit = {
    @ScalaJSDefined
    class MethodsWithExplicitName extends js.Object {
      @JSName("theAnswer")
      def bar(): Int = 42
      @JSName("doubleTheParam")
      def double(x: Int): Int = x*2
    }

    val foo = new MethodsWithExplicitName
    assertEquals(42, foo.bar())
    assertEquals(6, foo.double(3))

    val dyn = foo.asInstanceOf[js.Dynamic]
    assertJSUndefined(dyn.bar)
    assertEquals(js.typeOf(dyn.theAnswer), "function")
    assertEquals(42, dyn.theAnswer())
    assertEquals(6, dyn.doubleTheParam(3))
  }

  @Test def methods_with_constant_folded_name(): Unit = {
    @ScalaJSDefined
    class MethodsWithConstantFoldedName extends js.Object {
      @JSName(JSNameHolder.MethodName)
      def bar(): Int = 42
    }

    val foo = new MethodsWithConstantFoldedName
    assertEquals(42, foo.bar())

    val dyn = foo.asInstanceOf[js.Dynamic]
    assertJSUndefined(dyn.bar)
    assertEquals(42, dyn.myMethod())
  }

  @Test def protected_methods(): Unit = {
    @ScalaJSDefined
    class ProtectedMethods extends js.Object {
      protected def bar(): Int = 42

      protected[testsuite] def foo(): Int = 100
    }

    val foo = new ProtectedMethods
    assertEquals(100, foo.foo())

    val dyn = foo.asInstanceOf[js.Dynamic]
    assertEquals(js.typeOf(dyn.bar), "function")
    assertEquals(42, dyn.bar())
    assertEquals(js.typeOf(dyn.foo), "function")
    assertEquals(100, dyn.foo())
  }

  @Test def readonly_properties(): Unit = {
    assumeFalse(
        "Assuming strict mode semantics, which are not honored by Rhino",
        Platform.executingInRhino)

    // Named classes
    @ScalaJSDefined
    class Foo extends js.Object {
      def bar: Int = 1
    }

    val x: js.Dynamic = (new Foo()).asInstanceOf[js.Dynamic]
    assertThrows(classOf[js.JavaScriptException], {
      x.bar = 2
    })

    // Anonymous classes
    val y = new js.Object {
      def bar: Int = 1
    }.asInstanceOf[js.Dynamic]

    assertThrows(classOf[js.JavaScriptException], {
      y.bar = 2
    })
  }

  @Test def properties_are_not_enumerable(): Unit = {
    // Named classes
    @ScalaJSDefined
    class Foo extends js.Object {
      def myProp: Int = 1
    }

    val x: js.Any = (new Foo()).asInstanceOf[js.Any]
    assertFalse(js.Object.properties(x).contains("myProp"))

    // Anonymous classes
    val y = new js.Object {
      def myProp: Int = 1
    }

    assertFalse(js.Object.properties(y).contains("myProp"))
  }

  @Test def properties_are_configurable(): Unit = {
    // Named classes
    @ScalaJSDefined
    class Foo extends js.Object {
      def myProp: Int = 1
    }

    // Delete property from prototype.
    val prototype = js.constructorOf[Foo].prototype
    prototype.asInstanceOf[js.Dictionary[js.Any]].delete("myProp")

    // Check it is actually gone.
    assertTrue(js.isUndefined((new Foo()).asInstanceOf[js.Dynamic].myProp))

    // Anonymous classes
    val y = new js.Object {
      def myProp: Int = 1
    }

    // The property should be on the instance itself.
    assertTrue(y.hasOwnProperty("myProp"))
    y.asInstanceOf[js.Dictionary[js.Any]].delete("myProp")
    assertTrue(js.isUndefined(y.asInstanceOf[js.Dynamic].myProp))
    assertFalse(y.hasOwnProperty("myProp"))
  }

  @Test def properties_with_explicit_name(): Unit = {
    @ScalaJSDefined
    class PropertiesWithExplicitName extends js.Object {
      private[this] var myY: String = "hello"
      @JSName("answer")
      val answerScala: Int = 42
      @JSName("x")
      var xScala: Int = 3
      @JSName("doubleX")
      def doubleXScala: Int = xScala*2
      @JSName("y")
      def yGetter: String = myY + " get"
      @JSName("y")
      def ySetter_=(v: String): Unit = myY = v + " set"
    }

    val foo = new PropertiesWithExplicitName
    assertEquals(42, foo.answerScala)
    assertEquals(3, foo.xScala)
    assertEquals(6, foo.doubleXScala)
    foo.xScala = 23
    assertEquals(23, foo.xScala)
    assertEquals(46, foo.doubleXScala)
    assertEquals("hello get", foo.yGetter)
    foo.ySetter_=("world")
    assertEquals("world set get", foo.yGetter)

    val dyn = (new PropertiesWithExplicitName).asInstanceOf[js.Dynamic]
    assertJSUndefined(dyn.answerScala)
    assertEquals(js.typeOf(dyn.answer), "number")
    assertEquals(42, dyn.answer)
    assertEquals(3, dyn.x)
    assertEquals(6, dyn.doubleX)
    dyn.x = 23
    assertEquals(23, dyn.x)
    assertEquals(46, dyn.doubleX)
    assertEquals("hello get", dyn.y)
    dyn.y = "world"
    assertEquals("world set get", dyn.y)
  }

  @Test def protected_properties(): Unit = {
    @ScalaJSDefined
    class ProtectedProperties extends js.Object {
      protected val x: Int = 42
      protected[testsuite] val y: Int = 43
    }

    val foo = new ProtectedProperties
    assertEquals(43, foo.y)

    val dyn = foo.asInstanceOf[js.Dynamic]
    assertEquals(42, dyn.x)
    assertEquals(43, dyn.y)
  }

  @Test def simple_overloaded_methods(): Unit = {
    @ScalaJSDefined
    class SimpleOverloadedMethods extends js.Object {
      def foo(): Int = 42
      def foo(x: Int): Int = x*2
    }

    val foo = new SimpleOverloadedMethods
    assertEquals(42, foo.foo())
    assertEquals(6, foo.foo(3))

    val dyn = foo.asInstanceOf[js.Dynamic]
    assertEquals(js.typeOf(dyn.foo), "function")
    assertEquals(42, dyn.foo())
    assertEquals(6, dyn.foo(3))
  }

  @Test def renamed_overloaded_methods(): Unit = {
    @ScalaJSDefined
    class RenamedOverloadedMethods extends js.Object {
      @JSName("foobar")
      def foo(): Int = 42
      @JSName("foobar")
      def bar(x: Int): Int = x*2
    }

    val foo = new RenamedOverloadedMethods
    assertEquals(42, foo.foo())
    assertEquals(6, foo.bar(3))

    val dyn = foo.asInstanceOf[js.Dynamic]
    assertEquals(js.typeOf(dyn.foobar), "function")
    assertEquals(42, dyn.foobar())
    assertEquals(6, dyn.foobar(3))
  }

  @Test def overloaded_constructors_num_parameters_resolution(): Unit = {
    assertEquals(1, new OverloadedConstructorParamNumber(1).foo)
    assertEquals(3, new OverloadedConstructorParamNumber(1, 2).foo)
  }

  @Test def overloaded_constructors_parameter_type_resolution(): Unit = {
    assertEquals(1, new OverloadedConstructorParamType(1).foo)
    assertEquals(3, new OverloadedConstructorParamType("abc").foo)
  }

  @Test def overloaded_constructors_with_captured_parameters(): Unit = {
    @ScalaJSDefined
    class OverloadedConstructorWithOuterContextOnly(val x: Int) extends js.Object {
      def this(y: String) = this(y.length)
    }

    val z = (() => 5)()
    @ScalaJSDefined
    class OverloadedConstructorWithValCapture(val x: Int) extends js.Object {
      def this(y: String) = this(z)
    }

    assertEquals(1, new OverloadedConstructorWithOuterContextOnly(1).x)
    assertEquals(3, new OverloadedConstructorWithOuterContextOnly("abc").x)

    assertEquals(1, new OverloadedConstructorWithValCapture(1).x)
    assertEquals(5, new OverloadedConstructorWithValCapture("abc").x)
  }

  @Test def overloaded_constructors_with_super_class(): Unit = {
    @ScalaJSDefined
    class OverloadedConstructorSup(val x: Int) extends js.Object {
      def this(y: String) = this(y.length)
    }
    @ScalaJSDefined
    class OverloadedConstructorSub(x: Int)
        extends OverloadedConstructorSup(3 * x) {
      def this(y: String) = this(2 * y.length)
    }
    assertEquals(1, new OverloadedConstructorSup(1).x)
    assertEquals(3, new OverloadedConstructorSup("abc").x)

    assertEquals(9, new OverloadedConstructorSub(3).x)
    assertEquals(12, new OverloadedConstructorSub("ab").x)
  }

  @Test def overloaded_constructors_with_repeated_parameters(): Unit = {
    @ScalaJSDefined
    class OverloadedConstructorWithRepeatedParameters(xs: Int*)
        extends js.Object {
      def this(y: String, ys: String*) = this(y.length +: ys.map(_.length): _*)
      def sum: Int = xs.sum
    }

    assertEquals(0, new OverloadedConstructorWithRepeatedParameters().sum)
    assertEquals(1, new OverloadedConstructorWithRepeatedParameters(1).sum)
    assertEquals(3, new OverloadedConstructorWithRepeatedParameters(1, 2).sum)
    assertEquals(7, new OverloadedConstructorWithRepeatedParameters(1, 2, 4).sum)

    assertEquals(3, new OverloadedConstructorWithRepeatedParameters("abc").sum)
    assertEquals(3, new OverloadedConstructorWithRepeatedParameters("ab", "c").sum)
    assertEquals(3, new OverloadedConstructorWithRepeatedParameters("a", "b", "c").sum)
  }

  @Test def overloaded_constructors_complex_resolution(): Unit = {
    val bazPrim = new OverloadedConstructorComplex(1, 2)
    assertEquals(1, bazPrim.foo)
    assertEquals(2, bazPrim.bar)

    val baz1 = new OverloadedConstructorComplex()
    assertEquals(5, baz1.foo)
    assertEquals(6, baz1.bar)

    val baz2 = new OverloadedConstructorComplex(3)
    assertEquals(3, baz2.foo)
    assertEquals(3, baz2.bar)

    val baz3 = new OverloadedConstructorComplex(7, 8, 9)
    assertEquals(7, baz3.foo)
    assertEquals(9, baz3.bar)

    val baz4 = new OverloadedConstructorComplex("abc")
    assertEquals(3, baz4.foo)
    assertEquals(3, baz4.bar)

    val baz5 = new OverloadedConstructorComplex("abc", 10)
    assertEquals(3, baz5.foo)
    assertEquals(10, baz5.bar)

    val baz6 = new OverloadedConstructorComplex(11, "abc")
    assertEquals(11, baz6.foo)
    assertEquals(3, baz6.bar)

    val baz7 = new OverloadedConstructorComplex(1, 2, 4, 8)
    assertEquals(3, baz7.foo)
    assertEquals(4, baz7.bar)

    val baz8 = new OverloadedConstructorComplex("abc", "abcd")
    assertEquals(3, baz8.foo)
    assertEquals(4, baz8.bar)

    val baz9 = new OverloadedConstructorComplex("abc", "abcd", "zx")
    assertEquals(5, baz9.foo)
    assertEquals(4, baz9.bar)

    val baz10 = new OverloadedConstructorComplex("abc", "abcd", "zx", "tfd")
    assertEquals(5, baz10.foo)
    assertEquals(7, baz10.bar)
  }

  @Test def polytype_nullary_method_issue_2445(): Unit = {
    @ScalaJSDefined
    class PolyTypeNullaryMethod extends js.Object {
      def emptyArray[T]: js.Array[T] = js.Array()
    }

    val obj = new PolyTypeNullaryMethod
    val a = obj.emptyArray[Int]
    assertTrue((a: Any).isInstanceOf[js.Array[_]])
    assertEquals(0, a.length)

    val dyn = obj.asInstanceOf[js.Dynamic]
    val b = dyn.emptyArray
    assertTrue((b: Any).isInstanceOf[js.Array[_]])
    assertEquals(0, b.length)
  }

  @Test def default_parameters(): Unit = {
    @ScalaJSDefined
    class DefaultParameters extends js.Object {
      def bar(x: Int, y: Int = 1): Int = x + y
      def dependent(x: Int)(y: Int = x + 1): Int = x + y

      def foobar(x: Int): Int = bar(x)
    }

    @ScalaJSDefined
    object DefaultParametersMod extends js.Object {
      def bar(x: Int, y: Int = 1): Int = x + y
      def dependent(x: Int)(y: Int = x + 1): Int = x + y

      def foobar(x: Int): Int = bar(x)
    }

    val foo = new DefaultParameters
    assertEquals(9, foo.bar(4, 5))
    assertEquals(5, foo.bar(4))
    assertEquals(4, foo.foobar(3))
    assertEquals(9, foo.dependent(4)(5))
    assertEquals(17, foo.dependent(8)())

    assertEquals(9, DefaultParametersMod.bar(4, 5))
    assertEquals(5, DefaultParametersMod.bar(4))
    assertEquals(4, DefaultParametersMod.foobar(3))
    assertEquals(9, DefaultParametersMod.dependent(4)(5))
    assertEquals(17, DefaultParametersMod.dependent(8)())

    def testDyn(dyn: js.Dynamic): Unit = {
      assertEquals(9, dyn.bar(4, 5))
      assertEquals(5, dyn.bar(4))
      assertEquals(4, dyn.foobar(3))
      assertEquals(9, dyn.dependent(4, 5))
      assertEquals(17, dyn.dependent(8))
    }
    testDyn(foo.asInstanceOf[js.Dynamic])
    testDyn(DefaultParametersMod.asInstanceOf[js.Dynamic])
  }

  @Test def override_default_parameters(): Unit = {
    @ScalaJSDefined
    class OverrideDefaultParametersParent extends js.Object {
      def bar(x: Int, y: Int = 1): Int = x + y
      def dependent(x: Int)(y: Int = x + 1): Int = x + y

      def foobar(x: Int): Int = bar(x)
    }

    @ScalaJSDefined
    class OverrideDefaultParametersChild
        extends OverrideDefaultParametersParent {
      override def bar(x: Int, y: Int = 10): Int = super.bar(x, y)
      override def dependent(x: Int)(y: Int = x * 2): Int = x + y
    }

    val foo = new OverrideDefaultParametersChild
    assertEquals(9, foo.bar(4, 5))
    assertEquals(14, foo.bar(4))
    assertEquals(13, foo.foobar(3))
    assertEquals(9, foo.dependent(4)(5))
    assertEquals(24, foo.dependent(8)())

    val parent: OverrideDefaultParametersParent = foo
    assertEquals(9, parent.bar(4, 5))
    assertEquals(14, parent.bar(4))
    assertEquals(13, parent.foobar(3))
    assertEquals(9, parent.dependent(4)(5))
    assertEquals(24, parent.dependent(8)())

    val dyn = foo.asInstanceOf[js.Dynamic]
    assertEquals(9, dyn.bar(4, 5))
    assertEquals(14, dyn.bar(4))
    assertEquals(13, dyn.foobar(3))
    assertEquals(9, dyn.dependent(4, 5))
    assertEquals(24, dyn.dependent(8))
  }

  @Test def override_method_with_default_parameters_without_new_default(): Unit = {
    @ScalaJSDefined
    class OverrideDefaultParametersWithoutDefaultParent extends js.Object {
      def bar(x: Int, y: Int = 1): Int = x + y
      def dependent(x: Int)(y: Int = x + 1): Int = x + y

      def foobar(x: Int): Int = bar(x)
    }

    @ScalaJSDefined
    class OverrideDefaultParametersWithoutDefaultChild
        extends OverrideDefaultParametersWithoutDefaultParent {
      override def bar(x: Int, y: Int): Int = x - y
      override def dependent(x: Int)(y: Int): Int = x - y
    }

    val foo = new OverrideDefaultParametersWithoutDefaultChild
    assertEquals(-1, foo.bar(4, 5))
    assertEquals(3, foo.bar(4))
    assertEquals(2, foo.foobar(3))
    assertEquals(-4, foo.dependent(4)(8))
    assertEquals(-1, foo.dependent(8)())

    val parent: OverrideDefaultParametersWithoutDefaultParent = foo
    assertEquals(-1, parent.bar(4, 5))
    assertEquals(3, parent.bar(4))
    assertEquals(2, parent.foobar(3))
    assertEquals(-4, parent.dependent(4)(8))
    assertEquals(-1, parent.dependent(8)())

    val dyn = foo.asInstanceOf[js.Dynamic]
    assertEquals(-1, dyn.bar(4, 5))
    assertEquals(3, dyn.bar(4))
    assertEquals(2, dyn.foobar(3))
    assertEquals(-4, dyn.dependent(4, 8))
    assertEquals(-1, dyn.dependent(8))
  }

  @Test def `constructors_with_default_parameters_(ScalaJSDefined/-)`(): Unit = {
    assertEquals(-1, new ConstructorDefaultParamJSNonNativeNone().foo)
    assertEquals(1, new ConstructorDefaultParamJSNonNativeNone(1).foo)
    assertEquals(5, new ConstructorDefaultParamJSNonNativeNone(5).foo)
  }

  @Test def `constructors_with_default_parameters_(ScalaJSDefined/ScalaJSDefined)`(): Unit = {
    assertEquals(-1, new ConstructorDefaultParamJSNonNativeJSNonNative().foo)
    assertEquals(1, new ConstructorDefaultParamJSNonNativeJSNonNative(1).foo)
    assertEquals(5, new ConstructorDefaultParamJSNonNativeJSNonNative(5).foo)
  }

  @Test def `constructors_with_default_parameters_(ScalaJSDefined/Scala)`(): Unit = {
    assertEquals(-1, new ConstructorDefaultParamJSNonNativeScala().foo)
    assertEquals(1, new ConstructorDefaultParamJSNonNativeScala(1).foo)
    assertEquals(5, new ConstructorDefaultParamJSNonNativeScala(5).foo)
  }

  @Test def `constructors_with_default_parameters_(Scala/ScalaJSDefined)`(): Unit = {
    assertEquals(-1, new ConstructorDefaultParamScalaJSNonNative().foo)
    assertEquals(1, new ConstructorDefaultParamScalaJSNonNative(1).foo)
    assertEquals(5, new ConstructorDefaultParamScalaJSNonNative(5).foo)
  }

  @Test def `constructors_with_default_parameters_(Native/-)`(): Unit = {
    assertEquals(-1, new ConstructorDefaultParamJSNativeNone().foo)
    assertEquals(1, new ConstructorDefaultParamJSNativeNone(1).foo)
    assertEquals(5, new ConstructorDefaultParamJSNativeNone(5).foo)
  }

  @Test def `constructors_with_default_parameters_(Native/Scala)`(): Unit = {
    assertEquals(-1, new ConstructorDefaultParamJSNativeScala().foo)
    assertEquals(1, new ConstructorDefaultParamJSNativeScala(1).foo)
    assertEquals(5, new ConstructorDefaultParamJSNativeScala(5).foo)
  }

  @Test def `constructors_with_default_parameters_(Native/ScalaJSDefined)`(): Unit = {
    assertEquals(-1, new ConstructorDefaultParamJSNativeJSNonNative().foo)
    assertEquals(1, new ConstructorDefaultParamJSNativeJSNonNative(1).foo)
    assertEquals(5, new ConstructorDefaultParamJSNativeJSNonNative(5).foo)
  }

  @Test def `constructors_with_default_parameters_(Native/Native)`(): Unit = {
    assertEquals(-1, new ConstructorDefaultParamJSNativeJSNative().foo)
    assertEquals(1, new ConstructorDefaultParamJSNativeJSNative(1).foo)
    assertEquals(5, new ConstructorDefaultParamJSNativeJSNative(5).foo)
  }

  @Test def `constructors_with_default_parameters_(Scala/Scala)`(): Unit = {
    assertEquals(-1, new ConstructorDefaultParamScalaScala().foo)
    assertEquals(1, new ConstructorDefaultParamScalaScala(1).foo)
    assertEquals(5, new ConstructorDefaultParamScalaScala(5).foo)
  }

  @Test def `constructors_with_default_parameters_(Scala/-)`(): Unit = {
    assertEquals(-1, new ConstructorDefaultParamScalaNone().foo)
    assertEquals(1, new ConstructorDefaultParamScalaNone(1).foo)
    assertEquals(5, new ConstructorDefaultParamScalaNone(5).foo)
  }

  @Test def `call_super_constructor_with_:__*`(): Unit = {
    @ScalaJSDefined
    class CallSuperCtorWithSpread(x: Int, y: Int, z: Int)
        extends NativeParentClassWithVarargs(x, Seq(y, z): _*)

    val foo = new CallSuperCtorWithSpread(4, 8, 23)
    assertEquals(4, foo.x)
    assertJSArrayEquals(js.Array(8, 23), foo.args)

    val dyn = foo.asInstanceOf[js.Dynamic]
    assertEquals(4, dyn.x)
    val args = dyn.args.asInstanceOf[js.Array[Int]]
    assertJSArrayEquals(js.Array(8, 23), args)
  }

  @Test def override_native_method(): Unit = {
    @ScalaJSDefined
    class OverrideNativeMethod extends NativeParentClass(3) {
      override def foo(s: String): String = s + s + x
    }

    val foo = new OverrideNativeMethod
    assertEquals(3, foo.x)
    assertEquals("hellohello3", foo.foo("hello"))

    val parent: NativeParentClass = foo
    assertEquals(3, parent.x)
    assertEquals("hellohello3", parent.foo("hello"))

    val dyn = foo.asInstanceOf[js.Dynamic]
    assertEquals(3, dyn.x)
    assertEquals("hellohello3", dyn.foo("hello"))
  }

  @Test def override_non_native_method(): Unit = {
    @ScalaJSDefined
    class OverrideNonNativeMethod extends NonNativeParentClass(3) {
      override def foo(s: String): String = s + s + x
    }

    val foo = new OverrideNonNativeMethod
    assertEquals(3, foo.x)
    assertEquals("hellohello3", foo.foo("hello"))

    val parent: NonNativeParentClass = foo
    assertEquals(3, parent.x)
    assertEquals("hellohello3", parent.foo("hello"))

    val dyn = foo.asInstanceOf[js.Dynamic]
    assertEquals(3, dyn.x)
    assertEquals("hellohello3", dyn.foo("hello"))
  }

  @Test def override_non_native_method_with_separate_compilation(): Unit = {
    val foo = new SepRun.SimpleChildClass
    assertEquals(6, foo.foo(3))

    val fooParent: SepRun.SimpleParentClass = foo
    assertEquals(6, fooParent.foo(3))

    val dyn = foo.asInstanceOf[js.Dynamic]
    assertEquals(6, foo.foo(3))
  }

  @Test def override_native_method_and_call_super(): Unit = {
    @ScalaJSDefined
    class OverrideNativeMethodSuperCall extends NativeParentClass(3) {
      override def foo(s: String): String = super.foo("bar") + s
    }

    val foo = new OverrideNativeMethodSuperCall
    assertEquals(3, foo.x)
    assertEquals("bar3hello", foo.foo("hello"))

    val parent: NativeParentClass = foo
    assertEquals(3, parent.x)
    assertEquals("bar3hello", parent.foo("hello"))

    val dyn = foo.asInstanceOf[js.Dynamic]
    assertEquals(3, dyn.x)
    assertEquals("bar3hello", dyn.foo("hello"))
  }

  @Test def override_non_native_method_and_call_super(): Unit = {
    @ScalaJSDefined
    class OverrideNonNativeMethodSuperCall extends NonNativeParentClass(3) {
      override def foo(s: String): String = super.foo("bar") + s
    }

    val foo = new OverrideNonNativeMethodSuperCall
    assertEquals(3, foo.x)
    assertEquals("bar3hello", foo.foo("hello"))

    val parent: NonNativeParentClass = foo
    assertEquals(3, parent.x)
    assertEquals("bar3hello", parent.foo("hello"))

    val dyn = foo.asInstanceOf[js.Dynamic]
    assertEquals(3, dyn.x)
    assertEquals("bar3hello", dyn.foo("hello"))
  }

  @Test def override_native_val(): Unit = {
    @ScalaJSDefined
    class OverrideNativeVal extends NativeParentClass(3) {
      override val x: Int = 42
    }

    val foo = new OverrideNativeVal
    assertEquals(42, foo.x)
    assertEquals(84, foo.bar)
    assertEquals("hello42", foo.foo("hello"))

    val parent: NativeParentClass = foo
    assertEquals(42, parent.x)
    assertEquals(84, parent.bar)
    assertEquals("hello42", parent.foo("hello"))

    val dyn = foo.asInstanceOf[js.Dynamic]
    assertEquals(42, dyn.x)
    assertEquals(84, dyn.bar)
    assertEquals("hello42", dyn.foo("hello"))
  }

  @Test def override_non_native_val(): Unit = {
    @ScalaJSDefined
    class OverrideNonNativeVal extends NonNativeParentClass(3) {
      override val x: Int = 42
    }

    val foo = new OverrideNonNativeVal
    assertEquals(42, foo.x)
    assertEquals(84, foo.bar)
    assertEquals("hello42", foo.foo("hello"))

    val parent: NonNativeParentClass = foo
    assertEquals(42, parent.x)
    assertEquals(84, parent.bar)
    assertEquals("hello42", parent.foo("hello"))

    val dyn = foo.asInstanceOf[js.Dynamic]
    assertEquals(42, dyn.x)
    assertEquals(84, dyn.bar)
    assertEquals("hello42", dyn.foo("hello"))
  }

  @Test def override_native_getter(): Unit = {
    @ScalaJSDefined
    class OverrideNativeGetter extends NativeParentClass(3) {
      override def bar: Int = x * 3
    }

    val foo = new OverrideNativeGetter
    assertEquals(3, foo.x)
    assertEquals(9, foo.bar)

    val parent: NativeParentClass = foo
    assertEquals(3, parent.x)
    assertEquals(9, parent.bar)

    val dyn = foo.asInstanceOf[js.Dynamic]
    assertEquals(3, dyn.x)
    assertEquals(9, dyn.bar)
  }

  @Test def override_non_native_getter(): Unit = {
    @ScalaJSDefined
    class OverrideNonNativeGetter extends NonNativeParentClass(3) {
      override def bar: Int = x * 3
    }

    val foo = new OverrideNonNativeGetter
    assertEquals(3, foo.x)
    assertEquals(9, foo.bar)

    val parent: NonNativeParentClass = foo
    assertEquals(3, parent.x)
    assertEquals(9, parent.bar)

    val dyn = foo.asInstanceOf[js.Dynamic]
    assertEquals(3, dyn.x)
    assertEquals(9, dyn.bar)
  }

  @Test def override_native_getter_with_val(): Unit = {
    @ScalaJSDefined
    class OverrideNativeGetterWithVal extends NativeParentClass(3) {
      override val bar: Int = 1
    }

    val foo = new OverrideNativeGetterWithVal
    assertEquals(3, foo.x)
    assertEquals(1, foo.bar)

    val parent: NativeParentClass = foo
    assertEquals(3, parent.x)
    assertEquals(1, parent.bar)

    val dyn = foo.asInstanceOf[js.Dynamic]
    assertEquals(3, dyn.x)
    assertEquals(1, dyn.bar)
  }

  @Test def override_non_native_getter_with_val(): Unit = {
    @ScalaJSDefined
    class OverrideNonNativeGetterWithVal extends NonNativeParentClass(3) {
      override val bar: Int = 1
    }

    val foo = new OverrideNonNativeGetterWithVal
    assertEquals(3, foo.x)
    assertEquals(1, foo.bar)

    val parent: NonNativeParentClass = foo
    assertEquals(3, parent.x)
    assertEquals(1, parent.bar)

    val dyn = foo.asInstanceOf[js.Dynamic]
    assertEquals(3, dyn.x)
    assertEquals(1, dyn.bar)
  }

  @Test def override_getter_with_super(): Unit = {
    @ScalaJSDefined
    class OverrideGetterSuperParent extends js.Object {
      def bar: Int = 43
    }
    @ScalaJSDefined
    class OverrideGetterSuperChild extends OverrideGetterSuperParent {
      override def bar: Int = super.bar * 3
    }

    val foo = new OverrideGetterSuperChild
    assertEquals(129, foo.bar)

    val parent: OverrideGetterSuperParent = foo
    assertEquals(129, parent.bar)

    val dyn = foo.asInstanceOf[js.Dynamic]
    assertEquals(129, dyn.bar)
  }

  @Test def override_setter_with_super(): Unit = {
    @ScalaJSDefined
    class OverrideSetterSuperParent extends js.Object {
      var x: Int = 43
      def bar_=(v: Int): Unit = x = v
    }
    @ScalaJSDefined
    class OverrideSetterSuperChild extends OverrideSetterSuperParent {
      override def bar_=(v: Int): Unit = super.bar_=(v * 3)
    }

    val foo = new OverrideSetterSuperChild
    foo.bar_=(4)
    assertEquals(12, foo.x)

    val parent: OverrideSetterSuperParent = foo
    parent.bar_=(5)
    assertEquals(15, parent.x)

    val dyn = foo.asInstanceOf[js.Dynamic]
    dyn.bar = 6
    assertEquals(18, dyn.x)
  }

  @Test def add_setter_in_subclass(): Unit = {
    @ScalaJSDefined
    class AddSetterInSubclassParent extends js.Object {
      var x: Int = 43
      def bar: Int = x
    }
    @ScalaJSDefined
    class AddSetterInSubclassChild extends AddSetterInSubclassParent {
      def bar_=(v: Int): Unit = x = v
    }

    val foo = new AddSetterInSubclassChild
    foo.bar = 4
    assertEquals(4, foo.x)
    assertEquals(4, foo.bar)

    val dyn = foo.asInstanceOf[js.Dynamic]
    dyn.bar = 6
    assertEquals(6, dyn.x)
    assertEquals(6, dyn.bar)
  }

  @Test def add_getter_in_subclass(): Unit = {
    @ScalaJSDefined
    class AddGetterInSubclassParent extends js.Object {
      var x: Int = 43
      def bar_=(v: Int): Unit = x = v
    }
    @ScalaJSDefined
    class AddGetterInSubclassChild extends AddGetterInSubclassParent {
      def bar: Int = x
    }

    val foo = new AddGetterInSubclassChild
    foo.bar = 4
    assertEquals(4, foo.x)
    assertEquals(4, foo.bar)

    val dyn = foo.asInstanceOf[js.Dynamic]
    dyn.bar = 6
    assertEquals(6, dyn.x)
    assertEquals(6, dyn.bar)
  }

  @Test def overload_native_method(): Unit = {
    @ScalaJSDefined
    class OverloadNativeMethod extends NativeParentClass(3) {
      def foo(s: String, y: Int): String = foo(s) + " " + y
    }

    val foo = new OverloadNativeMethod
    assertEquals("hello3", foo.foo("hello"))
    assertEquals("hello3 4", foo.foo("hello", 4))

    val parent: NativeParentClass = foo
    assertEquals("hello3", parent.foo("hello"))

    val dyn = foo.asInstanceOf[js.Dynamic]
    assertEquals("hello3", dyn.foo("hello"))
    assertEquals("hello3 4", dyn.foo("hello", 4))
  }

  @Test def overload_non_native_method(): Unit = {
    @ScalaJSDefined
    class OverloadNonNativeMethod extends NonNativeParentClass(3) {
      def foo(s: String, y: Int): String = foo(s) + " " + y
    }

    val foo = new OverloadNonNativeMethod
    assertEquals("hello3", foo.foo("hello"))
    assertEquals("hello3 4", foo.foo("hello", 4))

    val parent: NonNativeParentClass = foo
    assertEquals("hello3", parent.foo("hello"))

    val dyn = foo.asInstanceOf[js.Dynamic]
    assertEquals("hello3", dyn.foo("hello"))
    assertEquals("hello3 4", dyn.foo("hello", 4))
  }

  @Test def overload_with_default_parameter(): Unit = {
    @ScalaJSDefined
    class OverloadDefaultParameter extends js.Object {
      def foo(x: Int): Int = x
      def foo(x: String = ""): String = x
    }

    val foo = new OverloadDefaultParameter
    assertEquals(5, foo.foo(5))
    assertEquals("", foo.foo())
    assertEquals("hello", foo.foo("hello"))
  }

  @Test def implement_a_simple_trait(): Unit = {
    @ScalaJSDefined
    class ImplementSimpleTrait extends js.Object with SimpleTrait {
      def foo(x: Int): Int = x + 1
    }

    val foo = new ImplementSimpleTrait
    assertEquals(4, foo.foo(3))

    val fooTrait: SimpleTrait = foo
    assertEquals(6, fooTrait.foo(5))
  }

  @Test def implement_a_simple_trait_under_separate_compilation(): Unit = {
    @ScalaJSDefined
    class ImplementSimpleTraitSepRun extends js.Object with SepRun.SimpleTrait {
      def foo(x: Int): Int = x + 1
    }

    val foo = new ImplementSimpleTraitSepRun
    assertEquals(4, foo.foo(3))

    val fooTrait: SepRun.SimpleTrait = foo
    assertEquals(6, fooTrait.foo(5))
  }

  @Test def implement_a_trait_with_a_val(): Unit = {
    @ScalaJSDefined
    trait TraitWithVal extends js.Object {
      val x: Int
    }

    @ScalaJSDefined
    class ImplWithVal extends TraitWithVal {
      val x: Int = 3
    }

    val foo = new ImplWithVal
    assertEquals(3, foo.x)

    val fooTrait: TraitWithVal = foo
    assertEquals(3, fooTrait.x)
  }

  @Test def implement_a_trait_with_a_var(): Unit = {
    @ScalaJSDefined
    trait TraitWithVar extends js.Object {
      var x: Int
    }

    @ScalaJSDefined
    class ImplWithVar extends TraitWithVar {
      var x: Int = 3
    }

    val foo = new ImplWithVar
    assertEquals(3, foo.x)

    val fooTrait: TraitWithVar = foo
    assertEquals(3, fooTrait.x)

    foo.x = 5
    assertEquals(5, fooTrait.x)
    fooTrait.x = 19
    assertEquals(19, foo.x)
  }

  @Test def implement_a_trait_extending_a_native_JS_class(): Unit = {
    @ScalaJSDefined
    trait TraitExtendsJSClass extends NativeParentClass {
      def foobar(x: Int): Int
    }

    @ScalaJSDefined
    class ImplExtendsJSClassAndTrait
        extends NativeParentClass(5) with TraitExtendsJSClass {
      def foobar(x: Int): Int = x * 3
    }

    val foo = new ImplExtendsJSClassAndTrait
    assertEquals(18, foo.foobar(6))
  }

  @Test def implement_abstract_members_coming_from_a_native_JS_class(): Unit = {
    @ScalaJSDefined
    class ImplDeferredMembersFromJSParent
        extends NativeParentClassWithDeferred {
      val x: Int = 43

      def bar(y: Int): Int = y * 2
    }

    val FooResult = (12 + 4) * 2 + 43

    val foo = new ImplDeferredMembersFromJSParent
    assertEquals(43, foo.x)
    assertEquals(64, foo.bar(32))
    assertEquals(FooResult, foo.foo(12))

    val fooParent: NativeParentClassWithDeferred = foo
    assertEquals(43, fooParent.x)
    assertEquals(64, fooParent.bar(32))
    assertEquals(FooResult, fooParent.foo(12))

    val dyn = foo.asInstanceOf[js.Dynamic]
    assertEquals(43, dyn.x)
    assertEquals(64, dyn.bar(32))
    assertEquals(FooResult, dyn.foo(12))
  }

  @Test def override_a_method_with_default_values_from_a_native_JS_class(): Unit = {
    @ScalaJSDefined
    class OverrideDefault extends NativeParentClass(7) {
      override def methodWithDefault(x: Int = 9): Int = x * 2
    }

    val child = new OverrideDefault
    assertEquals(18, child.methodWithDefault())
    assertEquals(14, child.methodWithDefault(7))

    val parent: NativeParentClass = child
    assertEquals(18, parent.methodWithDefault())
    assertEquals(14, parent.methodWithDefault(7))
  }

  // #2603
  @Test def default_values_in_non_exposed_methods(): Unit = {
    @ScalaJSDefined
    class DefaultParameterss(val default: Int) extends js.Object {
      /* We don't use a constant default value to make sure it actually comes
       * from the default parameter accessors.
       */
      private def privateWithDefault(x: Int = default) = x

      def callPrivate(): Int = privateWithDefault()
      def callNested(): Int = {
        def nested(x: Int = default) = x
        nested()
      }
    }

    val x = new DefaultParameterss(5)
    assertEquals(5, x.callPrivate())
    assertEquals(5, x.callNested())
  }
}

object ScalaJSDefinedTest {

  // Defined in test-suite/src/test/resources/ScalaJSDefinedTestNatives.js
  @JSName("ScalaJSDefinedTestNativeParentClass")
  @js.native
  class NativeParentClass(val x: Int) extends js.Object {
    def foo(s: String): String = js.native

    def bar: Int = js.native

    def methodWithDefault(x: Int = 5): Int = js.native
  }

  @ScalaJSDefined
  class NonNativeParentClass(val x: Int) extends js.Object {
    def foo(s: String): String = s + x

    def bar: Int = x * 2
  }

  @js.native
  trait NativeTraitWithDeferred extends js.Object {
    val x: Int
  }

  // Defined in test-suite/src/test/resources/ScalaJSDefinedTestNatives.js
  @JSName("ScalaJSDefinedTestNativeParentClassWithDeferred")
  @js.native
  abstract class NativeParentClassWithDeferred extends NativeTraitWithDeferred {
    def foo(y: Int): Int = js.native // = bar(y + 4) + x

    def bar(y: Int): Int
  }

  // Defined in test-suite/src/test/resources/ScalaJSDefinedTestNatives.js
  @JSName("ScalaJSDefinedTestNativeParentClassWithVarargs")
  @js.native
  class NativeParentClassWithVarargs(
      _x: Int, _args: Int*) extends js.Object {
    val x: Int = js.native
    val args: js.Array[Int] = js.native
  }

  @ScalaJSDefined
  trait SimpleTrait extends js.Any {
    def foo(x: Int): Int
  }

  @ScalaJSDefined
  class Minimal extends js.Object

  private var staticNonNativeObjectInitCount: Int = _

  @ScalaJSDefined
  object StaticNonNativeObject extends js.Object {
    staticNonNativeObjectInitCount += 1
  }

  @ScalaJSDefined
  class SimpleMethod extends js.Object {
    def foo(x: Int): Int = x + 3
    def bar(s: String, i: Int): String = s + i
  }

  @ScalaJSDefined
  object StaticObjectSimpleMethod extends js.Object {
    def foo(x: Int): Int = x + 3
    def bar(s: String, i: Int): String = s + i
  }

  @ScalaJSDefined
  class SimpleField extends js.Object {
    val x = 5
    var y = 10

    def sum(): Int = x + y
  }

  @ScalaJSDefined
  object StaticObjectSimpleField extends js.Object {
    val x = 5
    var y = 10

    def sum(): Int = x + y
  }

  @ScalaJSDefined
  class SimpleAccessors extends js.Object {
    var x = 1
    def readPlus1: Int = x + 1

    def neg: Int = -x
    def neg_=(v: Int): Unit = x = -v
  }

  @ScalaJSDefined
  class SimpleConstructor(_x: Int, _y: Int) extends js.Object {
    val x = _x
    var y = _y

    def sum(): Int = x + y
  }

  @ScalaJSDefined
  class ConstructorDefaultParamJSNonNativeNone(val foo: Int = -1) extends js.Object

  @ScalaJSDefined
  class ConstructorDefaultParamJSNonNativeJSNonNative(val foo: Int = -1) extends js.Object
  @ScalaJSDefined
  object ConstructorDefaultParamJSNonNativeJSNonNative extends js.Object

  @ScalaJSDefined
  class ConstructorDefaultParamJSNonNativeScala(val foo: Int = -1) extends js.Object
  object ConstructorDefaultParamJSNonNativeScala

  class ConstructorDefaultParamScalaJSNonNative(val foo: Int = -1)
  @ScalaJSDefined
  object ConstructorDefaultParamScalaJSNonNative extends js.Object

  @js.native
  @JSName("ConstructorDefaultParam")
  class ConstructorDefaultParamJSNativeNone(val foo: Int = -1) extends js.Object

  @js.native
  @JSName("ConstructorDefaultParam")
  class ConstructorDefaultParamJSNativeScala(val foo: Int = -1) extends js.Object
  object ConstructorDefaultParamJSNativeScala

  @js.native
  @JSName("ConstructorDefaultParam")
  class ConstructorDefaultParamJSNativeJSNonNative(val foo: Int = -1) extends js.Object
  @ScalaJSDefined
  object ConstructorDefaultParamJSNativeJSNonNative extends js.Object

  @js.native
  @JSName("ConstructorDefaultParam")
  class ConstructorDefaultParamJSNativeJSNative(val foo: Int = -1) extends js.Object
  @js.native
  @JSName("ConstructorDefaultParam")
  object ConstructorDefaultParamJSNativeJSNative extends js.Object

  // sanity check
  object ConstructorDefaultParamScalaScala
  class ConstructorDefaultParamScalaScala(val foo: Int = -1)

  // sanity check
  class ConstructorDefaultParamScalaNone(val foo: Int = -1)

  @ScalaJSDefined
  class OverloadedConstructorParamNumber(val foo: Int) extends js.Object {
    def this(x: Int, y: Int) = this(x + y)
    def this(x: Int, y: Int, z: Int) = this(x + y, z)
  }

  @ScalaJSDefined
  class OverloadedConstructorParamType(val foo: Int) extends js.Object {
    def this(x: String) = this(x.length)
    def this(x: Option[String]) = this(x.get)
  }

  @ScalaJSDefined
  class OverloadedConstructorComplex(val foo: Int, var bar: Int) extends js.Object {
    def this() = this(5, 6)
    def this(x: Int) = this(x, x)
    def this(x: Int, y: Int, z: Int) = {
      this(x, y)
      bar = z
    }
    def this(x: String) = this(x.length)
    def this(x: String, y: Int) = this(x.length, y)
    def this(x: Int, y: String) = this(x, y.length)
    def this(w: Int, x: Int, y: Int, z: Int) = {
      this(w + x, y, z)
      bar = y
    }
    def this(a: String, x: String, b: String = "", y: String = "") =
      this((a + b).length, (x + y).length)
  }

  @ScalaJSDefined
  class SimpleConstructorAutoFields(val x: Int, var y: Int) extends js.Object {
    def sum(): Int = x + y
  }

  @ScalaJSDefined
  class SimpleConstructorParamAccessors(x: Int, y: Int) extends js.Object {
    def sum(): Int = x + y
  }

  @ScalaJSDefined
  class DefaultFieldValues extends js.Object {
    var int: Int = _
    var bool: Boolean = _
    var char: Char = _
    var string: String = _
    var unit: Unit = _
    var valueClass: SomeValueClass = _
  }

  @ScalaJSDefined
  class SimpleInheritedFromNative(
      x: Int, val y: Int) extends NativeParentClass(x)

  class SomeValueClass(val i: Int) extends AnyVal

  object JSNameHolder {
    final val MethodName = "myMethod"
  }

}
