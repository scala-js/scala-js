/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.compiler

import scala.language.implicitConversions

import scala.scalajs.js
import scala.scalajs.js.annotation._

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._

import org.scalajs.testsuite.utils.AssertThrows._
import org.scalajs.testsuite.utils.JSUtils
import org.scalajs.testsuite.utils.Platform._

/*
 * Based on examples in:
 * http://lampwww.epfl.ch/~doeraene/scala-js/doc/js-interoperability.html
 */
class InteroperabilityTest {
  import InteroperabilityTest._

  implicit def jsArray2Array[T](a: js.Array[T]): Array[AnyRef] =
    a.map(_.asInstanceOf[AnyRef]).toArray

  implicit def array2Array[T](a: Array[T]): Array[AnyRef] =
    a.map(_.asInstanceOf[AnyRef])

  def assertArrayDynEquals[T](expected: Array[T], actual: js.Dynamic): Unit = {
    assertArrayEquals(expected, jsArray2Array(actual.asInstanceOf[js.Array[Any]]))
  }

  @Test def should_support_backquotes_to_escape_Scala_fields(): Unit = {
    val obj = js.eval("""
      var interoperabilityTestFieldEscape = {
        def: 0,
        val: function(x) { if (x) this.def = x; return this.def; }
      };
      interoperabilityTestFieldEscape;
    """).asInstanceOf[InteroperabilityTestFieldEscape]

    obj.`def` = 7357
    assertEquals(7357, obj.`def`)
    assertEquals(7357, obj.`val`())
    assertEquals(42, obj.`val`(42))
  }

  @Test def should_support_atJSName_to_specify_the_JavaScript_name_for_fields(): Unit = {
    val obj = js.eval("""
      var interoperabilityTestJSName = {
        def: 42,
        val: function(x) { if (x) this.def = x; return this.def; }
      };
      interoperabilityTestJSName;
    """).asInstanceOf[InteroperabilityTestJSName]

    assertEquals(42, obj.value())
    assertEquals(7357, obj.value(7357))
  }

  @Test def should_translate_explicit_getter_and_setter_names_to_field_access(): Unit = {
    val obj = js.eval("""
      var interoperabilityTestProperty = { a: 1 };
      interoperabilityTestProperty;
      """).asInstanceOf[InteroperabilityTestProperty]

    assertEquals(1, obj.a)
    obj.a = 100
    assertEquals(100, obj.a)
  }

  @Test def should_support_atJSName_together_with_field_access(): Unit = {
    val obj = js.eval("""
      var interoperabilityTestProperty = { b: 1 };
      interoperabilityTestProperty;
      """).asInstanceOf[InteroperabilityTestPropertyNamed]

    assertEquals(1, obj.a)
    obj.a = 100
    assertEquals(100, obj.a)
    assertEquals(100, obj.b)
  }

  @Test def should_support_atJSBracketAccess_to_specify_access_using_square_bracket_subscription(): Unit = {
    val obj = js.eval("""
      var interoperabilityTestJSBracketAccess = [ 0, 1, 7357 ];
      interoperabilityTestJSBracketAccess;
    """).asInstanceOf[InteroperabilityTestJSBracketAccess]

    assertEquals(7357, obj(2))
    obj(2) = 42
    assertEquals(42, obj(2))
  }

  @Test def should_allow_instanciation_of_JS_classes_inheriting_from_js_Object(): Unit = {
    js.eval("""
      var InteroperabilityTestInherit = {
        Pattern: function(x) {
          this.field = 42;
          this.method = function() {
            return '42';
          };
          this.getConstructorParam = function() {
            return x;
          };
        }
    }
    """)

    val obj = new InteroperabilityTestPattern("Scala.js")
    assertEquals(42, obj.field)
    assertEquals("42", obj.method)
    assertEquals("Scala.js", obj.getConstructorParam)
  }

  @Test def should_acces_top_level_JS_objects_via_Scala_objects_inheriting_from_js_Object(): Unit = {
    js.eval("""
      var InteroperabilityTestTopLevelObject = function(value) {
        return {
          value: value,
          valueAsInt: function() {
            return parseInt(value);
          }
        };
    }
    """)

    // Use alias for convenience: see end of file for definition
    val TopLevel = InteroperabilityTestTopLevel

    val obj = TopLevel("7357")
    assertEquals("7357", obj.value)
    assertEquals(7357, obj.valueAsInt)
  }

  @Test def should_access_native_JS_classes_and_objects_nested_in_JS_objects(): Unit = {
    js.eval("""
      var InteroperabilityTestContainerObject = {
        ContainedClass: function(x) {
          this.x = x;
        },
        ContainedObject: {
          x: 42
        },
        ContainedClassRenamed: function(x) {
          this.x = x * 2;
        },
        ContainedObjectRenamed: {
          x: 4242
        },
        ContainedClassWithDefaultParam: function(x) {
          this.x = x || 42;
        }
      };
    """)

    // Use alias for convenience: see end of file for definition
    val TopLevel = InteroperabilityTestContainerObject

    val obj1 = new TopLevel.ContainedClass(34)
    assertEquals(34, obj1.x)

    val obj2 = TopLevel.ContainedObject
    assertEquals(42, obj2.x)

    val obj3 = new TopLevel.ContainedClassWithJSName(65)
    assertEquals(130, obj3.x)

    val obj4 = TopLevel.ContainedObjectWithJSName
    assertEquals(4242, obj4.x)

    val obj5 = new TopLevel.ContainedClassWithDefaultParam()
    assertEquals(42, obj5.x)

    val obj6 = new TopLevel.ContainedClassWithDefaultParam(10)
    assertEquals(10, obj6.x)
  }

  @Test def should_access_native_JS_classes_and_objects_nested_in_atJSNamed_JS_objects(): Unit = {
    js.eval("""
      var InteroperabilityTestContainerObjectRenamed = {
        ContainedClass: function(x) {
          this.x = x;
        },
        ContainedObject: {
          x: 42
        },
        ContainedClassRenamed: function(x) {
          this.x = x * 2;
        },
        ContainedObjectRenamed: {
          x: 4242
        }
      };
    """)

    // Use alias for convenience: see end of file for definition
    val TopLevel = InteroperabilityTestContainerObjectWithJSName

    val obj1 = new TopLevel.ContainedClass(34)
    assertEquals(34, obj1.x)

    val obj2 = TopLevel.ContainedObject
    assertEquals(42, obj2.x)

    val obj3 = new TopLevel.ContainedClassWithJSName(65)
    assertEquals(130, obj3.x)

    val obj4 = TopLevel.ContainedObjectWithJSName
    assertEquals(4242, obj4.x)
  }

  @Test def should_allow_to_call_JS_methods_with_variadic_parameters(): Unit = {
    val obj = js.eval("""
      var obj = {
        foo: function() {
          var args = new Array(arguments.length);
          for (var i = 0; i < arguments.length; i++)
            args[i] = arguments[i];
          return args;
        }
      };
      obj;
    """)

    val elems = Seq[js.Any]("plop", 42, 51)

    val dyn = obj.asInstanceOf[js.Dynamic]
    assertArrayDynEquals(Array(), dyn.foo())
    assertArrayDynEquals(Array(3, 6), dyn.foo(3, 6))
    assertArrayDynEquals(Array("hello", false), dyn.foo("hello", false))
    assertArrayDynEquals(Array("plop", 42, 51), dyn.applyDynamic("foo")(elems: _*))

    val stat = obj.asInstanceOf[InteroperabilityTestVariadicMethod]
    assertArrayEquals(Array(), stat.foo())
    assertArrayEquals(Array(3, 6), stat.foo(3, 6))
    assertArrayEquals(Array("hello", false), stat.foo("hello", false))
    assertArrayEquals(Array("plop", 42, 51), stat.foo(elems: _*))
  }

  @Test def call_polytype_nullary_method_issue_2445(): Unit = {
    val obj = js.eval("""
      var obj = {
        emptyArray: []
      };
      obj;
    """)

    val objNative =
      obj.asInstanceOf[InteroperabilityTestPolyTypeNullaryMethodNative]
    val a = objNative.emptyArray[Int]
    assertTrue((a: Any).isInstanceOf[js.Array[_]])
    assertEquals(0, a.length)

    val objNonNative =
      obj.asInstanceOf[InteroperabilityTestPolyTypeNullaryMethodNonNative]
    val b = objNonNative.emptyArray[Int]
    assertTrue((b: Any).isInstanceOf[js.Array[_]])
    assertEquals(0, b.length)

    // From the bug report
    val objPoly =
      obj.asInstanceOf[InteroperabilityTestPolyClassPolyNullaryMethod[Int]]
    val c = objPoly.emptyArray[Any]
    assertTrue((c: Any).isInstanceOf[js.Array[_]])
    assertEquals(0, c.length)
  }

  @Test def should_allow_to_call_JS_constructors_with_variadic_parameters(): Unit = {
    import js.Dynamic.{newInstance => jsnew}

    js.eval("""
      var InteroperabilityTestVariadicCtor = function() {
        var args = new Array(arguments.length);
        for (var i = 0; i < arguments.length; i++)
          args[i] = arguments[i];
        this.args = args;
      };
    """)

    val elems = Seq[js.Any]("plop", 42, 51)

    val ctor = js.Dynamic.global.InteroperabilityTestVariadicCtor

    val args0 = jsnew(ctor)().args
    assertArrayDynEquals(Array(), args0)
    val args1 = jsnew(ctor)(3, 6).args
    assertArrayDynEquals(Array(3, 6), args1)
    val args2 = jsnew(ctor)("hello", false).args
    assertArrayDynEquals(Array("hello", false), args2)
    val args3 = jsnew(ctor)(elems: _*).args
    assertArrayDynEquals(Array("plop", 42, 51), args3)

    import org.scalajs.testsuite.compiler.{InteroperabilityTestVariadicCtor => C}
    assertArrayEquals(Array(), new C().args)
    assertArrayEquals(Array(3, 6), new C(3, 6).args)
    assertArrayEquals(Array("hello", false), new C("hello", false).args)
    assertArrayEquals(Array("plop", 42, 51), new C(elems: _*).args)
  }

  @Test def should_acces_top_level_JS_objects_via_Scala_object_with_annot_JSGlobalScope(): Unit = {
    js.eval("""
      var interoperabilityTestGlobalScopeValue = "7357";
      var interoperabilityTestGlobalScopeValueAsInt = function() {
        return parseInt(interoperabilityTestGlobalScopeValue);
      };
    """)

    // Use alias for convenience: see end of file for definition
    val Global = InteroperabilityTestGlobalScope

    assertEquals("7357", Global.interoperabilityTestGlobalScopeValue)
    assertEquals(7357, Global.interoperabilityTestGlobalScopeValueAsInt)

    Global.interoperabilityTestGlobalScopeValue = "42"
    assertEquals(42, Global.interoperabilityTestGlobalScopeValueAsInt)
  }

  @Test def should_acces_top_level_JS_objects_via_Scala_object_inheriting_from_js_GlobalScope(): Unit = {
    js.eval("""
      var interoperabilityTestGlobalScopeDeprecatedValue = "7357";
      var interoperabilityTestGlobalScopeDeprecatedValueAsInt = function() {
        return parseInt(interoperabilityTestGlobalScopeDeprecatedValue);
      };
    """)

    // Use alias for convenience: see end of file for definition
    val Global = InteroperabilityTestGlobalScopeDeprecated

    assertEquals("7357", Global.interoperabilityTestGlobalScopeDeprecatedValue)
    assertEquals(7357, Global.interoperabilityTestGlobalScopeDeprecatedValueAsInt)

    Global.interoperabilityTestGlobalScopeDeprecatedValue = "42"
    assertEquals(42, Global.interoperabilityTestGlobalScopeDeprecatedValueAsInt)
  }

  @Test def extends_js_GlobalScope_takes_precedence_over_JSName(): Unit = {
    assertSame(js.Dynamic.global,
        InteroperabilityTestGlobalScopeDeprecatedWithJSName)
  }

  @Test def should_protect_receiver_of_raw_JS_apply_if_its_a_select_issue_804(): Unit = {
    val rawReceiver = js.eval("""
      var interoperabilityTestRawReceiver = {
        member: 0xbad,
        check: function(raw) { return this.member ? this.member : raw; }
      };
      interoperabilityTestRawReceiver;
    """).asInstanceOf[InteroperabilityTestRawReceiver]

    assertEquals(7357, rawReceiver.check(7357))

    val check = rawReceiver.check
    assertEquals(0x600d, check(0x600d))

    class InScalaSelect(check: js.Function1[Int, Int]) {
      @JSExport
      val member: Int = 0xbad2
      def test(): Unit = assertEquals(5894, check(5894))
    }
    new InScalaSelect(check).test()
  }

  @Test def should_properly_handle_default_parameters(): Unit = {
    val obj = js.eval("""
      var interoperabilityTestDefaultParam = {
        fun: function() { return arguments; }
      };
      interoperabilityTestDefaultParam;
    """).asInstanceOf[InteroperabilityTestDefaultParam]

    // Helpers
    val keys = js.Dynamic.global.Object.keys
    val undef = js.undefined

    if (!executingInPhantomJS)
      assertEquals(1, keys(obj.simple(1)).length)
    assertEquals(1, obj.simple(1)("0"))

    if (!executingInPhantomJS)
      assertEquals(2, keys(obj.simple(1, 5)).length)
    assertEquals(1, obj.simple(1, 5)("0"))
    assertEquals(5, obj.simple(1, 5)("1"))

    if (!executingInPhantomJS)
      assertEquals(2, keys(obj.named(y = 5)).length)
    assertEquals(undef, obj.named(y = 5)("0"))
    assertEquals(5, obj.named(y = 5)("1"))

    if (!executingInPhantomJS)
      assertEquals(1, keys(obj.named(x = 5)).length)
    assertEquals(5, obj.named(x = 5)("0"))

    if (!executingInPhantomJS)
      assertEquals(5, keys(obj.multi()(1,2,3,4)()).length)
    assertEquals(undef, obj.multi()(1,2,3,4)()("0"))
    assertEquals(1, obj.multi()(1,2,3,4)()("1"))
    assertEquals(2, obj.multi()(1,2,3,4)()("2"))
    assertEquals(3, obj.multi()(1,2,3,4)()("3"))
    assertEquals(4, obj.multi()(1,2,3,4)()("4"))

    if (!executingInPhantomJS)
      assertEquals(2, keys(obj.multi(2)()(5)).length)
    assertEquals(2, obj.multi(2)()(5)("0"))
    assertEquals(5, obj.multi(2)()(5)("1"))
  }

  @Test def should_properly_handle_default_parameters_for_constructors_issue_791(): Unit = {
    js.eval("""
      var InteroperabilityTestCtor = function(x,y) {
        this.values = Array(x || 6, y || 8)
      }
    """);

    assertArrayEquals(Array(6, 8), new InteroperabilityTestCtor().values)
    assertArrayEquals(Array(6, 7), new InteroperabilityTestCtor(y = 7).values)
    assertArrayEquals(Array(3, 8), new InteroperabilityTestCtor(3).values)
    assertArrayEquals(Array(10, 2), new InteroperabilityTestCtor(10, 2).values)
  }

  @Test def should_generate_exports_for_methods_inherited_from_traits_issue_178(): Unit = {
    import js.annotation.JSExport

    trait Foo {
      @JSExport
      def theValue: Int = 1
    }
    class Bar extends Foo

    val x = (new Bar).asInstanceOf[js.Dynamic]

    // Call the export by using js.Dynamic
    val theValue = x.theValue
    assertEquals(1, theValue)
  }

  @Test def should_allow_constructor_params_that_are_vals_vars_in_facades_issue_1277(): Unit = {
    js.eval("""
        var InteroparabilityCtorInlineValue = function(x,y) {
          this.x = x;
          this.y = y;
        }
    """)

    val obj = new InteroparabilityCtorInlineValue(10, -1)

    assertEquals(10, obj.x)
    assertEquals(-1, obj.y)

    obj.y = 100

    assertEquals(10, obj.x)
    assertEquals(100, obj.y)
  }

  @Test def should_unbox_Chars_received_from_calling_a_JS_interop_method(): Unit = {
    val obj = js.eval("""
      var obj = {
        get: function(JSUtils) { return JSUtils.stringToChar('e'); }
      };
      obj;
    """).asInstanceOf[InteroperabilityTestCharResult]

    assertEquals('e'.toInt, obj.get(JSUtils).toInt)
  }

  @Test def should_box_Chars_given_to_a_JS_interop_method(): Unit = {
    val obj = js.eval("""
      var obj = {
        twice: function(JSUtils, c) { c = JSUtils.charToString(c); return c+c; }
      };
      obj;
    """).asInstanceOf[InteroperabilityTestCharParam]

    assertEquals("xx", obj.twice(JSUtils, 'x'))
  }

  @Test def should_unbox_value_classes_received_from_calling_a_JS_interop_method(): Unit = {
    val obj = js.eval("""
      var obj = {
        test: function(vc) { return vc; }
      };
      obj;
    """).asInstanceOf[InteroperabilityTestValueClassResult]

    val r = obj.test(new SomeValueClass(5))
    assertEquals(5, r.i)
  }

  @Test def should_box_value_classes_given_to_a_JS_interop_method(): Unit = {
    val obj = js.eval("""
      var obj = {
        stringOf: function(vc) { return vc.toString(); }
      };
      obj;
    """).asInstanceOf[InteroperabilityTestValueClassParam]

    val vc = new SomeValueClass(7)
    assertEquals("SomeValueClass(7)", obj.stringOf(vc))
  }

  @Test def should_not_unbox_values_received_from_JS_method_in_statement_position(): Unit = {
    /* To test this, we verify that a purposefully ill-typed facade does not
     * throw a ClassCastException when called in statement position.
     */
    val obj = js.eval("""
      var obj = {
        test: function() { return 4; } // typed as String in the trait
      };
      obj;
    """).asInstanceOf[InteroperabilityTestNoUnboxResultInStatement]
    obj.test() // in statement position, should not throw
    if (hasCompliantAsInstanceOfs)
      assertThrows(classOf[Exception], obj.test()) // in expression position, should throw
  }

  @Test def should_asInstanceOf_values_received_from_calling_a_JS_interop_method(): Unit = {
    assumeTrue("Assumed compliant asInstanceOf", hasCompliantAsInstanceOfs)
    val obj = js.eval("""
      var obj = {
        testChar: function() { return 5; },
        testInt: function() { return 6.4; },
        testShort: function() { return 60000; },
        testDouble: function() { return "hello"; },
        testString: function() { return {}; },
        testValueClass: function() { return "hello"; },
        testNormalClass: function() { return 45; },
        testAny: function() { return {}; }
      };
      obj;
    """).asInstanceOf[InteroperabilityTestAsInstanceOfResult]

    assertThrows(classOf[Exception], obj.testChar())
    assertThrows(classOf[Exception], obj.testInt())
    assertThrows(classOf[Exception], obj.testShort())
    assertThrows(classOf[Exception], obj.testDouble())
    assertThrows(classOf[Exception], obj.testString())
    assertThrows(classOf[Exception], obj.testValueClass())
    assertThrows(classOf[Exception], obj.testNormalClass())
    obj.testAny() // should not throw
  }

}

object InteroperabilityTest {

  @js.native
  trait InteroperabilityTestFieldEscape extends js.Object {
    var `def`: Int = js.native
    def `val`(): Int = js.native
    def `val`(n: Int): Int = js.native
  }

  @js.native
  trait InteroperabilityTestJSName extends js.Object {
    @JSName("val")
    def value(): Int = js.native
    @JSName("val")
    def value(n: Int): Int = js.native
  }

  @js.native
  trait InteroperabilityTestProperty extends js.Object {
    def a_=(x: Int): Unit = js.native
    def a: Int = js.native
  }

  @js.native
  trait InteroperabilityTestPropertyNamed extends js.Object {
    @JSName("b")
    def a_=(x: Int): Unit = js.native
    @JSName("b")
    def a: Int = js.native
    def b: Int = js.native
  }

  @js.native
  trait InteroperabilityTestJSBracketAccess extends js.Object {
    @JSBracketAccess
    def apply(index: Int): Int = js.native
    @JSBracketAccess
    def update(index: Int, v: Int): Unit = js.native
  }

  @js.native
  trait InteroperabilityTestRawReceiver extends js.Object {
    val check: js.Function1[Int, Int] = js.native
  }

  /** Trait with different method signatures, all forwarded to the same
   *  JS raw function that returns the argument list for inspection
   */
  @js.native
  trait InteroperabilityTestDefaultParam extends js.Object {
    @JSName("fun")
    def simple(x: Int, y: Int = 5): js.Dictionary[Any] = js.native
    @JSName("fun")
    def named(x: Int = 1, y: Int = 1, z: Int = 1): js.Dictionary[Any] = js.native
    @JSName("fun")
    def multi(x: Int = 1)(ys: Int*)(z: Int = 1): js.Dictionary[Any] = js.native
  }

  @js.native
  trait InteroperabilityTestCharResult extends js.Object {
    def get(jsUtils: JSUtils.type): Char = js.native
  }

  @js.native
  trait InteroperabilityTestCharParam extends js.Object {
    def twice(jsUtils: JSUtils.type, c: Char): String = js.native
  }

  @js.native
  trait InteroperabilityTestValueClassResult extends js.Object {
    def test(vc: Any): SomeValueClass = js.native
  }

  @js.native
  trait InteroperabilityTestValueClassParam extends js.Object {
    def stringOf(vc: SomeValueClass): String = js.native
  }

  @js.native
  trait InteroperabilityTestNoUnboxResultInStatement extends js.Object {
    def test(): String = js.native
  }

  @js.native
  trait InteroperabilityTestAsInstanceOfResult extends js.Object {
    def testChar(): Char = js.native
    def testInt(): Int = js.native
    def testShort(): Short = js.native
    def testDouble(): Double = js.native
    def testString(): String = js.native
    def testValueClass(): SomeValueClass = js.native
    def testNormalClass(): List[Int] = js.native
    def testAny(): Any = js.native
  }
}

/*
 * Helper classes, traits and objects defined here since they cannot be nested
 * without requiring explicit @JSName's, which would defeat the purpose of
 * their tests.
 */

@JSName("InteroperabilityTestInherit.Pattern")
@js.native
class InteroperabilityTestPattern protected () extends js.Object {
  def this(pattern: String) = this()
  val field: Int = js.native
  def method(): String = js.native
  def getConstructorParam(): String = js.native
}

@js.native
trait InteroperabilityTestTopLevel extends js.Object {
  val value: String = js.native
  def valueAsInt(): Int = js.native
}

@JSName("InteroperabilityTestTopLevelObject")
@js.native
object InteroperabilityTestTopLevel extends js.Object {
  def apply(value: String): InteroperabilityTestTopLevel = js.native
}

@js.native
object InteroperabilityTestContainerObject extends js.Object {
  @js.native
  class ContainedClass(_x: Int) extends js.Object {
    val x: Int = js.native
  }

  @js.native
  object ContainedObject extends js.Object {
    val x: Int = js.native
  }

  @JSName("ContainedClassRenamed")
  @js.native
  class ContainedClassWithJSName(_x: Int) extends js.Object {
    val x: Int = js.native
  }

  @JSName("ContainedObjectRenamed")
  @js.native
  object ContainedObjectWithJSName extends js.Object {
    val x: Int = js.native
  }

  @js.native
  class ContainedClassWithDefaultParam(_x: Int = ???) extends js.Object {
    val x: Int = js.native
  }
}

@JSName("InteroperabilityTestContainerObjectRenamed")
@js.native
object InteroperabilityTestContainerObjectWithJSName extends js.Object {
  @js.native
  class ContainedClass(_x: Int) extends js.Object {
    val x: Int = js.native
  }

  @js.native
  object ContainedObject extends js.Object {
    val x: Int = js.native
  }

  @JSName("ContainedClassRenamed")
  @js.native
  class ContainedClassWithJSName(_x: Int) extends js.Object {
    val x: Int = js.native
  }

  @JSName("ContainedObjectRenamed")
  @js.native
  object ContainedObjectWithJSName extends js.Object {
    val x: Int = js.native
  }
}

@js.native
trait InteroperabilityTestVariadicMethod extends js.Object {
  def foo(args: Any*): js.Array[Any] = js.native
}

@js.native
trait InteroperabilityTestPolyTypeNullaryMethodNative extends js.Object {
  def emptyArray[T]: js.Array[T] = js.native
}

@ScalaJSDefined
trait InteroperabilityTestPolyTypeNullaryMethodNonNative extends js.Object {
  def emptyArray[T]: js.Array[T]
}

@js.native
trait InteroperabilityTestPolyClassPolyNullaryMethod[+T] extends js.Object {
  def emptyArray[T2 >: T]: js.Array[T2] = js.native
}

@js.native
class InteroperabilityTestVariadicCtor(inargs: Any*) extends js.Object {
  val args: js.Array[Any] = js.native
}

@js.native
@JSGlobalScope
object InteroperabilityTestGlobalScope extends js.Object {
  var interoperabilityTestGlobalScopeValue: String = js.native
  def interoperabilityTestGlobalScopeValueAsInt(): Int = js.native
}

@js.native
object InteroperabilityTestGlobalScopeDeprecated extends js.GlobalScope {
  var interoperabilityTestGlobalScopeDeprecatedValue: String = js.native
  def interoperabilityTestGlobalScopeDeprecatedValueAsInt(): Int = js.native
}

@js.native
@JSName("ThisDoesNotExistButItIsIgnored")
object InteroperabilityTestGlobalScopeDeprecatedWithJSName extends js.GlobalScope

class SomeValueClass(val i: Int) extends AnyVal {
  override def toString(): String = s"SomeValueClass($i)"
}

@js.native
class InteroperabilityTestCtor(x: Int = 5, y: Int = ???) extends js.Object {
  def values: js.Array[Int] = js.native
}

@js.native
class InteroparabilityCtorInlineValue(val x: Int, var y: Int) extends js.Object
