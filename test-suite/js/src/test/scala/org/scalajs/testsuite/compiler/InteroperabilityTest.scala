/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.compiler

import scala.scalajs.js
import scala.scalajs.js.annotation._

import org.scalajs.jasminetest.{JasmineTest, TestSuiteContext}

/*
 * Based on examples in:
 * http://lampwww.epfl.ch/~doeraene/scala-js/doc/js-interoperability.html
 */
object InteroperabilityTest extends JasmineTest {

  describe("JavaScript interoperability") {

    it("should support backquotes to escape Scala fields") {
      val obj = js.eval("""
        var interoperabilityTestFieldEscape = {
          def: 0,
          val: function(x) { if (x) this.def = x; return this.def; }
        };
        interoperabilityTestFieldEscape;
      """).asInstanceOf[InteroperabilityTestFieldEscape]

      obj.`def` = 7357
      expect(obj.`def`).toEqual(7357)
      expect(obj.`val`()).toEqual(7357)
      expect(obj.`val`(42)).toEqual(42)
    }

    it("should support @JSName to specify the JavaScript name for fields") {
      val obj = js.eval("""
        var interoperabilityTestJSName = {
          def: 42,
          val: function(x) { if (x) this.def = x; return this.def; }
        };
        interoperabilityTestJSName;
      """).asInstanceOf[InteroperabilityTestJSName]

      expect(obj.value()).toEqual(42)
      expect(obj.value(7357)).toEqual(7357)
    }

    it("should translate explicit getter and setter names to field access") {
      val obj = js.eval("""
        var interoperabilityTestProperty = { a: 1 };
        interoperabilityTestProperty;
        """).asInstanceOf[InteroperabilityTestProperty]

      expect(obj.a).toEqual(1)
      obj.a = 100
      expect(obj.a).toEqual(100)
    }

    it("should support @JSName together with field access") {
      val obj = js.eval("""
        var interoperabilityTestProperty = { b: 1 };
        interoperabilityTestProperty;
        """).asInstanceOf[InteroperabilityTestPropertyNamed]

      expect(obj.a).toEqual(1)
      obj.a = 100
      expect(obj.a).toEqual(100)
      expect(obj.b).toEqual(100)
    }

    it("should support @JSBracketAccess to specify access using []-subscription") {
      val obj = js.eval("""
        var interoperabilityTestJSBracketAccess = [ 0, 1, 7357 ];
        interoperabilityTestJSBracketAccess;
      """).asInstanceOf[InteroperabilityTestJSBracketAccess]

      expect(obj(2)).toEqual(7357)
      obj(2) = 42
      expect(obj(2)).toEqual(42)
    }

    it("should allow instanciation of JS classes inheriting from js.Object") {
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
      expect(obj.field).toEqual(42)
      expect(obj.method).toEqual("42")
      expect(obj.getConstructorParam).toEqual("Scala.js")
    }

    it("should acces top-level JS objects via Scala objects inheriting from js.Object") {
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
      expect(obj.value).toEqual("7357")
      expect(obj.valueAsInt).toEqual(7357)
    }

    it("should access native JS classes and objects nested in JS objects") {
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
      expect(obj1.x).toEqual(34)

      val obj2 = TopLevel.ContainedObject
      expect(obj2.x).toEqual(42)

      val obj3 = new TopLevel.ContainedClassWithJSName(65)
      expect(obj3.x).toEqual(130)

      val obj4 = TopLevel.ContainedObjectWithJSName
      expect(obj4.x).toEqual(4242)

      val obj5 = new TopLevel.ContainedClassWithDefaultParam()
      expect(obj5.x).toEqual(42)

      val obj6 = new TopLevel.ContainedClassWithDefaultParam(10)
      expect(obj6.x).toEqual(10)
    }

    it("should access native JS classes and objects nested in @JSName'd JS objects") {
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
      expect(obj1.x).toEqual(34)

      val obj2 = TopLevel.ContainedObject
      expect(obj2.x).toEqual(42)

      val obj3 = new TopLevel.ContainedClassWithJSName(65)
      expect(obj3.x).toEqual(130)

      val obj4 = TopLevel.ContainedObjectWithJSName
      expect(obj4.x).toEqual(4242)
    }

    it("should allow to call JS methods with variadic parameters") {
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
      expect(dyn.foo()).toEqual(js.Array())
      expect(dyn.foo(3, 6)).toEqual(js.Array(3, 6))
      expect(dyn.foo("hello", false)).toEqual(js.Array("hello", false))
      expect(dyn.applyDynamic("foo")(elems: _*)).toEqual(js.Array("plop", 42, 51))

      val stat = obj.asInstanceOf[InteroperabilityTestVariadicMethod]
      expect(stat.foo()).toEqual(js.Array())
      expect(stat.foo(3, 6)).toEqual(js.Array(3, 6))
      expect(stat.foo("hello", false)).toEqual(js.Array("hello", false))
      expect(stat.foo(elems: _*)).toEqual(js.Array("plop", 42, 51))
    }

    it("should allow to call JS constructors with variadic parameters") {
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
      expect(jsnew(ctor)().args).toEqual(js.Array())
      expect(jsnew(ctor)(3, 6).args).toEqual(js.Array(3, 6))
      expect(jsnew(ctor)("hello", false).args).toEqual(js.Array("hello", false))
      expect(jsnew(ctor)(elems: _*).args).toEqual(js.Array("plop", 42, 51))

      import org.scalajs.testsuite.compiler.{InteroperabilityTestVariadicCtor => C}
      expect(new C().args).toEqual(js.Array())
      expect(new C(3, 6).args).toEqual(js.Array(3, 6))
      expect(new C("hello", false).args).toEqual(js.Array("hello", false))
      expect(new C(elems: _*).args).toEqual(js.Array("plop", 42, 51))
    }

    it("should acces top-level JS objects via Scala object inheriting from js.GlobalScope") {
      js.eval("""
        var interoperabilityTestGlobalScopeValue = "7357";
        var interoperabilityTestGlobalScopeValueAsInt = function() {
          return parseInt(interoperabilityTestGlobalScopeValue);
        };
      """)

      // Use alias for convenience: see end of file for definition
      val Global = InteroperabilityTestGlobalScope

      expect(Global.interoperabilityTestGlobalScopeValue).toEqual("7357")
      expect(Global.interoperabilityTestGlobalScopeValueAsInt).toEqual(7357)

      Global.interoperabilityTestGlobalScopeValue = "42"
      expect(Global.interoperabilityTestGlobalScopeValueAsInt).toEqual(42)
    }

    it("should protect receiver of raw JS apply if it's a select - #804") {
      val rawReceiver = js.eval("""
        var interoperabilityTestRawReceiver = {
          member: 0xbad,
          check: function(raw) { return this.member ? this.member : raw; }
        };
        interoperabilityTestRawReceiver;
      """).asInstanceOf[InteroperabilityTestRawReceiver]

      expect(rawReceiver.check(7357)).toEqual(7357)

      val check = rawReceiver.check
      expect(check(0x600d)).toEqual(0x600d)

      class InScalaSelect(check: js.Function1[Int, Int]) {
        @JSExport
        val member: Int = 0xbad2
        def test(): Unit = expect(check(5894)).toEqual(5894)
      }
      new InScalaSelect(check).test()
    }

    it("should properly handle default parameters") {
      val obj = js.eval("""
        var interoperabilityTestDefaultParam = {
          fun: function() { return arguments; }
        };
        interoperabilityTestDefaultParam;
      """).asInstanceOf[InteroperabilityTestDefaultParam]

      // Helpers
      import js.Dynamic.{literal => args}
      val undef = js.undefined

      expect(obj.simple(1)).toEqual(args(`0` = 1))
      expect(obj.simple(1,5)).toEqual(args(`0` = 1, `1` = 5))
      expect(obj.named(y = 5)).toEqual(args(`0` = undef, `1` = 5))
      expect(obj.named(x = 5)).toEqual(args(`0` = 5))
      expect(obj.multi()(1,2,3,4)()).toEqual(args(
          `0` = undef,
          `1` = 1,
          `2` = 2,
          `3` = 3,
          `4` = 4))
      expect(obj.multi(2)()(5)).toEqual(args(
          `0` = 2,
          `1` = 5))
    }

    it("should properly handle default parameters for constructors - #791") {
      js.eval("""
        var InteroperabilityTestCtor = function(x,y) {
          this.values = Array(x || 6, y || 8)
        }
      """);

      expect(new InteroperabilityTestCtor().values).toEqual(js.Array(6,8))
      expect(new InteroperabilityTestCtor(y = 7).values).toEqual(js.Array(6,7))
      expect(new InteroperabilityTestCtor(3).values).toEqual(js.Array(3,8))
      expect(new InteroperabilityTestCtor(10, 2).values).toEqual(js.Array(10,2))
    }

    it("should generate exports for methods inherited from traits - #178") {
      import js.annotation.JSExport

      trait Foo {
        @JSExport
        def theValue: Int = 1
      }
      class Bar extends Foo

      val x = (new Bar).asInstanceOf[js.Dynamic]

      // Call the export by using js.Dynamic
      expect(x.theValue).toEqual(1)
    }

    it("should allow constructor params that are vals/vars in facades - #1277") {
      js.eval("""
          var InteroparabilityCtorInlineValue = function(x,y) {
            this.x = x;
            this.y = y;
          }
      """)

      val obj = new InteroparabilityCtorInlineValue(10, -1)

      expect(obj.x).toEqual(10)
      expect(obj.y).toEqual(-1)

      obj.y = 100

      expect(obj.x).toEqual(10)
      expect(obj.y).toEqual(100)
    }

    it("should unbox Chars received from calling a JS interop method") {
      val obj = js.eval("""
        var obj = {
          get: function() { return JSUtils().stringToChar('e'); }
        };
        obj;
      """).asInstanceOf[InteroperabilityTestCharResult]

      expect(obj.get().toInt).toEqual('e'.toInt)
    }

    it("should box Chars given to a JS interop method") {
      val obj = js.eval("""
        var obj = {
          twice: function(c) { c = JSUtils().charToString(c); return c+c; }
        };
        obj;
      """).asInstanceOf[InteroperabilityTestCharParam]

      expect(obj.twice('x')).toEqual("xx")
    }

    it("should unbox value classes received from calling a JS interop method") {
      val obj = js.eval("""
        var obj = {
          test: function(vc) { return vc; }
        };
        obj;
      """).asInstanceOf[InteroperabilityTestValueClassResult]

      val r = obj.test(new SomeValueClass(5))
      expect(r.i).toEqual(5)
    }

    it("should box value classes given to a JS interop method") {
      val obj = js.eval("""
        var obj = {
          stringOf: function(vc) { return vc.toString(); }
        };
        obj;
      """).asInstanceOf[InteroperabilityTestValueClassParam]

      val vc = new SomeValueClass(7)
      expect(obj.stringOf(vc)).toEqual("SomeValueClass(7)")
    }

    it("should not unbox values received from JS method in statement position") {
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
      if (TestSuiteContext.hasTag("compliant-asinstanceofs"))
        expect(() => obj.test()).toThrow // in expression position, should throw
    }

    when("compliant-asinstanceofs").
    it("should asInstanceOf values received from calling a JS interop method") {
      val obj = js.eval("""
        var obj = {
          testChar: function() { return 5; },
          testInt: function() { return 6.4; },
          testShort: function() { return 60000; },
          testDouble: function() { return JSUtils().stringToChar('e'); },
          testString: function() { return {}; },
          testValueClass: function() { return "hello"; },
          testNormalClass: function() { return 45; },
          testAny: function() { return {}; }
        };
        obj;
      """).asInstanceOf[InteroperabilityTestAsInstanceOfResult]

      expect(() => obj.testChar()).toThrow
      expect(() => obj.testInt()).toThrow
      expect(() => obj.testShort()).toThrow
      expect(() => obj.testDouble()).toThrow
      expect(() => obj.testString()).toThrow
      expect(() => obj.testValueClass()).toThrow
      expect(() => obj.testNormalClass()).toThrow
      expect(() => obj.testAny()).not.toThrow
    }

  }

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
    def simple(x: Int, y: Int = 5): js.Any = js.native
    @JSName("fun")
    def named(x: Int = 1, y: Int = 1, z: Int = 1): js.Any = js.native
    @JSName("fun")
    def multi(x: Int = 1)(ys: Int*)(z: Int = 1): js.Any = js.native
  }

  @js.native
  trait InteroperabilityTestCharResult extends js.Object {
    def get(): Char = js.native
  }

  @js.native
  trait InteroperabilityTestCharParam extends js.Object {
    def twice(c: Char): String = js.native
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
class InteroperabilityTestVariadicCtor(inargs: Any*) extends js.Object {
  val args: js.Array[Any] = js.native
}

@js.native
object InteroperabilityTestGlobalScope extends js.GlobalScope {
  var interoperabilityTestGlobalScopeValue: String = js.native
  def interoperabilityTestGlobalScopeValueAsInt(): Int = js.native
}

class SomeValueClass(val i: Int) extends AnyVal {
  override def toString(): String = s"SomeValueClass($i)"
}

@js.native
class InteroperabilityTestCtor(x: Int = 5, y: Int = ???) extends js.Object {
  def values: js.Array[Int] = js.native
}

@js.native
class InteroparabilityCtorInlineValue(val x: Int, var y: Int) extends js.Object
