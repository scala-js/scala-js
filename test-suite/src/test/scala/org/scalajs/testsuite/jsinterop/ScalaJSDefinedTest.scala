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

import org.scalajs.jasminetest.JasmineTest

object ScalaJSDefinedTest extends JasmineTest {

  import org.scalajs.testsuite.jsinterop.{ScalaJSDefinedTestSeparateRun => SepRun}

  // Defined in test-suite/src/test/resources/ScalaJSDefinedTestNatives.js
  @JSName("ScalaJSDefinedTestNativeParentClass")
  class NativeParentClass(val x: Int) extends js.Object {
    def foo(s: String): String = js.native

    def bar: Int = js.native
  }

  @ScalaJSDefined
  class NonNativeParentClass(val x: Int) extends js.Object {
    def foo(s: String): String = s + x

    def bar: Int = x * 2
  }

  trait NativeTraitWithDeferred extends js.Object {
    val x: Int
  }

  // Defined in test-suite/src/test/resources/ScalaJSDefinedTestNatives.js
  @JSName("ScalaJSDefinedTestNativeParentClassWithDeferred")
  abstract class NativeParentClassWithDeferred extends NativeTraitWithDeferred {
    def foo(y: Int): Int = js.native // = bar(y + 4) + x

    def bar(y: Int): Int
  }

  @ScalaJSDefined
  trait SimpleTrait extends js.Any {
    def foo(x: Int): Int
  }

  @ScalaJSDefined
  class Minimal extends js.Object

  @ScalaJSDefined
  class SimpleMethod extends js.Object {
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

  describe("Scala.js-defined JS classes") {

    it("minimal definition") {
      val obj = new Minimal
      expect(js.typeOf(obj)).toEqual("object")
      expect(js.Object.keys(obj)).toEqual(js.Array[String]())
      expect(obj.toString()).toEqual("[object Object]")
      expect(obj.getClass().asInstanceOf[js.Any]).toBeNull

      expect((obj: Any).isInstanceOf[Minimal]).toBeTruthy
      expect((obj: Any).isInstanceOf[js.Object]).toBeTruthy
      expect((obj: Any).isInstanceOf[js.Error]).toBeFalsy
    }

    it("simple method") {
      val obj = new SimpleMethod
      expect(obj.foo(5)).toEqual(8)
      expect(obj.bar("hello", 42)).toEqual("hello42")

      val dyn = obj.asInstanceOf[js.Dynamic]
      expect(dyn.foo(5)).toEqual(8)
      expect(dyn.bar("hello", 42)).toEqual("hello42")
    }

    it("simple field") {
      val obj = new SimpleField
      expect(js.Object.keys(obj)).toEqual(js.Array("x", "y"))
      expect(obj.x).toEqual(5)
      expect(obj.y).toEqual(10)
      expect(obj.sum()).toEqual(15)

      obj.y = 3
      expect(obj.y).toEqual(3)
      expect(obj.sum()).toEqual(8)

      val dyn = obj.asInstanceOf[js.Dynamic]
      expect(dyn.x).toEqual(5)
      expect(dyn.y).toEqual(3)
      expect(dyn.sum()).toEqual(8)

      dyn.y = 89
      expect(dyn.y).toEqual(89)
      expect(obj.y).toEqual(89)
      expect(dyn.sum()).toEqual(94)
    }

    it("simple accessors") {
      val obj = new SimpleAccessors
      expect(js.Object.keys(obj)).toEqual(js.Array("x"))
      expect(obj.x).toEqual(1)

      expect(obj.readPlus1).toEqual(2)
      expect(obj.neg).toEqual(-1)
      obj.neg = 4
      expect(obj.x).toEqual(-4)
      expect(obj.neg).toEqual(4)
      expect(obj.readPlus1).toEqual(-3)

      val dyn = obj.asInstanceOf[js.Dynamic]
      expect(dyn.x).toEqual(-4)

      expect(dyn.readPlus1).toEqual(-3)
      expect(dyn.neg).toEqual(4)
      dyn.neg = -9
      expect(dyn.x).toEqual(9)
      expect(dyn.neg).toEqual(-9)
      expect(dyn.readPlus1).toEqual(10)
    }

    it("simple constructor") {
      val obj = new SimpleConstructor(5, 10)
      expect(js.Object.keys(obj)).toEqual(js.Array("x", "y"))
      expect(obj.x).toEqual(5)
      expect(obj.y).toEqual(10)
      expect(obj.sum()).toEqual(15)

      obj.y = 3
      expect(obj.y).toEqual(3)
      expect(obj.sum()).toEqual(8)

      val dyn = obj.asInstanceOf[js.Dynamic]
      expect(dyn.x).toEqual(5)
      expect(dyn.y).toEqual(3)
      expect(dyn.sum()).toEqual(8)

      dyn.y = 89
      expect(dyn.y).toEqual(89)
      expect(obj.y).toEqual(89)
      expect(dyn.sum()).toEqual(94)
    }

    it("simple constructor with automatic fields") {
      val obj = new SimpleConstructorAutoFields(5, 10)
      expect(js.Object.keys(obj)).toEqual(js.Array("x", "y"))
      expect(obj.x).toEqual(5)
      expect(obj.y).toEqual(10)
      expect(obj.sum()).toEqual(15)

      obj.y = 3
      expect(obj.y).toEqual(3)
      expect(obj.sum()).toEqual(8)

      val dyn = obj.asInstanceOf[js.Dynamic]
      expect(dyn.x).toEqual(5)
      expect(dyn.y).toEqual(3)
      expect(dyn.sum()).toEqual(8)

      dyn.y = 89
      expect(dyn.y).toEqual(89)
      expect(obj.y).toEqual(89)
      expect(dyn.sum()).toEqual(94)
    }

    it("simple constructor with param accessors") {
      val obj = new SimpleConstructorParamAccessors(5, 10)
      expect(js.Object.keys(obj)).not.toEqual(js.Array("x", "y"))
      expect(obj.sum()).toEqual(15)

      val dyn = obj.asInstanceOf[js.Dynamic]
      expect(dyn.sum()).toEqual(15)
    }

    it("default values for fields") {
      val obj = new DefaultFieldValues
      expect(obj.int).toBe(0)
      expect(obj.bool).toBe(false)
      expect(obj.char.toInt).toBe(0)
      expect(obj.string).toBeNull
      expect(obj.unit).toBeUndefined

      /* Value class fields are initialized to null, instead of a boxed
       * representation of the zero of their underlying types, as for a
       * Scala class.
       */
      expect(obj.asInstanceOf[js.Dynamic].valueClass).toBeNull
    }

    it("simple inherited from a native class") {
      val obj = new SimpleInheritedFromNative(3, 5)
      expect(obj.x).toEqual(3)
      expect(obj.y).toEqual(5)
      expect(obj.bar).toEqual(6)
      expect(obj.isInstanceOf[SimpleInheritedFromNative]).toBeTruthy
      expect(obj.isInstanceOf[NativeParentClass]).toBeTruthy
    }

    it("nested inside a Scala class") {
      class OuterScalaClass(val x: Int) {
        @ScalaJSDefined
        class InnerJSClass(val y: Int) extends js.Object {
          def sum(z: Int): Int = x + y + z
        }
      }

      val outerObj = new OuterScalaClass(3)
      val obj = new outerObj.InnerJSClass(6)
      expect(obj.y).toEqual(6)
      expect(obj.sum(11)).toEqual(20)
    }

    it("nested inside a Scala.js-defined JS class") {
      @ScalaJSDefined
      class OuterJSClass(val x: Int) extends js.Object {
        @ScalaJSDefined
        class InnerJSClass(val y: Int) extends js.Object {
          def sum(z: Int): Int = x + y + z
        }
      }

      val outerObj = new OuterJSClass(3)
      val obj = new outerObj.InnerJSClass(6)
      expect(obj.y).toEqual(6)
      expect(obj.sum(11)).toEqual(20)
    }

    it("Scala class nested inside a Scala.js-defined JS class") {
      @ScalaJSDefined
      class OuterJSClass(val x: Int) extends js.Object {
        class InnerScalaClass(val y: Int) {
          def sum(z: Int): Int = x + y + z
        }
      }

      val outerObj = new OuterJSClass(3)
      val obj = new outerObj.InnerScalaClass(6)
      expect(obj.y).toEqual(6)
      expect(obj.sum(11)).toEqual(20)
    }

    it("anonymous class with captures") {
      val x = (() => 5)()
      val obj = new js.Object {
        val y = 10
        def sum(z: Int): Int = x + y + z
      }

      val dyn = obj.asInstanceOf[js.Dynamic]
      expect(dyn.y).toEqual(10)
      expect(dyn.sum(11)).toEqual(26)
    }

    it("methods with explicit name") {
      @ScalaJSDefined
      class MethodsWithExplicitName extends js.Object {
        @JSName("theAnswer")
        def bar(): Int = 42
        @JSName("doubleTheParam")
        def double(x: Int): Int = x*2
      }

      val foo = new MethodsWithExplicitName
      expect(foo.bar()).toEqual(42)
      expect(foo.double(3)).toEqual(6)

      val dyn = foo.asInstanceOf[js.Dynamic]
      expect(dyn.bar).toBeUndefined
      expect(js.typeOf(dyn.theAnswer)).toBe("function")
      expect(dyn.theAnswer()).toEqual(42)
      expect(dyn.doubleTheParam(3)).toEqual(6)
    }

    it("methods with constant folded name") {
      @ScalaJSDefined
      class MethodsWithConstantFoldedName extends js.Object {
        @JSName(JSNameHolder.MethodName)
        def bar(): Int = 42
      }

      val foo = new MethodsWithConstantFoldedName
      expect(foo.bar()).toEqual(42)

      val dyn = foo.asInstanceOf[js.Dynamic]
      expect(dyn.bar).toBeUndefined
      expect(dyn.myMethod()).toEqual(42)
    }

    it("protected methods") {
      @ScalaJSDefined
      class ProtectedMethods extends js.Object {
        protected def bar(): Int = 42

        protected[testsuite] def foo(): Int = 100
      }

      val foo = new ProtectedMethods
      expect(foo.foo()).toEqual(100)

      val dyn = foo.asInstanceOf[js.Dynamic]
      expect(js.typeOf(dyn.bar)).toBe("function")
      expect(dyn.bar()).toEqual(42)
      expect(js.typeOf(dyn.foo)).toBe("function")
      expect(dyn.foo()).toEqual(100)
    }

    it("properties with explicit name") {
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
      expect(foo.answerScala).toEqual(42)
      expect(foo.xScala).toEqual(3)
      expect(foo.doubleXScala).toEqual(6)
      foo.xScala = 23
      expect(foo.xScala).toEqual(23)
      expect(foo.doubleXScala).toEqual(46)
      expect(foo.yGetter).toEqual("hello get")
      foo.ySetter_=("world")
      expect(foo.yGetter).toEqual("world set get")

      val dyn = (new PropertiesWithExplicitName).asInstanceOf[js.Dynamic]
      expect(dyn.answerScala).toBeUndefined
      expect(js.typeOf(dyn.answer)).toBe("number")
      expect(dyn.answer).toEqual(42)
      expect(dyn.x).toEqual(3)
      expect(dyn.doubleX).toEqual(6)
      dyn.x = 23
      expect(dyn.x).toEqual(23)
      expect(dyn.doubleX).toEqual(46)
      expect(dyn.y).toEqual("hello get")
      dyn.y = "world"
      expect(dyn.y).toEqual("world set get")
    }

    it("protected properties") {
      @ScalaJSDefined
      class ProtectedProperties extends js.Object {
        protected val x: Int = 42
        protected[testsuite] val y: Int = 43
      }

      val foo = new ProtectedProperties
      expect(foo.y).toEqual(43)

      val dyn = foo.asInstanceOf[js.Dynamic]
      expect(dyn.x).toEqual(42)
      expect(dyn.y).toEqual(43)
    }

    it("simple overloaded methods") {
      @ScalaJSDefined
      class SimpleOverloadedMethods extends js.Object {
        def foo(): Int = 42
        def foo(x: Int): Int = x*2
      }

      val foo = new SimpleOverloadedMethods
      expect(foo.foo()).toEqual(42)
      expect(foo.foo(3)).toEqual(6)

      val dyn = foo.asInstanceOf[js.Dynamic]
      expect(js.typeOf(dyn.foo)).toBe("function")
      expect(dyn.foo()).toEqual(42)
      expect(dyn.foo(3)).toEqual(6)
    }

    it("renamed overloaded methods") {
      @ScalaJSDefined
      class RenamedOverloadedMethods extends js.Object {
        @JSName("foobar")
        def foo(): Int = 42
        @JSName("foobar")
        def bar(x: Int): Int = x*2
      }

      val foo = new RenamedOverloadedMethods
      expect(foo.foo()).toEqual(42)
      expect(foo.bar(3)).toEqual(6)

      val dyn = foo.asInstanceOf[js.Dynamic]
      expect(js.typeOf(dyn.foobar)).toBe("function")
      expect(dyn.foobar()).toEqual(42)
      expect(dyn.foobar(3)).toEqual(6)
    }

    it("default parameters") {
      @ScalaJSDefined
      class DefaultParameters extends js.Object {
        def bar(x: Int, y: Int = 1): Int = x + y
        def dependent(x: Int)(y: Int = x + 1): Int = x + y

        def foobar(x: Int): Int = bar(x)
      }

      val foo = new DefaultParameters
      expect(foo.bar(4, 5)).toEqual(9)
      expect(foo.bar(4)).toEqual(5)
      expect(foo.foobar(3)).toEqual(4)
      expect(foo.dependent(4)(5)).toEqual(9)
      expect(foo.dependent(8)()).toEqual(17)

      val dyn = foo.asInstanceOf[js.Dynamic]
      expect(dyn.bar(4, 5)).toEqual(9)
      expect(dyn.bar(4)).toEqual(5)
      expect(dyn.foobar(3)).toEqual(4)
      expect(dyn.dependent(4, 5)).toEqual(9)
      expect(dyn.dependent(8)).toEqual(17)
    }

    it("override default parameters") {
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
      expect(foo.bar(4, 5)).toEqual(9)
      expect(foo.bar(4)).toEqual(14)
      expect(foo.foobar(3)).toEqual(13)
      expect(foo.dependent(4)(5)).toEqual(9)
      expect(foo.dependent(8)()).toEqual(24)

      val parent: OverrideDefaultParametersParent = foo
      expect(parent.bar(4, 5)).toEqual(9)
      expect(parent.bar(4)).toEqual(14)
      expect(parent.foobar(3)).toEqual(13)
      expect(parent.dependent(4)(5)).toEqual(9)
      expect(parent.dependent(8)()).toEqual(24)

      val dyn = foo.asInstanceOf[js.Dynamic]
      expect(dyn.bar(4, 5)).toEqual(9)
      expect(dyn.bar(4)).toEqual(14)
      expect(dyn.foobar(3)).toEqual(13)
      expect(dyn.dependent(4, 5)).toEqual(9)
      expect(dyn.dependent(8)).toEqual(24)
    }

    it("override method with default parameters without new default") {
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
      expect(foo.bar(4, 5)).toEqual(-1)
      expect(foo.bar(4)).toEqual(3)
      expect(foo.foobar(3)).toEqual(2)
      expect(foo.dependent(4)(8)).toEqual(-4)
      expect(foo.dependent(8)()).toEqual(-1)

      val parent: OverrideDefaultParametersWithoutDefaultParent = foo
      expect(parent.bar(4, 5)).toEqual(-1)
      expect(parent.bar(4)).toEqual(3)
      expect(parent.foobar(3)).toEqual(2)
      expect(parent.dependent(4)(8)).toEqual(-4)
      expect(parent.dependent(8)()).toEqual(-1)

      val dyn = foo.asInstanceOf[js.Dynamic]
      expect(dyn.bar(4, 5)).toEqual(-1)
      expect(dyn.bar(4)).toEqual(3)
      expect(dyn.foobar(3)).toEqual(2)
      expect(dyn.dependent(4, 8)).toEqual(-4)
      expect(dyn.dependent(8)).toEqual(-1)
    }

    it("override native method") {
      @ScalaJSDefined
      class OverrideNativeMethod extends NativeParentClass(3) {
        override def foo(s: String): String = s + s + x
      }

      val foo = new OverrideNativeMethod
      expect(foo.x).toEqual(3)
      expect(foo.foo("hello")).toEqual("hellohello3")

      val parent: NativeParentClass = foo
      expect(parent.x).toEqual(3)
      expect(parent.foo("hello")).toEqual("hellohello3")

      val dyn = foo.asInstanceOf[js.Dynamic]
      expect(dyn.x).toEqual(3)
      expect(dyn.foo("hello")).toEqual("hellohello3")
    }

    it("override non-native method") {
      @ScalaJSDefined
      class OverrideNonNativeMethod extends NonNativeParentClass(3) {
        override def foo(s: String): String = s + s + x
      }

      val foo = new OverrideNonNativeMethod
      expect(foo.x).toEqual(3)
      expect(foo.foo("hello")).toEqual("hellohello3")

      val parent: NonNativeParentClass = foo
      expect(parent.x).toEqual(3)
      expect(parent.foo("hello")).toEqual("hellohello3")

      val dyn = foo.asInstanceOf[js.Dynamic]
      expect(dyn.x).toEqual(3)
      expect(dyn.foo("hello")).toEqual("hellohello3")
    }

    it("override non-native method with separate compilation") {
      val foo = new SepRun.SimpleChildClass
      expect(foo.foo(3)).toEqual(6)

      val fooParent: SepRun.SimpleParentClass = foo
      expect(fooParent.foo(3)).toEqual(6)

      val dyn = foo.asInstanceOf[js.Dynamic]
      expect(foo.foo(3)).toEqual(6)
    }

    it("override native method and call super") {
      @ScalaJSDefined
      class OverrideNativeMethodSuperCall extends NativeParentClass(3) {
        override def foo(s: String): String = super.foo("bar") + s
      }

      val foo = new OverrideNativeMethodSuperCall
      expect(foo.x).toEqual(3)
      expect(foo.foo("hello")).toEqual("bar3hello")

      val parent: NativeParentClass = foo
      expect(parent.x).toEqual(3)
      expect(parent.foo("hello")).toEqual("bar3hello")

      val dyn = foo.asInstanceOf[js.Dynamic]
      expect(dyn.x).toEqual(3)
      expect(dyn.foo("hello")).toEqual("bar3hello")
    }

    it("override non-native method and call super") {
      @ScalaJSDefined
      class OverrideNonNativeMethodSuperCall extends NonNativeParentClass(3) {
        override def foo(s: String): String = super.foo("bar") + s
      }

      val foo = new OverrideNonNativeMethodSuperCall
      expect(foo.x).toEqual(3)
      expect(foo.foo("hello")).toEqual("bar3hello")

      val parent: NonNativeParentClass = foo
      expect(parent.x).toEqual(3)
      expect(parent.foo("hello")).toEqual("bar3hello")

      val dyn = foo.asInstanceOf[js.Dynamic]
      expect(dyn.x).toEqual(3)
      expect(dyn.foo("hello")).toEqual("bar3hello")
    }

    it("override native val") {
      @ScalaJSDefined
      class OverrideNativeVal extends NativeParentClass(3) {
        override val x: Int = 42
      }

      val foo = new OverrideNativeVal
      expect(foo.x).toEqual(42)
      expect(foo.bar).toEqual(84)
      expect(foo.foo("hello")).toEqual("hello42")

      val parent: NativeParentClass = foo
      expect(parent.x).toEqual(42)
      expect(parent.bar).toEqual(84)
      expect(parent.foo("hello")).toEqual("hello42")

      val dyn = foo.asInstanceOf[js.Dynamic]
      expect(dyn.x).toEqual(42)
      expect(dyn.bar).toEqual(84)
      expect(dyn.foo("hello")).toEqual("hello42")
    }

    it("override non-native val") {
      @ScalaJSDefined
      class OverrideNonNativeVal extends NonNativeParentClass(3) {
        override val x: Int = 42
      }

      val foo = new OverrideNonNativeVal
      expect(foo.x).toEqual(42)
      expect(foo.bar).toEqual(84)
      expect(foo.foo("hello")).toEqual("hello42")

      val parent: NonNativeParentClass = foo
      expect(parent.x).toEqual(42)
      expect(parent.bar).toEqual(84)
      expect(parent.foo("hello")).toEqual("hello42")

      val dyn = foo.asInstanceOf[js.Dynamic]
      expect(dyn.x).toEqual(42)
      expect(dyn.bar).toEqual(84)
      expect(dyn.foo("hello")).toEqual("hello42")
    }

    it("override native getter") {
      @ScalaJSDefined
      class OverrideNativeGetter extends NativeParentClass(3) {
        override def bar: Int = x * 3
      }

      val foo = new OverrideNativeGetter
      expect(foo.x).toEqual(3)
      expect(foo.bar).toEqual(9)

      val parent: NativeParentClass = foo
      expect(parent.x).toEqual(3)
      expect(parent.bar).toEqual(9)

      val dyn = foo.asInstanceOf[js.Dynamic]
      expect(dyn.x).toEqual(3)
      expect(dyn.bar).toEqual(9)
    }

    it("override non-native getter") {
      @ScalaJSDefined
      class OverrideNonNativeGetter extends NonNativeParentClass(3) {
        override def bar: Int = x * 3
      }

      val foo = new OverrideNonNativeGetter
      expect(foo.x).toEqual(3)
      expect(foo.bar).toEqual(9)

      val parent: NonNativeParentClass = foo
      expect(parent.x).toEqual(3)
      expect(parent.bar).toEqual(9)

      val dyn = foo.asInstanceOf[js.Dynamic]
      expect(dyn.x).toEqual(3)
      expect(dyn.bar).toEqual(9)
    }

    it("override native getter with val") {
      @ScalaJSDefined
      class OverrideNativeGetterWithVal extends NativeParentClass(3) {
        override val bar: Int = 1
      }

      val foo = new OverrideNativeGetterWithVal
      expect(foo.x).toEqual(3)
      expect(foo.bar).toEqual(1)

      val parent: NativeParentClass = foo
      expect(parent.x).toEqual(3)
      expect(parent.bar).toEqual(1)

      val dyn = foo.asInstanceOf[js.Dynamic]
      expect(dyn.x).toEqual(3)
      expect(dyn.bar).toEqual(1)
    }

    it("override non-native getter with val") {
      @ScalaJSDefined
      class OverrideNonNativeGetterWithVal extends NonNativeParentClass(3) {
        override val bar: Int = 1
      }

      val foo = new OverrideNonNativeGetterWithVal
      expect(foo.x).toEqual(3)
      expect(foo.bar).toEqual(1)

      val parent: NonNativeParentClass = foo
      expect(parent.x).toEqual(3)
      expect(parent.bar).toEqual(1)

      val dyn = foo.asInstanceOf[js.Dynamic]
      expect(dyn.x).toEqual(3)
      expect(dyn.bar).toEqual(1)
    }

    it("override getter with super") {
      @ScalaJSDefined
      class OverrideGetterSuperParent extends js.Object {
        def bar: Int = 43
      }
      @ScalaJSDefined
      class OverrideGetterSuperChild extends OverrideGetterSuperParent {
        override def bar: Int = super.bar * 3
      }

      val foo = new OverrideGetterSuperChild
      expect(foo.bar).toEqual(129)

      val parent: OverrideGetterSuperParent = foo
      expect(parent.bar).toEqual(129)

      val dyn = foo.asInstanceOf[js.Dynamic]
      expect(dyn.bar).toEqual(129)
    }

    it("override setter with super") {
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
      expect(foo.x).toEqual(12)

      val parent: OverrideSetterSuperParent = foo
      parent.bar_=(5)
      expect(parent.x).toEqual(15)

      val dyn = foo.asInstanceOf[js.Dynamic]
      dyn.bar = 6
      expect(dyn.x).toEqual(18)
    }

    it("add setter in subclass") {
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
      expect(foo.x).toEqual(4)
      expect(foo.bar).toEqual(4)

      val dyn = foo.asInstanceOf[js.Dynamic]
      dyn.bar = 6
      expect(dyn.x).toEqual(6)
      expect(dyn.bar).toEqual(6)
    }

    it("add getter in subclass") {
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
      expect(foo.x).toEqual(4)
      expect(foo.bar).toEqual(4)

      val dyn = foo.asInstanceOf[js.Dynamic]
      dyn.bar = 6
      expect(dyn.x).toEqual(6)
      expect(dyn.bar).toEqual(6)
    }

    it("overload native method") {
      @ScalaJSDefined
      class OverloadNativeMethod extends NativeParentClass(3) {
        def foo(s: String, y: Int): String = foo(s) + " " + y
      }

      val foo = new OverloadNativeMethod
      expect(foo.foo("hello")).toEqual("hello3")
      expect(foo.foo("hello", 4)).toEqual("hello3 4")

      val parent: NativeParentClass = foo
      expect(parent.foo("hello")).toEqual("hello3")

      val dyn = foo.asInstanceOf[js.Dynamic]
      expect(dyn.foo("hello")).toEqual("hello3")
      expect(dyn.foo("hello", 4)).toEqual("hello3 4")
    }

    it("overload non-native method") {
      @ScalaJSDefined
      class OverloadNonNativeMethod extends NonNativeParentClass(3) {
        def foo(s: String, y: Int): String = foo(s) + " " + y
      }

      val foo = new OverloadNonNativeMethod
      expect(foo.foo("hello")).toEqual("hello3")
      expect(foo.foo("hello", 4)).toEqual("hello3 4")

      val parent: NonNativeParentClass = foo
      expect(parent.foo("hello")).toEqual("hello3")

      val dyn = foo.asInstanceOf[js.Dynamic]
      expect(dyn.foo("hello")).toEqual("hello3")
      expect(dyn.foo("hello", 4)).toEqual("hello3 4")
    }

    it("implement a simple trait") {
      @ScalaJSDefined
      class ImplementSimpleTrait extends js.Object with SimpleTrait {
        def foo(x: Int): Int = x + 1
      }

      val foo = new ImplementSimpleTrait
      expect(foo.foo(3)).toEqual(4)

      val fooTrait: SimpleTrait = foo
      expect(fooTrait.foo(5)).toEqual(6)
    }

    it("implement a simple trait under separate compilation") {
      @ScalaJSDefined
      class ImplementSimpleTraitSepRun extends js.Object with SepRun.SimpleTrait {
        def foo(x: Int): Int = x + 1
      }

      val foo = new ImplementSimpleTraitSepRun
      expect(foo.foo(3)).toEqual(4)

      val fooTrait: SepRun.SimpleTrait = foo
      expect(fooTrait.foo(5)).toEqual(6)
    }

    it("implement a trait with a val") {
      @ScalaJSDefined
      trait TraitWithVal extends js.Object {
        val x: Int
      }

      @ScalaJSDefined
      class ImplWithVal extends TraitWithVal {
        val x: Int = 3
      }

      val foo = new ImplWithVal
      expect(foo.x).toEqual(3)

      val fooTrait: TraitWithVal = foo
      expect(fooTrait.x).toEqual(3)
    }

    it("implement a trait with a var") {
      @ScalaJSDefined
      trait TraitWithVar extends js.Object {
        var x: Int
      }

      @ScalaJSDefined
      class ImplWithVar extends TraitWithVar {
        var x: Int = 3
      }

      val foo = new ImplWithVar
      expect(foo.x).toEqual(3)

      val fooTrait: TraitWithVar = foo
      expect(fooTrait.x).toEqual(3)

      foo.x = 5
      expect(fooTrait.x).toEqual(5)
      fooTrait.x = 19
      expect(foo.x).toEqual(19)
    }

    it("implement a trait extending a native JS class") {
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
      expect(foo.foobar(6)).toEqual(18)
    }

    it("implement abstract members coming from a native JS class") {
      @ScalaJSDefined
      class ImplDeferredMembersFromJSParent
          extends NativeParentClassWithDeferred {
        val x: Int = 43

        def bar(y: Int): Int = y * 2
      }

      val FooResult = (12 + 4) * 2 + 43

      val foo = new ImplDeferredMembersFromJSParent
      expect(foo.x).toEqual(43)
      expect(foo.bar(32)).toEqual(64)
      expect(foo.foo(12)).toEqual(FooResult)

      val fooParent: NativeParentClassWithDeferred = foo
      expect(fooParent.x).toEqual(43)
      expect(fooParent.bar(32)).toEqual(64)
      expect(fooParent.foo(12)).toEqual(FooResult)

      val dyn = foo.asInstanceOf[js.Dynamic]
      expect(dyn.x).toEqual(43)
      expect(dyn.bar(32)).toEqual(64)
      expect(dyn.foo(12)).toEqual(FooResult)
    }

  }

  class SomeValueClass(val i: Int) extends AnyVal

  object JSNameHolder {
    final val MethodName = "myMethod"
  }

}
