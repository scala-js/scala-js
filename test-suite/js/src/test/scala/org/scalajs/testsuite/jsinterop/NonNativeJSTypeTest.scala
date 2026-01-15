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

import scala.collection.mutable

import scala.scalajs.js
import scala.scalajs.js.annotation._

import org.junit.Assert._
import org.junit.Assume._
import org.junit.Test

import org.scalajs.testsuite.utils.JSAssert._
import org.scalajs.testsuite.utils.Platform
import org.scalajs.testsuite.utils.AssertThrows.assertThrows

class NonNativeJSTypeTest {
  import org.scalajs.testsuite.jsinterop.{NonNativeJSTypeTestSeparateRun => SepRun}
  import NonNativeJSTypeTest._

  @Test def minimalDefinition(): Unit = {
    val obj = new Minimal
    assertEquals("object", js.typeOf(obj))
    assertEquals(List[String](), js.Object.keys(obj).toList)
    assertEquals("[object Object]", obj.toString())
    assertNull(obj.getClass().asInstanceOf[js.Any])

    assertTrue((obj: Any).isInstanceOf[Minimal])
    assertTrue((obj: Any).isInstanceOf[js.Object])
    assertFalse((obj: Any).isInstanceOf[js.Error])
  }

  @Test def minimalStaticObjectWithLazyInitialization(): Unit = {
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

  @Test def simpleMethod(): Unit = {
    val obj = new SimpleMethod
    assertEquals(8, obj.foo(5))
    assertEquals("hello42", obj.bar("hello", 42))

    val dyn = obj.asInstanceOf[js.Dynamic]
    assertEquals(8, dyn.foo(5))
    assertEquals("hello42", dyn.bar("hello", 42))
  }

  @Test def staticObjectWithSimpleMethod(): Unit = {
    val obj = StaticObjectSimpleMethod
    assertEquals(8, obj.foo(5))
    assertEquals("hello42", obj.bar("hello", 42))

    val dyn = obj.asInstanceOf[js.Dynamic]
    assertEquals(8, dyn.foo(5))
    assertEquals("hello42", dyn.bar("hello", 42))
  }

  @Test def simpleField(): Unit = {
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

  @Test def staticObjectWithSimpleField(): Unit = {
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

  @Test def simpleAccessors(): Unit = {
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

  @Test def simpleConstructor(): Unit = {
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

  @Test def simpleConstructorWithAutomaticFields(): Unit = {
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

  @Test def simpleConstructorWithParamAccessors(): Unit = {
    val obj = new SimpleConstructorParamAccessors(5, 10)
    assertNotEquals(Array("x", "y"), js.Object.keys(obj).toArray)
    assertEquals(15, obj.sum())

    val dyn = obj.asInstanceOf[js.Dynamic]
    assertEquals(15, dyn.sum())
  }

  @Test def constructorWithParamNameClashes_Issue3933(): Unit = {
    val obj = new ConstructorWithParamNameClashes(1, 2, 3, 4, 5, 6)
    assertEquals(List(1, 2, 3, 4, 5, 6), obj.allArgs)
  }

  @Test def methodNamedConstructor(): Unit = {
    val obj1 = new MethodNamedConstructor(5)
    assertEquals(5, obj1.x)
    assertEquals(7, obj1.constructor(2))
    assertNotSame(
        js.constructorOf[MethodNamedConstructor], obj1.asInstanceOf[js.Dynamic].constructor)

    val obj2 = new SubclassOfMethodNamedConstructor(11, 15)
    assertEquals(11, obj2.x)
    assertEquals(15, obj2.z)
    assertEquals(13, obj2.constructor(2))
    assertEquals("foo 15", obj2.constructor("foo "))

    // Undesirable behavior, but the same as what would happen if we did it in JavaScript
    val obj3 = new SubclassOfMethodNamedConstructorNoRedefine(42)
    assertSame(js.constructorOf[SubclassOfMethodNamedConstructorNoRedefine],
        obj3.asInstanceOf[js.Dynamic].constructor)
    if (Platform.useECMAScript2015Semantics)
      assertThrows(classOf[Exception], obj3.constructor(1))
  }

  @Test def defaultValuesForFields(): Unit = {
    val obj = new DefaultFieldValues
    assertEquals(0, obj.int)
    assertEquals(false, obj.bool)
    assertEquals(0, obj.char.toInt)
    assertNull(obj.string)
    assertJSUndefined(obj.unit)

    /* There is an additional test for value class fields in
     * NonNativeJSTypeTestScala2.scala, which asserts that they are (wrongly)
     * instantiated to `null`.
     */
  }

  @Test def lazyVals(): Unit = {
    val obj1 = new LazyValFields()
    assertEquals(0, obj1.initCount)
    assertEquals(42, obj1.field)
    assertEquals(1, obj1.initCount)
    assertEquals(42, obj1.field)
    assertEquals(1, obj1.initCount)
    assertEquals(42, obj1.asInstanceOf[js.Dynamic].field)
    assertEquals(1, obj1.initCount)
    assertEquals(42, (obj1: LazyValFieldsSuperTrait).field)
    assertEquals(1, obj1.initCount)

    val obj2 = new LazyValFields().asInstanceOf[js.Dynamic]
    assertEquals(0, obj2.initCount)
    assertEquals(42, obj2.field)
    assertEquals(1, obj2.initCount)
    assertEquals(42, obj2.field)
    assertEquals(1, obj2.initCount)
    assertEquals(42, obj2.asInstanceOf[LazyValFields].field)
    assertEquals(1, obj2.initCount)
    assertEquals(42, obj2.asInstanceOf[LazyValFieldsSuperTrait].field)
    assertEquals(1, obj2.initCount)

    val obj3: LazyValFieldsSuperTrait = new LazyValFields()
    assertEquals(0, obj3.initCount)
    assertEquals(42, obj3.field)
    assertEquals(1, obj3.initCount)
    assertEquals(42, obj3.field)
    assertEquals(1, obj3.initCount)
    assertEquals(42, obj3.asInstanceOf[LazyValFields].field)
    assertEquals(1, obj3.initCount)
    assertEquals(42, obj3.asInstanceOf[js.Dynamic].field)
    assertEquals(1, obj3.initCount)
  }

  @Test def overrideLazyVals(): Unit = {
    val obj1 = new OverrideLazyValFields()
    assertEquals(0, obj1.initCount)
    assertEquals(53, obj1.field)
    assertEquals(1, obj1.initCount)
    assertEquals(53, obj1.field)
    assertEquals(1, obj1.initCount)
    assertEquals(53, obj1.asInstanceOf[js.Dynamic].field)
    assertEquals(1, obj1.initCount)
    assertEquals(53, (obj1: LazyValFieldsSuperTrait).field)
    assertEquals(1, obj1.initCount)
    assertEquals(53, (obj1: LazyValFields).field)
    assertEquals(1, obj1.initCount)

    val obj2 = new OverrideLazyValFields()
    assertEquals(0, obj2.initCount)
    assertEquals(53, (obj2: LazyValFields).field)
    assertEquals(1, obj2.initCount)
    assertEquals(53, obj2.field)
    assertEquals(1, obj2.initCount)
    assertEquals(53, obj2.field)
    assertEquals(1, obj2.initCount)
    assertEquals(53, obj2.asInstanceOf[js.Dynamic].field)
    assertEquals(1, obj2.initCount)
    assertEquals(53, (obj2: LazyValFieldsSuperTrait).field)
    assertEquals(1, obj2.initCount)
  }

  @Test def nullingOutLazyValField_Issue3422(): Unit =
    assertEquals("foo", new NullingOutLazyValFieldBug3422("foo").str)

  @Test def simpleInheritedFromNativeClass(): Unit = {
    val obj = new SimpleInheritedFromNative(3, 5)
    assertEquals(3, obj.x)
    assertEquals(5, obj.y)
    assertEquals(6, obj.bar)
    assertTrue(obj.isInstanceOf[SimpleInheritedFromNative])
    assertTrue(obj.isInstanceOf[NativeParentClass])
  }

  @Test def doubleUnderscoreInMemberNames_Issue3784(): Unit = {
    class DoubleUnderscoreInMemberNames extends js.Object {
      val x__y: String = "xy"
      def foo__bar(x: Int): Int = x + 1
      def ba__bar: String = "babar"
    }

    val obj = new DoubleUnderscoreInMemberNames
    assertEquals("xy", obj.x__y)
    assertEquals(6, obj.foo__bar(5))
    assertEquals("babar", obj.ba__bar)
  }

  @Test def lambdaInsideMethod_Issue2220(): Unit = {
    class LambdaInsideMethod extends js.Object {
      def foo(): Int =
        List(1, 2, 3).map(_ * 2).sum
    }

    assertEquals(12, new LambdaInsideMethod().foo())
  }

  @Test def nestedInsideScalaClass(): Unit = {
    class OuterScalaClass(val x: Int) {
      class InnerJSClass(val y: Int) extends js.Object {
        def sum(z: Int): Int = x + y + z
      }
    }

    val outerObj = new OuterScalaClass(3)
    val obj = new outerObj.InnerJSClass(6)
    assertEquals(6, obj.y)
    assertEquals(20, obj.sum(11))
  }

  @Test def nestedInsideScalaJSDefinedJSClass(): Unit = {
    class OuterJSClass(val x: Int) extends js.Object {
      class InnerJSClass(val y: Int) extends js.Object {
        def sum(z: Int): Int = x + y + z
      }
    }

    val outerObj = new OuterJSClass(3)
    val obj = new outerObj.InnerJSClass(6)
    assertEquals(6, obj.y)
    assertEquals(20, obj.sum(11))
  }

  @Test def scalaClassNestedInsideScalaJSDefinedJSClass(): Unit = {
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

  @Test def scalaObjectNestedInsideScalaJSDefinedJSClass(): Unit = {
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

  // #2772
  @Test def scalaObjectNestedInsideScalaJSDefinedJSClassJSName(): Unit = {
    class Foo extends js.Object {
      var innerInitCount: Int = _

      @JSName("innerName")
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
    val inner2 = dyn.innerName
    assertEquals(1, dyn.innerInitCount)
    assertTrue((dyn.innerName: AnyRef) eq inner2)
    assertEquals(1, dyn.innerInitCount)

    assertFalse((inner2: AnyRef) eq inner1)
  }

  @Test def anonymousClassWithCaptures(): Unit = {
    val x = (() => 5)()
    val obj = new js.Object {
      val y = 10
      def sum(z: Int): Int = x + y + z
    }

    val dyn = obj.asInstanceOf[js.Dynamic]
    assertEquals(10, dyn.y)
    assertEquals(26, dyn.sum(11))
  }

  @Test def anonymousClassHasNoOwnPrototype(): Unit = {
    val obj = new js.Object {
      val x = 1
    }

    assertEquals(1, obj.asInstanceOf[js.Dynamic].x)
    assertSame(js.Object.getPrototypeOf(obj),
        js.constructorOf[js.Object].prototype)
  }

  @Test def localClassHasOwnPrototype(): Unit = {
    class Local extends js.Object {
      val x = 1
    }

    val obj = new Local

    assertEquals(1, obj.asInstanceOf[js.Dynamic].x)

    val prototype = js.Object.getPrototypeOf(obj)

    assertNotSame(prototype, js.constructorOf[js.Object].prototype)
    assertSame(prototype, js.constructorOf[Local].prototype)
  }

  @Test def anonymousClassNonTrivialSupertype(): Unit = {
    val obj = new SimpleConstructor(1, 2) {
      val z = sum()
    }

    assertEquals(3, obj.asInstanceOf[js.Dynamic].z)
  }

  @Test def anonymousClassUsingOwnMethodInCtor(): Unit = {
    val obj = new js.Object {
      val y = inc(0)
      def inc(x: Int) = x + 1
    }

    assertEquals(1, obj.asInstanceOf[js.Dynamic].y)
  }

  @Test def anonymousClassUninitializedFields(): Unit = {
    val obj = new js.Object {
      var x: String = _
      var y: Int = _
    }

    assertNull(obj.asInstanceOf[js.Dynamic].x)
    assertEquals(0, obj.asInstanceOf[js.Dynamic].y)
  }

  @Test def anonymousClassFieldInitOrder(): Unit = {
    val obj = new js.Object {
      val x = getY
      val y = "Hello World"

      private def getY: String = y
    }.asInstanceOf[js.Dynamic]

    assertNull(obj.x)
    assertEquals("Hello World", obj.y)
  }

  @Test def anonymousClassDependentFields(): Unit = {
    val obj = new js.Object {
      val x = 1
      val y = x + 1
    }

    assertEquals(2, obj.asInstanceOf[js.Dynamic].y)
  }

  @Test def anonymousClassUseThisInCtor(): Unit = {
    var obj0: js.Object = null
    val obj1 = new js.Object {
      obj0 = this
    }

    assertSame(obj0, obj1)
  }

  @Test def nestedAnonymousClasses(): Unit = {
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

  @Test def nestedAnonymousClassesAndLambdas(): Unit = {
    def call(f: Int => js.Any) = f(1)

    // Also check that f's capture is properly transformed.
    val obj = call(x => new js.Object { val f: js.Any = (y: Int) => x + y })
    val res = obj.asInstanceOf[js.Dynamic].f(3)
    assertEquals(4, res)

    assertEquals(1, call(x => x))
  }

  @Test def anonymousClassesPrivateFieldsAreNotVisible_Issue2748(): Unit = {
    trait TheOuter extends js.Object {
      val id: String
      val paint: js.UndefOr[TheInner] = js.undefined
    }

    trait TheInner extends js.Object {
      val options: js.UndefOr[String] = js.undefined
    }

    def someValue = "some-value"

    val pcFn = someValue

    val r0 = new TheOuter {
      override val id: String = "some-" + pcFn
      override val paint: js.UndefOr[TheInner] = {
        new TheInner {
          override val options: js.UndefOr[String] = "{" + pcFn + "}"
        }
      }
    }

    assertEquals(
        """{"id":"some-some-value","paint":{"options":"{some-value}"}}""",
        js.JSON.stringify(r0))
  }

  @Test def localObjectIsLazy(): Unit = {
    var initCount: Int = 0

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

  @Test def localObjectWithCaptures(): Unit = {
    val x = (() => 5)()

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

  @Test def objectInScalaJSDefinedJSClass(): Unit = {
    class Foo extends js.Object {
      var innerInitCount: Int = _

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

  @Test def localDefsAreNotExposed(): Unit = {
    class LocalDefsAreNotExposed extends js.Object {
      def foo(): String = {
        def bar(): String = "hello"
        bar()
      }
    }

    val obj = new LocalDefsAreNotExposed
    assertFalse(js.Object.properties(obj).exists(_.contains("bar")))
  }

  @Test def localObjectsAreNotExposed(): Unit = {
    class LocalObjectsAreNotExposed extends js.Object {
      def foo(): String = {
        object Bar
        Bar.toString()
      }
    }

    val obj = new LocalObjectsAreNotExposed
    assertFalse(js.Object.properties(obj).exists(_.contains("Bar")))
  }

  @Test def localDefsWithCaptures_Issue1975(): Unit = {
    class LocalDefsWithCaptures extends js.Object {
      def foo(suffix: String): String = {
        def bar(): String = "hello " + suffix
        bar()
      }
    }

    val obj = new LocalDefsWithCaptures
    assertEquals("hello world", obj.foo("world"))
  }

  @Test def methodsWithExplicitName(): Unit = {
    class MethodsWithExplicitName extends js.Object {
      @JSName("theAnswer")
      def bar(): Int = 42
      @JSName("doubleTheParam")
      def double(x: Int): Int = x * 2
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

  @Test def methodsWithConstantFoldedName(): Unit = {
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

  @Test def protectedMethods(): Unit = {
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

  @Test def readonlyProperties(): Unit = {
    // Named classes
    class Foo extends js.Object {
      def bar: Int = 1
    }

    val x: js.Dynamic = (new Foo()).asInstanceOf[js.Dynamic]
    assertThrows(classOf[js.JavaScriptException], {
      x.bar = 2
    })

    // Read the property to trick GCC into not discarding the writes.
    assertEquals(1, x.bar)

    // Anonymous classes
    val y = new js.Object {
      def bar: Int = 1
    }.asInstanceOf[js.Dynamic]

    assertThrows(classOf[js.JavaScriptException], {
      y.bar = 2
    })

    // Read the property to trick GCC into not discarding the writes.
    assertEquals(1, y.bar)
  }

  @Test def propertiesAreNotEnumerable(): Unit = {
    // Named classes
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

  @Test def propertiesAreConfigurable(): Unit = {
    // Named classes
    class Foo extends js.Object {
      def myProp: Int = 1
    }

    // Delete property from prototype.
    val prototype = js.constructorOf[Foo].prototype
    js.special.delete(prototype, "myProp")

    // Check it is actually gone.
    assertTrue(js.isUndefined((new Foo()).asInstanceOf[js.Dynamic].myProp))

    // Anonymous classes
    val y = new js.Object {
      def myProp: Int = 1
    }

    // The property should be on the instance itself.
    assertTrue(y.hasOwnProperty("myProp"))
    js.special.delete(y, "myProp")
    assertTrue(js.isUndefined(y.asInstanceOf[js.Dynamic].myProp))
    assertFalse(y.hasOwnProperty("myProp"))
  }

  @Test def propertiesWithExplicitName(): Unit = {
    class PropertiesWithExplicitName extends js.Object {
      private[this] var myY: String = "hello"
      @JSName("answer")
      val answerScala: Int = 42
      @JSName("x")
      var xScala: Int = 3
      @JSName("doubleX")
      def doubleXScala: Int = xScala * 2
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

  @Test def protectedProperties(): Unit = {
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

  @Test def simpleOverloadedMethods(): Unit = {
    class SimpleOverloadedMethods extends js.Object {
      def foo(): Int = 42
      def foo(x: Int): Int = x * 2
    }

    val foo = new SimpleOverloadedMethods
    assertEquals(42, foo.foo())
    assertEquals(6, foo.foo(3))

    val dyn = foo.asInstanceOf[js.Dynamic]
    assertEquals(js.typeOf(dyn.foo), "function")
    assertEquals(42, dyn.foo())
    assertEquals(6, dyn.foo(3))
  }

  @Test def simpleOverloadedMethodsAnonJSClass_Issue3054(): Unit = {
    trait SimpleOverloadedMethodsAnonJSClass extends js.Object {
      def foo(): Int
      def foo(x: Int): Int
    }

    val foo = new SimpleOverloadedMethodsAnonJSClass {
      def foo(): Int = 42
      def foo(x: Int): Int = x * 2
    }
    assertEquals(42, foo.foo())
    assertEquals(6, foo.foo(3))

    val dyn = foo.asInstanceOf[js.Dynamic]
    assertEquals(js.typeOf(dyn.foo), "function")
    assertEquals(42, dyn.foo())
    assertEquals(6, dyn.foo(3))
  }

  @Test def renamedOverloadedMethods(): Unit = {
    class RenamedOverloadedMethods extends js.Object {
      @JSName("foobar")
      def foo(): Int = 42
      @JSName("foobar")
      def bar(x: Int): Int = x * 2
    }

    val foo = new RenamedOverloadedMethods
    assertEquals(42, foo.foo())
    assertEquals(6, foo.bar(3))

    val dyn = foo.asInstanceOf[js.Dynamic]
    assertEquals(js.typeOf(dyn.foobar), "function")
    assertEquals(42, dyn.foobar())
    assertEquals(6, dyn.foobar(3))
  }

  @Test def overloadedMethodsWithVarargs(): Unit = {
    class OverloadedMethodsWithVarargs extends js.Object {
      def foo(x: Int): Int = x * 2
      def foo(strs: String*): Int = strs.foldLeft(0)(_ + _.length)
    }

    val foo = new OverloadedMethodsWithVarargs
    assertEquals(42, foo.foo(21))
    assertEquals(0, foo.foo())
    assertEquals(3, foo.foo("bar"))
    assertEquals(8, foo.foo("bar", "babar"))

    val dyn = foo.asInstanceOf[js.Dynamic]
    assertEquals(js.typeOf(dyn.foo), "function")
    assertEquals(42, dyn.foo(21))
    assertEquals(0, dyn.foo())
    assertEquals(3, dyn.foo("bar"))
    assertEquals(8, dyn.foo("bar", "babar"))
  }

  @Test def overloadedMethodsWithVarargsAnonJSClass_Issue3054(): Unit = {
    trait OverloadedMethodsWithVarargsAnonJSClass extends js.Object {
      def foo(x: Int): Int
      def foo(strs: String*): Int
    }

    val foo = new OverloadedMethodsWithVarargsAnonJSClass {
      def foo(x: Int): Int = x * 2
      def foo(strs: String*): Int = strs.foldLeft(0)(_ + _.length)
    }
    assertEquals(42, foo.foo(21))
    assertEquals(0, foo.foo())
    assertEquals(3, foo.foo("bar"))
    assertEquals(8, foo.foo("bar", "babar"))

    val dyn = foo.asInstanceOf[js.Dynamic]
    assertEquals(js.typeOf(dyn.foo), "function")
    assertEquals(42, dyn.foo(21))
    assertEquals(0, dyn.foo())
    assertEquals(3, dyn.foo("bar"))
    assertEquals(8, dyn.foo("bar", "babar"))
  }

  @Test def overloadedConstructorsNumParametersResolution(): Unit = {
    assertEquals(1, new OverloadedConstructorParamNumber(1).foo)
    assertEquals(3, new OverloadedConstructorParamNumber(1, 2).foo)
  }

  @Test def overloadedConstructorsParameterTypeResolution(): Unit = {
    assertEquals(1, new OverloadedConstructorParamType(1).foo)
    assertEquals(3, new OverloadedConstructorParamType("abc").foo)
  }

  @Test def overloadedConstructorsWithCapturedParameters(): Unit = {
    class OverloadedConstructorWithOuterContextOnly(val x: Int) extends js.Object {
      def this(y: String) = this(y.length)
    }

    val z = (() => 5)()
    class OverloadedConstructorWithValCapture(val x: Int) extends js.Object {
      def this(y: String) = this(z)
    }

    assertEquals(1, new OverloadedConstructorWithOuterContextOnly(1).x)
    assertEquals(3, new OverloadedConstructorWithOuterContextOnly("abc").x)

    assertEquals(1, new OverloadedConstructorWithValCapture(1).x)
    assertEquals(5, new OverloadedConstructorWithValCapture("abc").x)
  }

  @Test def overloadedConstructorsWithSuperClass(): Unit = {
    class OverloadedConstructorSup(val x: Int) extends js.Object {
      def this(y: String) = this(y.length)
    }
    class OverloadedConstructorSub(x: Int) extends OverloadedConstructorSup(3 * x) {
      def this(y: String) = this(2 * y.length)
    }
    assertEquals(1, new OverloadedConstructorSup(1).x)
    assertEquals(3, new OverloadedConstructorSup("abc").x)

    assertEquals(9, new OverloadedConstructorSub(3).x)
    assertEquals(12, new OverloadedConstructorSub("ab").x)
  }

  @Test def overloadedConstructorsWithRepeatedParameters(): Unit = {
    class OverloadedConstructorWithRepeatedParameters(xs: Int*) extends js.Object {
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

  @Test def overloadedConstructorsComplexResolution(): Unit = {
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

  @Test def secondaryConstructorUseDefaultParam(): Unit = {
    val a = new SecondaryConstructorUseDefaultParam(1)
    assertEquals(a.y, "1y")

    val b = new SecondaryConstructorUseDefaultParam()()
    assertEquals(b.y, "xy")
  }

  @Test def constructorsWithPatternMatch_Issue4581(): Unit = {
    val a = new PrimaryConstructorWithPatternMatch_Issue4581(5 :: Nil)
    assertEquals(5, a.head)

    val b = new SecondaryConstructorWithPatternMatch_Issue4581()
    assertEquals(0, b.head)

    val c = new SecondaryConstructorWithPatternMatch_Issue4581(6 :: Nil)
    assertEquals(6, c.head)
  }

  @Test def polytypeNullaryMethod_Issue2445(): Unit = {
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

  @Test def defaultParameters(): Unit = {
    class DefaultParameters extends js.Object {
      var sideEffectCounter: Int = 0

      def bar(x: Int, y: Int = 1): Int = x + y
      def dependent(x: Int)(y: Int = x + 1): Int = x + y
      def unitParam(x: Unit, y: Unit = { sideEffectCounter += 1; () }): Int = sideEffectCounter

      def foobar(x: Int): Int = bar(x)
    }

    object DefaultParametersMod extends js.Object {
      var sideEffectCounter: Int = 0

      def bar(x: Int, y: Int = 1): Int = x + y
      def dependent(x: Int)(y: Int = x + 1): Int = x + y
      def unitParam(x: Unit, y: Unit = { sideEffectCounter += 1; () }): Int = sideEffectCounter

      def foobar(x: Int): Int = bar(x)
    }

    val foo = new DefaultParameters
    assertEquals(9, foo.bar(4, 5))
    assertEquals(5, foo.bar(4))
    assertEquals(4, foo.foobar(3))
    assertEquals(9, foo.dependent(4)(5))
    assertEquals(17, foo.dependent(8)())

    // #4684 Default params with Unit type
    assertEquals(0, foo.sideEffectCounter)
    assertEquals(1, foo.unitParam(()))
    assertEquals(1, foo.sideEffectCounter)
    assertEquals(2, foo.unitParam((), ())) // an actual undefined param counts as not provided
    assertEquals(2, foo.sideEffectCounter)

    assertEquals(9, DefaultParametersMod.bar(4, 5))
    assertEquals(5, DefaultParametersMod.bar(4))
    assertEquals(4, DefaultParametersMod.foobar(3))
    assertEquals(9, DefaultParametersMod.dependent(4)(5))
    assertEquals(17, DefaultParametersMod.dependent(8)())

    // #4684 Default params with Unit type
    assertEquals(0, DefaultParametersMod.sideEffectCounter)
    assertEquals(1, DefaultParametersMod.unitParam(()))
    assertEquals(1, DefaultParametersMod.sideEffectCounter)
    assertEquals(2, DefaultParametersMod.unitParam((), ())) // an actual undefined param counts as not provided
    assertEquals(2, DefaultParametersMod.sideEffectCounter)

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

  @Test def overrideDefaultParameters(): Unit = {
    class OverrideDefaultParametersParent extends js.Object {
      def bar(x: Int, y: Int = 1): Int = x + y
      def dependent(x: Int)(y: Int = x + 1): Int = x + y

      def foobar(x: Int): Int = bar(x)
    }

    class OverrideDefaultParametersChild extends OverrideDefaultParametersParent {
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

  @Test def overrideMethodWithDefaultParametersWithoutNewDefault(): Unit = {
    class OverrideDefaultParametersWithoutDefaultParent extends js.Object {
      def bar(x: Int, y: Int = 1): Int = x + y
      def dependent(x: Int)(y: Int = x + 1): Int = x + y

      def foobar(x: Int): Int = bar(x)
    }

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

  @Test def constructorsWithDefaultParametersNonNativeNone(): Unit = {
    assertEquals(-1, new ConstructorDefaultParamJSNonNativeNone().foo)
    assertEquals(1, new ConstructorDefaultParamJSNonNativeNone(1).foo)
    assertEquals(5, new ConstructorDefaultParamJSNonNativeNone(5).foo)
  }

  @Test def constructorsWithDefaultParametersNonNativeNonNative(): Unit = {
    assertEquals(-1, new ConstructorDefaultParamJSNonNativeJSNonNative().foo)
    assertEquals(1, new ConstructorDefaultParamJSNonNativeJSNonNative(1).foo)
    assertEquals(5, new ConstructorDefaultParamJSNonNativeJSNonNative(5).foo)
  }

  @Test def constructorsWithDefaultParametersNonNativeScala(): Unit = {
    assertEquals(-1, new ConstructorDefaultParamJSNonNativeScala().foo)
    assertEquals(1, new ConstructorDefaultParamJSNonNativeScala(1).foo)
    assertEquals(5, new ConstructorDefaultParamJSNonNativeScala(5).foo)
  }

  @Test def constructorsWithDefaultParametersScalaNonNative(): Unit = {
    assertEquals(-1, new ConstructorDefaultParamScalaJSNonNative().foo)
    assertEquals(1, new ConstructorDefaultParamScalaJSNonNative(1).foo)
    assertEquals(5, new ConstructorDefaultParamScalaJSNonNative(5).foo)
  }

  @Test def constructorsWithDefaultParametersNativeNone(): Unit = {
    assertEquals(-1, new ConstructorDefaultParamJSNativeNone().foo)
    assertEquals(1, new ConstructorDefaultParamJSNativeNone(1).foo)
    assertEquals(5, new ConstructorDefaultParamJSNativeNone(5).foo)
  }

  @Test def constructorsWithDefaultParametersNativeScala(): Unit = {
    assertEquals(-1, new ConstructorDefaultParamJSNativeScala().foo)
    assertEquals(1, new ConstructorDefaultParamJSNativeScala(1).foo)
    assertEquals(5, new ConstructorDefaultParamJSNativeScala(5).foo)
  }

  @Test def constructorsWithDefaultParametersNativeNonNative(): Unit = {
    assertEquals(-1, new ConstructorDefaultParamJSNativeJSNonNative().foo)
    assertEquals(1, new ConstructorDefaultParamJSNativeJSNonNative(1).foo)
    assertEquals(5, new ConstructorDefaultParamJSNativeJSNonNative(5).foo)
  }

  @Test def constructorsWithDefaultParametersNativeNative(): Unit = {
    assertEquals(-1, new ConstructorDefaultParamJSNativeJSNative().foo)
    assertEquals(1, new ConstructorDefaultParamJSNativeJSNative(1).foo)
    assertEquals(5, new ConstructorDefaultParamJSNativeJSNative(5).foo)
  }

  @Test def constructorsWithDefaultParametersScalaScala(): Unit = {
    assertEquals(-1, new ConstructorDefaultParamScalaScala().foo)
    assertEquals(1, new ConstructorDefaultParamScalaScala(1).foo)
    assertEquals(5, new ConstructorDefaultParamScalaScala(5).foo)
  }

  @Test def constructorsWithDefaultParametersScalaNone(): Unit = {
    assertEquals(-1, new ConstructorDefaultParamScalaNone().foo)
    assertEquals(1, new ConstructorDefaultParamScalaNone(1).foo)
    assertEquals(5, new ConstructorDefaultParamScalaNone(5).foo)
  }

  @Test def constructorsWithDefaultParametersInMultiParamLists(): Unit = {
    val foo1 = new ConstructorDefaultParamMultiParamList(5)("foobar")
    assertEquals(5, foo1.default)
    assertEquals("foobar", foo1.title)
    assertEquals("5", foo1.description)

    val foo2 = new ConstructorDefaultParamMultiParamList(56)("babar", "desc")
    assertEquals(56, foo2.default)
    assertEquals("babar", foo2.title)
    assertEquals("desc", foo2.description)
  }

  @Test def constructorsWithDefaultParametersInMultiParamListsAndOverloading(): Unit = {
    val foo1 = new ConstructorDefaultParamMultiParamListWithOverloading(5)(
        "foobar")
    assertEquals(5, foo1.default)
    assertEquals("foobar", foo1.title)
    assertEquals("5", foo1.description)

    val foo2 = new ConstructorDefaultParamMultiParamListWithOverloading(56)(
        "babar", "desc")
    assertEquals(56, foo2.default)
    assertEquals("babar", foo2.title)
    assertEquals("desc", foo2.description)

    val foo3 = new ConstructorDefaultParamMultiParamListWithOverloading('A')
    assertEquals(65, foo3.default)
    assertEquals("char", foo3.title)
    assertEquals("a char", foo3.description)

    val foo4 = new ConstructorDefaultParamMultiParamListWithOverloading(123, 456)
    assertEquals(123, foo4.default)
    assertEquals("456", foo4.title)
    assertEquals(js.undefined, foo4.description)
  }

  @Test def callSuperConstructorWithDefaultParams_Issue4929(): Unit = {
    import ConstructorSuperCallWithDefaultParams._

    sideEffects.clear()

    val child = new Child(4, "hello", 23)
    assertEquals(4, child.foo)
    assertEquals(23, child.bar)

    assertEquals(
      List(
        "4",
        "Parent constructor; param1, 27, param1-27",
        "Child constructor; 4, hello, 23"
      ),
      sideEffects.toList
    )
  }

  @Test def callSuperConstructorWithColonAsterisk(): Unit = {
    class CallSuperCtorWithSpread(x: Int, y: Int, z: Int)
        extends NativeParentClassWithVarargs(x, Seq(y, z): _*)

    val foo = new CallSuperCtorWithSpread(4, 8, 23)
    assertEquals(4, foo.x)
    assertJSArrayEquals(js.Array(8, 23), foo.args)

    val dyn = foo.asInstanceOf[js.Dynamic]
    /* Dark magic is at play here: everywhere else in this compilation unit,
     * it's fine to do `assertEquals(4, dyn.x)` (for example, in the test
     * `override_native_method` below), but right here, it causes scalac to die
     * with a completely nonsensical compile error:
     *
     * > applyDynamic does not support passing a vararg parameter
     *
     * Extracting it in a separate `val` works around it.
     */
    val dynx = dyn.x
    assertEquals(4, dynx)
    val args = dyn.args.asInstanceOf[js.Array[Int]]
    assertJSArrayEquals(js.Array(8, 23), args)
  }

  @Test def overrideNativeMethod(): Unit = {
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

  @Test def overrideNonNativeMethod(): Unit = {
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

  @Test def overrideNonNativeMethodWithSeparateCompilation(): Unit = {
    val foo = new SepRun.SimpleChildClass
    assertEquals(6, foo.foo(3))

    val fooParent: SepRun.SimpleParentClass = foo
    assertEquals(6, fooParent.foo(3))

    val dyn = foo.asInstanceOf[js.Dynamic]
    assertEquals(6, foo.foo(3))
  }

  @Test def overrideNativeMethodAndCallSuper(): Unit = {
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

  @Test def overrideNonNativeMethodAndCallSuper(): Unit = {
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

  @Test def overloadSuperMethod_Issue4452(): Unit = {
    class Base extends js.Object {
      def f(x: Int, y: Int*): String = "Base " + y.length
      def g(x: Int, y: String): Unit = ()
    }

    class Sub extends Base {
      def f(x: String, y: Int*): String = "Sub " + y.length
      def g(x: Int): Unit = ()
    }

    val base = new Base
    val sub = new Sub

    assertEquals("Base 3", base.f(0, 1, 2, 3))
    assertEquals("Base 3", sub.f(0, 1, 2, 3))
    assertEquals("Sub 3", sub.f("0", 1, 2, 3))

    // Just check they don't throw.
    base.g(1, "0")
    sub.g(1, "0")
    sub.g(1)
  }

  @Test def superMethodCallInAnonJSClass_Issue3055(): Unit = {
    class Foo extends js.Object {
      def bar(msg: String): String = "super: " + msg
    }

    val foo = new Foo {
      override def bar(msg: String): String = super.bar("foo: " + msg)
    }

    assertEquals("super: foo: foobar", foo.bar("foobar"))
  }

  @Test def overrideNativeVal(): Unit = {
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

  @Test def overrideNonNativeVal(): Unit = {
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

  @Test def overrideNativeGetter(): Unit = {
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

  @Test def overrideNonNativeGetter(): Unit = {
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

  @Test def overrideNativeGetterWithVal(): Unit = {
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

  @Test def overrideNonNativeGetterWithVal(): Unit = {
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

  @Test def overrideGetterWithSuper(): Unit = {
    class OverrideGetterSuperParent extends js.Object {
      def bar: Int = 43
    }
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

  @Test def overrideSetterWithSuper(): Unit = {
    class OverrideSetterSuperParent extends js.Object {
      var x: Int = 43
      def bar_=(v: Int): Unit = x = v
    }
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

  @Test def superPropertyGetSetInAnonJSClass_Issue3055(): Unit = {
    class Foo extends js.Object {
      var x: Int = 1
      var lastSetValue: Int = 0

      def bar: Int = x
      def bar_=(v: Int): Unit = x = v
    }

    val foo = new Foo {
      override def bar: Int = super.bar * 2
      override def bar_=(v: Int): Unit = {
        lastSetValue = v
        super.bar = v + 3
      }
    }

    assertEquals(2, foo.bar)
    foo.bar = 6
    assertEquals(6, foo.lastSetValue)
    assertEquals(9, foo.x)
    assertEquals(18, foo.bar)
  }

  @Test def addOverloadInSubclass(): Unit = {
    class AddOverloadInSubclassParent extends js.Object {
      def bar(): Int = 53
    }
    class AddOverloadInSubclassChild extends AddOverloadInSubclassParent {
      def bar(x: Int): Int = x + 2
    }

    val foo = new AddOverloadInSubclassChild
    assertEquals(53, foo.bar())
    assertEquals(7, foo.bar(5))

    val dyn = foo.asInstanceOf[js.Dynamic]
    assertEquals(53, dyn.bar())
    assertEquals(7, dyn.bar(5))
  }

  @Test def addSetterInSubclass(): Unit = {
    class AddSetterInSubclassParent extends js.Object {
      var x: Int = 43
      def bar: Int = x
    }
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

  @Test def addGetterInSubclass(): Unit = {
    class AddGetterInSubclassParent extends js.Object {
      var x: Int = 43
      def bar_=(v: Int): Unit = x = v
    }
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

  @Test def overloadNativeMethod(): Unit = {
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

  @Test def overloadNonNativeMethod(): Unit = {
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

  @Test def overloadWithDefaultParameter(): Unit = {
    class OverloadDefaultParameter extends js.Object {
      def foo(x: Int): Int = x
      def foo(x: String = ""): String = x
    }

    val foo = new OverloadDefaultParameter
    assertEquals(5, foo.foo(5))
    assertEquals("", foo.foo())
    assertEquals("hello", foo.foo("hello"))
  }

  @Test def implementSimpleTrait(): Unit = {
    class ImplementSimpleTrait extends js.Object with SimpleTrait {
      def foo(x: Int): Int = x + 1
    }

    val foo = new ImplementSimpleTrait
    assertEquals(4, foo.foo(3))

    val fooTrait: SimpleTrait = foo
    assertEquals(6, fooTrait.foo(5))
  }

  @Test def implementSimpleTraitUnderSeparateCompilation(): Unit = {
    class ImplementSimpleTraitSepRun extends js.Object with SepRun.SimpleTrait {
      def foo(x: Int): Int = x + 1
    }

    val foo = new ImplementSimpleTraitSepRun
    assertEquals(4, foo.foo(3))

    val fooTrait: SepRun.SimpleTrait = foo
    assertEquals(6, fooTrait.foo(5))
  }

  @Test def implementTraitWithVal(): Unit = {
    trait TraitWithVal extends js.Object {
      val x: Int
    }

    class ImplWithVal extends TraitWithVal {
      val x: Int = 3
    }

    val foo = new ImplWithVal
    assertEquals(3, foo.x)

    val fooTrait: TraitWithVal = foo
    assertEquals(3, fooTrait.x)
  }

  @Test def implementTraitWithVar(): Unit = {
    trait TraitWithVar extends js.Object {
      var x: Int
    }

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

  @Test def implementTraitExtendingNativeJSClass(): Unit = {
    trait TraitExtendsJSClass extends NativeParentClass {
      def foobar(x: Int): Int
    }

    class ImplExtendsJSClassAndTrait extends NativeParentClass(5) with TraitExtendsJSClass {
      def foobar(x: Int): Int = x * 3
    }

    val foo = new ImplExtendsJSClassAndTrait
    assertEquals(18, foo.foobar(6))
  }

  @Test def implementAbstractMembersComingFromNativeJSClass(): Unit = {
    class ImplDeferredMembersFromJSParent extends NativeParentClassWithDeferred {
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

  @Test def overrideMethodWithDefaultValuesFromNativeJSClass(): Unit = {
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
  @Test def defaultValuesInNonExposedMethods(): Unit = {
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

  // #3939
  @Test def javaLangObjectMethodNames(): Unit = {
    class JavaLangObjectMethods extends js.Object {
      @JSName("clone")
      def myClone(): String = "myClone"

      @JSName("equals")
      def myEquals(): String = "myEquals"

      @JSName("finalize")
      def myFinalize(): String = "myFinalize"

      @JSName("hashCode")
      def myHashCode(): String = "myHashCode"

      @JSName("notify")
      def myNotify(): String = "myNotify"

      @JSName("notifyAll")
      def myNotifyAll(): String = "myNotifyAll"

      @JSName("wait")
      def myWait(): String = "myWait"
    }

    val x = (new JavaLangObjectMethods).asInstanceOf[js.Dynamic]

    assertEquals("myClone", x.applyDynamic("clone")())
  }
}

object NonNativeJSTypeTest {

  // Defined in test-suite/src/test/resources/NonNativeJSTypeTestNatives.js
  @JSGlobal("NonNativeJSTypeTestNativeParentClass")
  @js.native
  class NativeParentClass(val x: Int) extends js.Object {
    def foo(s: String): String = js.native

    def bar: Int = js.native

    def methodWithDefault(x: Int = 5): Int = js.native
  }

  class NonNativeParentClass(val x: Int) extends js.Object {
    def foo(s: String): String = s + x

    def bar: Int = x * 2
  }

  @js.native
  trait NativeTraitWithDeferred extends js.Object {
    val x: Int
  }

  // Defined in test-suite/src/test/resources/NonNativeJSTypeTestNatives.js
  @JSGlobal("NonNativeJSTypeTestNativeParentClassWithDeferred")
  @js.native
  abstract class NativeParentClassWithDeferred extends NativeTraitWithDeferred {
    def foo(y: Int): Int = js.native // = bar(y + 4) + x

    def bar(y: Int): Int
  }

  // Defined in test-suite/src/test/resources/NonNativeJSTypeTestNatives.js
  @JSGlobal("NonNativeJSTypeTestNativeParentClassWithVarargs")
  @js.native
  class NativeParentClassWithVarargs(
      _x: Int, _args: Int*)
      extends js.Object {
    val x: Int = js.native
    val args: js.Array[Int] = js.native
  }

  trait SimpleTrait extends js.Any {
    def foo(x: Int): Int
  }

  class Minimal extends js.Object

  private var staticNonNativeObjectInitCount: Int = _

  object StaticNonNativeObject extends js.Object {
    staticNonNativeObjectInitCount += 1
  }

  class SimpleMethod extends js.Object {
    def foo(x: Int): Int = x + 3
    def bar(s: String, i: Int): String = s + i
  }

  object StaticObjectSimpleMethod extends js.Object {
    def foo(x: Int): Int = x + 3
    def bar(s: String, i: Int): String = s + i
  }

  class SimpleField extends js.Object {
    val x = 5
    var y = 10

    def sum(): Int = x + y
  }

  object StaticObjectSimpleField extends js.Object {
    val x = 5
    var y = 10

    def sum(): Int = x + y
  }

  class SimpleAccessors extends js.Object {
    var x = 1
    def readPlus1: Int = x + 1

    def neg: Int = -x
    def neg_=(v: Int): Unit = x = -v
  }

  class SimpleConstructor(_x: Int, _y: Int) extends js.Object {
    val x = _x
    var y = _y

    def sum(): Int = x + y
  }

  class ConstructorDefaultParamJSNonNativeNone(val foo: Int = -1) extends js.Object

  class ConstructorDefaultParamJSNonNativeJSNonNative(val foo: Int = -1) extends js.Object
  object ConstructorDefaultParamJSNonNativeJSNonNative extends js.Object

  class ConstructorDefaultParamJSNonNativeScala(val foo: Int = -1) extends js.Object
  object ConstructorDefaultParamJSNonNativeScala

  class ConstructorDefaultParamScalaJSNonNative(val foo: Int = -1)
  object ConstructorDefaultParamScalaJSNonNative extends js.Object

  @js.native
  @JSGlobal("ConstructorDefaultParam")
  class ConstructorDefaultParamJSNativeNone(val foo: Int = -1) extends js.Object

  @js.native
  @JSGlobal("ConstructorDefaultParam")
  class ConstructorDefaultParamJSNativeScala(val foo: Int = -1) extends js.Object

  object ConstructorDefaultParamJSNativeScala

  @js.native
  @JSGlobal("ConstructorDefaultParam")
  class ConstructorDefaultParamJSNativeJSNonNative(val foo: Int = -1) extends js.Object

  object ConstructorDefaultParamJSNativeJSNonNative extends js.Object

  @js.native
  @JSGlobal("ConstructorDefaultParam")
  class ConstructorDefaultParamJSNativeJSNative(val foo: Int = -1) extends js.Object

  @js.native
  @JSGlobal("ConstructorDefaultParam")
  object ConstructorDefaultParamJSNativeJSNative extends js.Object

  // sanity check
  object ConstructorDefaultParamScalaScala
  class ConstructorDefaultParamScalaScala(val foo: Int = -1)

  // sanity check
  class ConstructorDefaultParamScalaNone(val foo: Int = -1)

  class ConstructorDefaultParamMultiParamList(val default: Int)(
      val title: String, val description: js.UndefOr[String] = default.toString)
      extends js.Object

  class ConstructorDefaultParamMultiParamListWithOverloading(val default: Int)(
      val title: String, val description: js.UndefOr[String] = default.toString)
      extends js.Object {
    def this(c: Char) = this(c.toInt)("char", "a char")

    def this(x: Int, y: Int) = this(x)(y.toString, js.undefined)
  }

  object ConstructorSuperCallWithDefaultParams {
    val sideEffects = mutable.ListBuffer.empty[String]

    class Parent(parentParam1: Any = "param1", parentParam2: Any = "param2")(
        dependentParam: String = s"$parentParam1-$parentParam2")
        extends js.Object {
      sideEffects += s"Parent constructor; $parentParam1, $parentParam2, $dependentParam"
    }

    class Child(val foo: Int, parentParam2: Any, val bar: Int)
        extends Parent(parentParam2 = { sideEffects += foo.toString(); foo + bar })() {
      sideEffects += s"Child constructor; $foo, $parentParam2, $bar"
    }
  }

  class OverloadedConstructorParamNumber(val foo: Int) extends js.Object {
    def this(x: Int, y: Int) = this(x + y)
    def this(x: Int, y: Int, z: Int) = this(x + y, z)
  }

  class OverloadedConstructorParamType(val foo: Int) extends js.Object {
    def this(x: String) = this(x.length)
    def this(x: Option[String]) = this(x.get)
  }

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

  class SecondaryConstructorUseDefaultParam(x: String = "x")(val y: String = x + "y")
      extends js.Object {
    def this(x: Int) = this(x.toString())()
  }

  class PrimaryConstructorWithPatternMatch_Issue4581(xs: List[Int]) extends js.Object {
    var head: Int = 0

    xs match {
      case x :: xr => head = x
      case _       => fail(xs.toString())
    }
  }

  class SecondaryConstructorWithPatternMatch_Issue4581 extends js.Object {
    var head: Int = 0

    def this(xs: List[Int]) = {
      this()

      xs match {
        case x :: xr => head = x
        case _       => fail(xs.toString())
      }
    }
  }

  class SimpleConstructorAutoFields(val x: Int, var y: Int) extends js.Object {
    def sum(): Int = x + y
  }

  class SimpleConstructorParamAccessors(x: Int, y: Int) extends js.Object {
    def sum(): Int = x + y
  }

  class ConstructorWithParamNameClashes(arg: Int, arg$1: Int, arg$2: Int,
      prep: Int, prep$1: Int, prep$2: Int)
      extends js.Object {
    val allArgs = List(arg, arg$1, arg$2, prep, prep$1, prep$2)
  }

  class MethodNamedConstructor(val x: Int) extends js.Object {
    def constructor(y: Int): Int = x + y
  }

  class SubclassOfMethodNamedConstructor(x: Int, val z: Int) extends MethodNamedConstructor(x) {
    def constructor(y: String): String = y + z
  }

  class SubclassOfMethodNamedConstructorNoRedefine(x: Int) extends MethodNamedConstructor(x)

  class DefaultFieldValues extends js.Object {
    var int: Int = _
    var bool: Boolean = _
    var char: Char = _
    var string: String = _
    var unit: Unit = _
  }

  trait LazyValFieldsSuperTrait extends js.Object {
    def initCount: Int
    def field: Int
  }

  class LazyValFields extends js.Object with LazyValFieldsSuperTrait {
    var initCount: Int = 0

    lazy val field: Int = {
      initCount += 1
      42
    }
  }

  class OverrideLazyValFields extends LazyValFields {
    override lazy val field: Int = {
      initCount += 1
      53
    }
  }

  class NullingOutLazyValFieldBug3422(initStr: String) extends js.Object {
    lazy val str: String = initStr
  }

  class SimpleInheritedFromNative(
      x: Int, val y: Int)
      extends NativeParentClass(x)

  object JSNameHolder {
    final val MethodName = "myMethod"
  }

  // #3998
  trait SelfTypeTest1 extends js.Object { self => }
  trait SelfTypeTest2 extends js.Object { self: js.Date => }
  trait SelfTypeTest3 extends js.Object { this: js.Date => }
}
