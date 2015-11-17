/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.library

import scala.scalajs.js
import scala.scalajs.js.annotation._

import org.scalajs.jasminetest.JasmineTest
import org.scalajs.testsuite.Typechecking._

object UseAsTest extends JasmineTest {

  describe("js.use(x).as[T] - Scala Types - success cases") {

    it("should support basic typechecking") {
      class A {
        @JSExport
        def m(a: Int, b: String): js.Object = ???
      }

      js.use(new A).as[JSBasic]
    }

    it("should support covariance in return types") {
      class A {
        @JSExport
        def m(a: Int, b: String): js.Array[Int] = ???
      }

      js.use(new A).as[JSBasic]
    }

    it("should support contravariance in argument") {
      class A {
        @JSExport
        def m(a: Int, b: Any): js.Object = ???
      }

      js.use(new A).as[JSBasic]
    }

    it("should support explicit names in JSExports") {
      class A {
        @JSExport("m")
        def f(a: Int, b: String): js.Object = ???
      }

      js.use(new A).as[JSBasic]
    }

    it("should support JSName") {
      class A {
        @JSExport
        def m(a: Int, b: String): js.Object = ???
      }

      class B {
        @JSExport("m")
        def bar(a: Int, b: String): js.Object = ???
      }

      js.use(new A).as[JSBasicJSName]
      js.use(new B).as[JSBasicJSName]
    }

    it("should support JSExportAll") {
      @JSExportAll
      class A {
        def m(a: Int, b: String): js.Object = ???
      }

      class B extends A

      js.use(new A).as[JSBasic]
      js.use(new B).as[JSBasic]
    }

    it("should support inherited exports") {
      abstract class A {
        @JSExport
        def m(a: Int, b: String): js.Object
      }

      class B extends A {
        def m(a: Int, b: String): js.Object = ???
      }

      js.use(new B).as[JSBasic]
    }

    it("should support JSExportAll on superclass") {
      @JSExportAll
      abstract class A {
        def m(a: Int, b: String): js.Object
      }

      class B extends A {
        def m(a: Int, b: String): js.Object = ???
      }

      js.use(new B).as[JSBasic]
    }

    it("should work with JSExportAll with an apply method") {
      @JSExportAll
      class A {
        @JSExport("bar")
        def apply(x: Int): Int = x * 2
      }

      val a = js.use(new A).as[JSNamedApply]

      expect(a(2)).toEqual(4)
      expect(a.bar(2)).toEqual(4)
    }

    it("should resolve generics in JSRaw types") {
      class A {
        @JSExport
        def arr: js.Array[Int] = ???
      }

      js.use(new A).as[JSGeneric[Int]]
      js.use(new A).as[JSGenericInt]
    }

    it("should resolve type members in JSRaw types") {
      class A {
        @JSExport
        def foo(x: Int): Int = ???
      }

      js.use(new A).as[JSTypeMember { type R = Int }]
    }

    it("should resolve exports with class-level type parameter") {
      class A[T] {
        @JSExport
        def arr: js.Array[T] = ???
      }

      class B extends A[Int]

      js.use(new A[Int]).as[JSGeneric[Int]]
      js.use(new B).as[JSGeneric[Int]]
    }

    it("should resolve exports with type member") {
      class A {
        type T

        @JSExport
        def arr: js.Array[T] = ???
      }

      class B extends A {
        type T = Int
      }

      js.use(new B).as[JSGeneric[Int]]
    }

    it("should resolve overloading") {
      @JSExportAll
      class A {
        def m(a: Int, b: String): js.Object = ???
        def m(b: String): Int = ???

        @JSExport("m")
        def strangeName(a: Int): js.Object = ???
      }

      js.use(new A).as[JSOverload]
    }

    it("should support vals/getters") {
      @JSExportAll
      class A {
        val a: Int = 1
        def b: String = ???
        // Test covariance as well
        def c: js.Array[Int] = ???
      }

      js.use(new A).as[JSGetters]
    }

    it("should support setters") {
      class A {
        @JSExport("a")
        def fooA_=(x: Int): Unit = ???

        @JSExport
        def b_=(x: String): Unit = ???

        @JSExport("c_=")
        def barC_=(x: js.Object): Unit = ???
      }

      js.use(new A).as[JSSetters]
    }

    it("should support vars") {
      class A {
        @JSExport
        def a: Int = ???
        @JSExport
        def a_=(x: Int): Unit = ???

        @JSExport("b")
        var fooB: String = _

        @JSExport
        var c: js.Object = _
      }

      js.use(new A).as[JSVars]
    }

    it("should support basic default arguments") {
      @JSExportAll
      class A {
        def sum4(a: Int, b: Int = 1, c: Int = 2, d: Int = 3): Int = a + b + c + d
        def sum2(a: Int, b: Int = 1): Int = a + b
      }

      js.use(new A).as[JSDefaultArgs]
    }

    it("should allow additional default arguments at the end of the params") {
      class A {
        @JSExport
        def m(a: Int, b: String, c: Int = ???, d: String = ???): js.Object = ???
      }

      js.use(new A).as[JSBasic]
    }

    it("should support repeated parameter lists") {
      @JSExportAll
      class A {
        def rep(a: Int, b: String*): Unit = ???
        def rep(a: Int*): Unit = ???
      }

      js.use(new A).as[JSRepeated]
    }

    it("should flatten multi parameter lists in raw JS type") {
      @JSExportAll
      class A {
        def multi(a: Int, b: String): Int = ???
      }

      js.use(new A).as[JSMulti]
    }

    it("should flatten multi parameter lists in exported method") {
      @JSExportAll
      class B {
        def m(a: Int)(b: String): js.Object = ???
      }

      js.use(new B).as[JSBasic]
    }

    it("should support anonymous types") {
      js.use(new { @JSExport def m(a: Int, b: String): js.Object = ??? }).as[JSBasic]
    }

    it("should allow Nothing") {
      if (false) {
        js.use(???).as[JSBasic]
      }
    }

    it("should allow Null") {
      js.use(null).as[JSBasic]
    }

  }

  describe("js.use(x).as[T] - Raw JS Types - success cases") {

    it("should support basic typechecking") {
      js.use(null: JSBasic).as[JSBasicJSName]
      js.use(null: JSBasicJSName).as[JSBasic]
    }

    it("should support generics") {
      js.use(null: JSGeneric[Int]).as[JSGenericInt]
      js.use(null: JSGenericInt).as[JSGeneric[Int]]
    }

    it("should support JS calls") {
      js.use(null: js.Function0[String]).as[JSApplyString]
    }

    it("should support @JSBracketAccess") {
      js.use(new js.Array[Int](0)).as[JSBracketAccessInt]
    }

    it("should support @JSBracketCall") {
      js.use(null: JSBracketCallInt1).as[JSBracketCallInt2]
    }

  }

  describe("js.use(x).as[T] - general failure cases") {

    it("fails with polymorphic methods") {
      typeErrorWithMsg(
          "js.use(new Object).as[JSPolyMethod]",
          "Polymorphic methods are currently not supported. Offending " +
          "method: org.scalajs.testsuite.library.UseAsTest.JSPolyMethod.poly")
    }

    it("fails with non-type refinements") {
      typeErrorWithMsg(
          "js.use(???).as[JSBasic { def foo: Int }]",
          "Refinement foo is not a type. Only types may be refined with as.")
    }

    it("fails with non trait") {
      typeErrorWithMsg(
          "js.use(???).as[js.Date]",
          "Only traits can be used with as")
    }

    it("fails with class parents") {
      typeErrorWithMsg(
          "js.use(???).as[JSNonClassParent]",
          "Supertype scala.scalajs.js.Date of trait JSNonClassParent is a " +
          "class. Cannot be used with as.")
    }

    it("fails gracefully with existential types - #1841") {
      typeErrorWithMsg(
          "js.use(null: JSTypeMember).as[JSTypeMember]",
          "Methods with existential types are not supported. Offending " +
          "method: org.scalajs.testsuite.library.UseAsTest.JSTypeMember.foo. " +
          "This is likely caused by an abstract type in the method signature")
    }

  }

  describe("js.use(x).as[T] - Scala Types - failure cases") {

    it("fails with apply in a raw JS type") {
      typeErrorWithMsg(
          "js.use(new Object).as[JSWithApply]",
          "org.scalajs.testsuite.library.UseAsTest.JSWithApply defines an apply " +
          "method. This cannot be implemented by any Scala exported type, " +
          "since it would need to chain Function's prototype.")
    }

    it("fails with @JSBracketAccess in a raw JS type") {
      typeErrorWithMsg(
          "js.use(new Object).as[JSWithBracketAccess]",
          "org.scalajs.testsuite.library.UseAsTest.JSWithBracketAccess " +
          "defines a @JSMemberBracketAccess method. Existence of such a " +
          "method cannot be statically checked for any Scala exported type.")
    }

    it("fails with @JSBracketCall in a raw JS type") {
      typeErrorWithMsg(
          "js.use(new Object).as[JSWithBracketCall]",
          "org.scalajs.testsuite.library.UseAsTest.JSWithBracketCall defines " +
          "a @JSMemberBracketCall method. Existence of such a method cannot " +
          "be statically checked for any Scala exported type.")
    }

    it("fails with a missing method") {
      class A {
        @JSExport
        def e(a: Int, b: String): js.Object = ???
      }

      typeErrorWithMsg(
          "js.use(new A).as[JSBasic]",
          "A does not export a method m(Int, String): scala.scalajs.js.Object.")
    }

    it("fails with a missing overload") {
      class A {
        @JSExport
        def m(a: Int, b: String): js.Object = ???
      }

      typeErrorWithMsg(
          "js.use(new A).as[JSOverload]",
          "A does not export a method m(Int): scala.scalajs.js.Object.")
    }

    it("fails with wrong argument types") {
      class A {
        @JSExport
        def m(a: String, b: Int): js.Object = ???
      }

      typeErrorWithMsg(
          "js.use(new A).as[JSBasic]",
          "A does not export a method m(Int, String): scala.scalajs.js.Object.")
    }

    it("fails with wrong return types") {
      class A {
        @JSExport
        def m(a: Int, b: String): Any = ???
      }

      typeErrorWithMsg(
          "js.use(new A).as[JSBasic]",
          "A does not export a method m(Int, String): scala.scalajs.js.Object.")
    }

    it("fails with a missing default argument") {
      @JSExportAll
      class A {
        def sum4(a: Int, b: Int = 1, c: Int = 2, d: Int = 3): Int = a + b + c + d
        def sum2(a: Int, b: Int): Int = a + b // should have default
      }

      typeErrorWithMsg(
          "js.use(new A).as[JSDefaultArgs]",
          "A does not export a method sum2(Int, Int = ???): Int.")
    }

    it("fails with a mismatching repeated argument") {
      @JSExportAll
      class A {
        def rep(a: Int, b: String): Unit = ??? // should be repeated
        def rep(a: Int*): Unit = ???
      }

      typeErrorWithMsg(
          "js.use(new A).as[JSRepeated]",
          "A does not export a method rep(Int, String*): Unit.")

      class B {
        @JSExport
        def m(a: Int, b: String*): js.Object = ??? // should not be repeated
      }

      typeErrorWithMsg(
          "js.use(new B).as[JSBasic]",
          "B does not export a method m(Int, String): scala.scalajs.js.Object.")
    }

  }

  describe("js.use(x).as[T] - Raw JS Types - failure cases") {

    it("fails with a missing apply") {
      typeErrorWithMsg(
          "js.use(new js.Object).as[JSWithApply]",
          "scala.scalajs.js.Object does not have a method " +
          "<apply>(String): Int. (type is not callable)")
    }

    it("fails with a missing @JSBracketAccess") {
      typeErrorWithMsg(
          "js.use(new js.Object).as[JSWithBracketAccess]",
          "scala.scalajs.js.Object does not have a method " +
          "<bracketaccess>(String): Int. (type doesn't support member " +
          "selection via []). Add @JSBracketAccess to use a method for " +
          "member selection.")
    }

    it("fails with a missing @JSBracketCall") {
      typeErrorWithMsg(
          "js.use(new js.Object).as[JSWithBracketCall]",
          "scala.scalajs.js.Object does not have a method " +
          "<bracketcall>(String, String): Int. (type doesn't support " +
          "dynamically calling methods). Add @JSBracketCall to use a method " +
          "for dynamic calls.")
    }

    it("fails with a missing method") {
      typeErrorWithMsg(
          "js.use(new js.Object).as[JSBasic]",
          "scala.scalajs.js.Object does not have a method " +
          "m(Int, String): scala.scalajs.js.Object.")
    }

    it("fails with a missing overload") {
      typeErrorWithMsg(
          "js.use(null: JSBasic).as[JSOverload]",
          "org.scalajs.testsuite.library.UseAsTest.JSBasic does not have a " +
          "method m(Int): scala.scalajs.js.Object.")
    }

    it("fails with wrongly typed generic") {
      typeErrorWithMsg(
          "js.use(null: JSGeneric[Int]).as[JSGeneric[String]]",
          "org.scalajs.testsuite.library.UseAsTest.JSGeneric[Int] does not " +
          "have a getter arr: scala.scalajs.js.Array[String].")
    }

  }

  @js.native
  trait JSBasic extends js.Object {
    def m(a: Int, b: String): js.Object = js.native
  }

  @js.native
  trait JSBasicJSName extends js.Object {
    @JSName("m")
    def foo(a: Int, b: String): js.Object = js.native
  }

  @js.native
  trait JSNamedApply extends js.Object {
    @JSName("apply")
    def apply(x: Int): Int = js.native

    def bar(x: Int): Int = js.native
  }

  @js.native
  trait JSGeneric[T] extends js.Object {
    def arr: js.Array[T] = js.native
  }

  @js.native
  trait JSGenericInt extends JSGeneric[Int]

  @js.native
  trait JSTypeMember extends js.Object {
    type R
    def foo(x: R): Int = js.native
  }

  @js.native
  trait JSOverload extends JSBasic {
    def m(b: String): Int = js.native
    def m(a: Int): js.Object = js.native
  }

  @js.native
  trait JSGetters extends js.Object {
    def a: Int = js.native
    val b: String = js.native
    def c: js.Object = js.native
  }

  @js.native
  trait JSSetters extends js.Object {
    def a_=(x: Int): Unit = js.native

    @JSName("b")
    def fooJS_=(x: String): Unit = js.native

    @JSName("c_=")
    def barJS_=(x: js.Array[Int]): Unit = js.native
  }

  @js.native
  trait JSVars extends js.Object {
    var a: Int = js.native
    def b: String = js.native
    def b_=(x: String): Unit = js.native

    @JSName("c")
    var fooJS: js.Object = js.native
  }

  @js.native
  trait JSDefaultArgs extends js.Object {
    def sum4(a: Int, b: Int = ???, c: Int = ???, d: Int = ???): Int = js.native
    def sum2(a: Int, b: Int = ???): Int = js.native
  }

  @js.native
  trait JSRepeated extends js.Object {
    def rep(a: Int, b: String*): Unit = js.native
    def rep(a: Int*): Unit = js.native
  }

  @js.native
  trait JSMulti extends js.Object {
    def multi(a: Int)(b: String): Int = js.native
  }

  @js.native
  trait JSPolyMethod extends js.Object {
    def poly[T](a: T): js.Array[T] = js.native
  }

  @js.native
  trait JSWithApply extends js.Object {
    def apply(a: String): Int = js.native
  }

  @js.native
  trait JSWithBracketAccess extends js.Object {
    @JSBracketAccess
    def foo(a: String): Int = js.native
  }

  @js.native
  trait JSWithBracketCall extends js.Object {
    @JSBracketCall
    def foo(name: String, b: String): Int = js.native
  }

  @js.native
  trait JSNonClassParent extends js.Date

  @js.native
  trait JSApplyString extends js.Object {
    def apply(): String = js.native
  }

  @js.native
  trait JSBracketAccessInt extends js.Object {
    @JSBracketAccess
    def apply(x: Int): Int = js.native
  }

  @js.native
  trait JSBracketCallInt1 extends js.Object {
    @JSBracketCall
    def foo(method: String): Int = js.native
  }

  @js.native
  trait JSBracketCallInt2 extends js.Object {
    @JSBracketCall
    def bar(method: String): Int = js.native
  }
}
