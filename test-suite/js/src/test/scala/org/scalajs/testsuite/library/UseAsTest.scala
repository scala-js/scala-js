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

import org.scalajs.testsuite.Typechecking._

import org.junit.Assert._
import org.junit.Test

class UseAsScalaTypesTest {
  import UseAsTest._

  // js.use(x).as[T] - Scala Types - success cases

  @Test def should_support_basic_typechecking(): Unit = {
    class A {
      @JSExport
      def m(a: Int, b: String): js.Object = ???
    }

    js.use(new A).as[JSBasic]
  }

  @Test def should_support_covariance_in_return_types(): Unit = {
    class A {
      @JSExport
      def m(a: Int, b: String): js.Array[Int] = ???
    }

    js.use(new A).as[JSBasic]
  }

  @Test def should_support_contravariance_in_argument(): Unit = {
    class A {
      @JSExport
      def m(a: Int, b: Any): js.Object = ???
    }

    js.use(new A).as[JSBasic]
  }

  @Test def should_support_explicit_names_in_JSExports(): Unit = {
    class A {
      @JSExport("m")
      def f(a: Int, b: String): js.Object = ???
    }

    js.use(new A).as[JSBasic]
  }

  @Test def should_support_JSName(): Unit = {
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

  @Test def should_support_JSExportAll(): Unit = {
    @JSExportAll
    class A {
      def m(a: Int, b: String): js.Object = ???
    }

    class B extends A

    js.use(new A).as[JSBasic]
    js.use(new B).as[JSBasic]
  }

  @Test def should_support_inherited_exports(): Unit = {
    abstract class A {
      @JSExport
      def m(a: Int, b: String): js.Object
    }

    class B extends A {
      def m(a: Int, b: String): js.Object = ???
    }

    js.use(new B).as[JSBasic]
  }

  @Test def should_support_JSExportAll_on_superclass(): Unit = {
    @JSExportAll
    abstract class A {
      def m(a: Int, b: String): js.Object
    }

    class B extends A {
      def m(a: Int, b: String): js.Object = ???
    }

    js.use(new B).as[JSBasic]
  }

  @Test def should_work_with_JSExportAll_with_an_apply_method(): Unit = {
    @JSExportAll
    class A {
      @JSExport("bar")
      def apply(x: Int): Int = x * 2
    }

    val a = js.use(new A).as[JSNamedApply]

    assertEquals(4, a(2))
    assertEquals(4, a.bar(2))
  }

  @Test def should_resolve_generics_in_JSRaw_types(): Unit = {
    class A {
      @JSExport
      def arr: js.Array[Int] = ???
    }

    js.use(new A).as[JSGeneric[Int]]
    js.use(new A).as[JSGenericInt]
  }

  @Test def should_resolve_type_members_in_JSRaw_types(): Unit = {
    class A {
      @JSExport
      def foo(x: Int): Int = ???
    }

    js.use(new A).as[JSTypeMember { type R = Int }]
  }

  @Test def should_resolve_exports_with_class_level_type_parameter(): Unit = {
    class A[T] {
      @JSExport
      def arr: js.Array[T] = ???
    }

    class B extends A[Int]

    js.use(new A[Int]).as[JSGeneric[Int]]
    js.use(new B).as[JSGeneric[Int]]
  }

  @Test def should_resolve_exports_with_type_member(): Unit = {
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

  @Test def should_resolve_overloading(): Unit = {
    @JSExportAll
    class A {
      def m(a: Int, b: String): js.Object = ???
      def m(b: String): Int = ???

      @JSExport("m")
      def strangeName(a: Int): js.Object = ???
    }

    js.use(new A).as[JSOverload]
  }

  @Test def should_support_vals_getters(): Unit = {
    @JSExportAll
    class A {
      val a: Int = 1
      def b: String = ???
      // Test covariance as well
      def c: js.Array[Int] = ???
    }

    js.use(new A).as[JSGetters]
  }

  @Test def should_support_setters(): Unit = {
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

  @Test def should_support_vars(): Unit = {
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

  @Test def should_support_abstract_members_class(): Unit = {
    abstract class AbstractFieldsClass {
      @JSExport
      def a: Int
      @JSExport
      def a_=(x: Int): Unit

      @JSExport("b")
      var fooB: String

      @JSExport
      var c: js.Object
    }

    js.use(null: AbstractFieldsClass).as[JSVars]
  }

  @Test def should_support_abstract_members_trait(): Unit = {
    trait AbstractFieldsTrait {
      @JSExport
      def a: Int
      @JSExport
      def a_=(x: Int): Unit

      @JSExport("b")
      var fooB: String

      @JSExport
      var c: js.Object
    }

    js.use(null: AbstractFieldsTrait).as[JSVars]
  }

  @Test def should_support_basic_default_arguments(): Unit = {
    @JSExportAll
    class A {
      def sum4(a: Int, b: Int = 1, c: Int = 2, d: Int = 3): Int = a + b + c + d
      def sum2(a: Int, b: Int = 1): Int = a + b
    }

    js.use(new A).as[JSDefaultArgs]
  }

  @Test def should_allow_additional_default_arguments_at_the_end_of_the_params(): Unit = {
    class A {
      @JSExport
      def m(a: Int, b: String, c: Int = ???, d: String = ???): js.Object = ???
    }

    js.use(new A).as[JSBasic]
  }

  @Test def should_support_repeated_parameter_lists(): Unit = {
    @JSExportAll
    class A {
      def rep(a: Int, b: String*): Unit = ???
      def rep(a: Int*): Unit = ???
    }

    js.use(new A).as[JSRepeated]
  }

  @Test def should_flatten_multi_parameter_lists_in_raw_JS_type(): Unit = {
    @JSExportAll
    class A {
      def multi(a: Int, b: String): Int = ???
    }

    js.use(new A).as[JSMulti]
  }

  @Test def should_flatten_multi_parameter_lists_in_exported_method(): Unit = {
    @JSExportAll
    class B {
      def m(a: Int)(b: String): js.Object = ???
    }

    js.use(new B).as[JSBasic]
  }

  @Test def should_support_anonymous_types(): Unit = {
    js.use(new { @JSExport def m(a: Int, b: String): js.Object = ??? }).as[JSBasic]
  }

  @Test def should_allow_Nothing(): Unit = {
    if (false) {
      js.use(???).as[JSBasic]
    }
  }

  @Test def should_allow_Null(): Unit = {
    js.use(null).as[JSBasic]
  }

  // js.use(x).as[T] - Raw JS Types - success cases

  @Test def should_support_basic_typechecking_raw_js(): Unit = {
    js.use(null: JSBasic).as[JSBasicJSName]
    js.use(null: JSBasicJSName).as[JSBasic]
  }

  @Test def should_support_generics(): Unit = {
    js.use(null: JSGeneric[Int]).as[JSGenericInt]
    js.use(null: JSGenericInt).as[JSGeneric[Int]]
  }

  @Test def should_support_JS_calls(): Unit = {
    js.use(null: js.Function0[String]).as[JSApplyString]
  }

  @Test def should_support_atJSBracketAccess(): Unit = {
    js.use(new js.Array[Int](0)).as[JSBracketAccessInt]
  }

  @Test def should_support_atJSBracketCall(): Unit = {
    js.use(null: JSBracketCallInt1).as[JSBracketCallInt2]
  }

  // js.use(x).as[T] - general failure cases

  @Test def fails_with_polymorphic_methods(): Unit = {
    typeErrorWithMsg(
        "js.use(new Object).as[JSPolyMethod]",
        "Polymorphic methods are currently not supported. Offending " +
        "method: org.scalajs.testsuite.library.UseAsTest.JSPolyMethod.poly")
  }

  @Test def fails_with_non_type_refinements(): Unit = {
    typeErrorWithMsg(
        "js.use(???).as[JSBasic { def foo: Int }]",
        "Refinement foo is not a type. Only types may be refined with as.")
  }

  @Test def fails_with_non_trait(): Unit = {
    typeErrorWithMsg(
        "js.use(???).as[js.Date]",
        "Only traits can be used with as")
  }

  @Test def fails_with_class_parents(): Unit = {
    typeErrorWithMsg(
        "js.use(???).as[JSNonClassParent]",
        "Supertype scala.scalajs.js.Date of trait JSNonClassParent is a " +
        "class. Cannot be used with as.")
  }

  @Test def fails_gracefully_with_existential_types_issue_1841(): Unit = {
    typeErrorWithMsg(
        "js.use(null: JSTypeMember).as[JSTypeMember]",
        "Methods with existential types are not supported. Offending " +
        "method: org.scalajs.testsuite.library.UseAsTest.JSTypeMember.foo. " +
        "This is likely caused by an abstract type in the method signature")
  }

  // js.use(x).as[T] - Scala Types - failure cases

  @Test def fails_with_apply_in_a_raw_JS_type(): Unit = {
    typeErrorWithMsg(
        "js.use(new Object).as[JSWithApply]",
        "org.scalajs.testsuite.library.UseAsTest.JSWithApply defines an apply " +
        "method. This cannot be implemented by any Scala exported type, " +
        "since it would need to chain Function's prototype.")
  }

  @Test def fails_with_atJSBracketAccess_in_a_raw_JS_type(): Unit = {
    typeErrorWithMsg(
        "js.use(new Object).as[JSWithBracketAccess]",
        "org.scalajs.testsuite.library.UseAsTest.JSWithBracketAccess " +
        "defines a @JSMemberBracketAccess method. Existence of such a " +
        "method cannot be statically checked for any Scala exported type.")
  }

  @Test def fails_with_atJSBracketCall_in_a_raw_JS_type(): Unit = {
    typeErrorWithMsg(
        "js.use(new Object).as[JSWithBracketCall]",
        "org.scalajs.testsuite.library.UseAsTest.JSWithBracketCall defines " +
        "a @JSMemberBracketCall method. Existence of such a method cannot " +
        "be statically checked for any Scala exported type.")
  }

  @Test def fails_with_a_missing_method_failure(): Unit = {
    class A {
      @JSExport
      def e(a: Int, b: String): js.Object = ???
    }

    typeErrorWithMsg(
        "js.use(new A).as[JSBasic]",
        "A does not export a method m(Int, String): scala.scalajs.js.Object.")
  }

  @Test def fails_with_a_missing_overload_failure(): Unit = {
    class A {
      @JSExport
      def m(a: Int, b: String): js.Object = ???
    }

    typeErrorWithMsg(
        "js.use(new A).as[JSOverload]",
        "A does not export a method m(Int): scala.scalajs.js.Object.")
  }

  @Test def fails_with_wrong_argument_types(): Unit = {
    class A {
      @JSExport
      def m(a: String, b: Int): js.Object = ???
    }

    typeErrorWithMsg(
        "js.use(new A).as[JSBasic]",
        "A does not export a method m(Int, String): scala.scalajs.js.Object.")
  }

  @Test def fails_with_wrong_return_types(): Unit = {
    class A {
      @JSExport
      def m(a: Int, b: String): Any = ???
    }

    typeErrorWithMsg(
        "js.use(new A).as[JSBasic]",
        "A does not export a method m(Int, String): scala.scalajs.js.Object.")
  }

  @Test def fails_with_a_missing_default_argument(): Unit = {
    @JSExportAll
    class A {
      def sum4(a: Int, b: Int = 1, c: Int = 2, d: Int = 3): Int = a + b + c + d
      def sum2(a: Int, b: Int): Int = a + b // should have default
    }

    typeErrorWithMsg(
        "js.use(new A).as[JSDefaultArgs]",
        "A does not export a method sum2(Int, Int = ???): Int.")
  }

  @Test def fails_with_a_mismatching_repeated_argument(): Unit = {
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

  // js.use(x).as[T] - Raw JS Types - failure cases

  @Test def fails_with_a_missing_apply(): Unit = {
    typeErrorWithMsg(
        "js.use(new js.Object).as[JSWithApply]",
        "scala.scalajs.js.Object does not have a method " +
        "<apply>(String): Int. (type is not callable)")
  }

  @Test def fails_with_a_missing_atJSBracketAccess(): Unit = {
    typeErrorWithMsg(
        "js.use(new js.Object).as[JSWithBracketAccess]",
        "scala.scalajs.js.Object does not have a method " +
        "<bracketaccess>(String): Int. (type doesn't support member " +
        "selection via []). Add @JSBracketAccess to use a method for " +
        "member selection.")
  }

  @Test def fails_with_a_missing_atJSBracketCall(): Unit = {
    typeErrorWithMsg(
        "js.use(new js.Object).as[JSWithBracketCall]",
        "scala.scalajs.js.Object does not have a method " +
        "<bracketcall>(String, String): Int. (type doesn't support " +
        "dynamically calling methods). Add @JSBracketCall to use a method " +
        "for dynamic calls.")
  }

  @Test def fails_with_a_missing_method(): Unit = {
    typeErrorWithMsg(
        "js.use(new js.Object).as[JSBasic]",
        "scala.scalajs.js.Object does not have a method " +
        "m(Int, String): scala.scalajs.js.Object.")
  }

  @Test def fails_with_a_missing_overload(): Unit = {
    typeErrorWithMsg(
        "js.use(null: JSBasic).as[JSOverload]",
        "org.scalajs.testsuite.library.UseAsTest.JSBasic does not have a " +
        "method m(Int): scala.scalajs.js.Object.")
  }

  @Test def fails_with_wrongly_typed_generic(): Unit = {
    typeErrorWithMsg(
        "js.use(null: JSGeneric[Int]).as[JSGeneric[String]]",
        "org.scalajs.testsuite.library.UseAsTest.JSGeneric[Int] does not " +
        "have a getter arr: scala.scalajs.js.Array[String].")
  }

}

object UseAsTest {

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
