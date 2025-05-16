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

package org.scalajs.nscplugin.test

import org.scalajs.nscplugin.test.util._
import org.scalajs.nscplugin.test.util.VersionDependentUtils.methodSig

import org.junit.Assume._
import org.junit.Test

// scalastyle:off line.size.limit

class JSExportTest extends DirectTest with TestHelpers {

  override def extraArgs: List[String] =
    super.extraArgs ::: List("-deprecation")

  override def preamble: String =
    """import scala.scalajs.js, js.annotation._
    """

  @Test
  def warnOnDuplicateExport(): Unit = {
    """
    class A {
      @JSExport
      @JSExport
      def a = 1
    }
    """ hasWarns
    """
      |newSource1.scala:6: warning: Found duplicate @JSExport
      |      def a = 1
      |          ^
    """

    """
    class A {
      @JSExport
      @JSExport("a")
      def a = 1
    }
    """ hasWarns
    """
      |newSource1.scala:6: warning: Found duplicate @JSExport
      |      def a = 1
      |          ^
    """

    """
    class A {
      @JSExport("a")
      @JSExport("a")
      def a = 1
    }
    """ hasWarns
    """
      |newSource1.scala:6: warning: Found duplicate @JSExport
      |      def a = 1
      |          ^
    """

    // special case for @JSExportAll and 2 or more @JSExport("apply")
    // since @JSExportAll and single @JSExport("apply") should not be warned (see other tests)
    """
    @JSExportAll
    class A {
      @JSExport("apply")
      @JSExport("apply")
      def apply(): Int = 1
    }
    """ hasWarns
    """
      |newSource1.scala:7: warning: Found duplicate @JSExport
      |      def apply(): Int = 1
      |          ^
    """

    """
    @JSExportAll
    class A {
      @JSExport
      def a = 1
    }
    """ hasWarns
    """
      |newSource1.scala:6: warning: Found duplicate @JSExport
      |      def a = 1
      |          ^
    """
  }

  @Test
  def noWarnOnUniqueExplicitName(): Unit = {
    """
    class A {
      @JSExport("a")
      @JSExport("b")
      def c = 1
    }
    """.hasNoWarns()
  }

  @Test
  def noJSExportClass(): Unit = {
    """
    @JSExport
    class A

    @JSExport("Foo")
    class B
    """ hasErrors
    """
      |newSource1.scala:3: error: @JSExport is forbidden on top-level objects and classes. Use @JSExportTopLevel instead.
      |    @JSExport
      |     ^
      |newSource1.scala:6: error: @JSExport is forbidden on top-level objects and classes. Use @JSExportTopLevel instead.
      |    @JSExport("Foo")
      |     ^
    """
  }

  @Test
  def noJSExportObject(): Unit = {
    """
    @JSExport
    object A

    @JSExport("Foo")
    object B
    """ hasErrors
    """
      |newSource1.scala:3: error: @JSExport is forbidden on top-level objects and classes. Use @JSExportTopLevel instead.
      |    @JSExport
      |     ^
      |newSource1.scala:6: error: @JSExport is forbidden on top-level objects and classes. Use @JSExportTopLevel instead.
      |    @JSExport("Foo")
      |     ^
    """
  }

  @Test
  def noDoubleUnderscoreExport(): Unit = {
    """
    class A {
      @JSExport(name = "__")
      def foo = 1

      @JSExport
      def bar__(x: Int) = x
    }
    """ hasErrors
    """
      |newSource1.scala:4: error: An exported name may not contain a double underscore (`__`)
      |      @JSExport(name = "__")
      |                       ^
      |newSource1.scala:8: error: An exported name may not contain a double underscore (`__`)
      |      def bar__(x: Int) = x
      |          ^
    """
  }

  @Test
  def doubleUnderscoreOKInTopLevelExport(): Unit = {
    """
    @JSExportTopLevel("__A")
    class A

    @JSExportTopLevel("__B")
    object B

    object Container {
      @JSExportTopLevel("__c")
      def c(): Int = 4

      @JSExportTopLevel("__d")
      val d: Boolean = true
    }
    """.hasNoWarns()
  }

  @Test
  def noConflictingExport(): Unit = {
    """
    class Confl {
      @JSExport("value")
      def hello = "foo"

      @JSExport("value")
      def world = "bar"
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: double definition:
      |def $js$exported$prop$value: Any at line 4 and
      |def $js$exported$prop$value: Any at line 7
      |have same type
      |      @JSExport("value")
      |       ^
    """

    """
    class Confl {
      class Box[T](val x: T)

      @JSExport
      def ub(x: Box[String]): String = x.x
      @JSExport
      def ub(x: Box[Int]): Int = x.x
    }
    """ hasErrors
    s"""
      |newSource1.scala:8: error: double definition:
      |def ${"$js$exported$meth$ub"}(x: Confl.this.Box[String]): Any at line 6 and
      |def ${"$js$exported$meth$ub"}(x: Confl.this.Box[Int]): Any at line 8
      |have same type after erasure: ${methodSig("(x: Confl#Box)", "Object")}
      |      @JSExport
      |       ^
    """

    """
    class Confl {
      @JSExport
      def rtType(x: js.Any) = x

      @JSExport
      def rtType(x: js.Dynamic) = x
    }
    """ hasErrors
    s"""
      |newSource1.scala:7: error: Cannot disambiguate overloads for exported method rtType with types
      |  ${methodSig("(x: scala.scalajs.js.Any)", "Object")}
      |  ${methodSig("(x: scala.scalajs.js.Dynamic)", "Object")}
      |      @JSExport
      |       ^
    """

    """
    class Confl {
      @JSExport
      def foo(x: Int)(ys: Int*) = x

      @JSExport
      def foo(x: Int*) = x
    }
    """ hasErrors
    s"""
      |newSource1.scala:7: error: Cannot disambiguate overloads for exported method foo with types
      |  ${methodSig("(x: Int, ys: Seq)", "Object")}
      |  ${methodSig("(x: Seq)", "Object")}
      |      @JSExport
      |       ^
    """

    """
    class Confl {
      @JSExport
      def foo(x: Int = 1) = x
      @JSExport
      def foo(x: String*) = x
    }
    """ hasErrors
    s"""
      |newSource1.scala:6: error: Cannot disambiguate overloads for exported method foo with types
      |  ${methodSig("(x: Int)", "Object")}
      |  ${methodSig("(x: Seq)", "Object")}
      |      @JSExport
      |       ^
    """

    """
    class Confl {
      @JSExport
      def foo(x: Double, y: String)(z: Int = 1) = x
      @JSExport
      def foo(x: Double, y: String)(z: String*) = x
    }
    """ hasErrors
    s"""
      |newSource1.scala:6: error: Cannot disambiguate overloads for exported method foo with types
      |  ${methodSig("(x: Double, y: String, z: Int)", "Object")}
      |  ${methodSig("(x: Double, y: String, z: Seq)", "Object")}
      |      @JSExport
      |       ^
    """

    """
    class A {
      @JSExport
      def a(x: scala.scalajs.js.Any) = 1

      @JSExport
      def a(x: Any) = 2
    }
    """ hasErrors
    s"""
      |newSource1.scala:7: error: Cannot disambiguate overloads for exported method a with types
      |  ${methodSig("(x: Object)", "Object")}
      |  ${methodSig("(x: scala.scalajs.js.Any)", "Object")}
      |      @JSExport
      |       ^
    """

  }

  @Test
  def noExportLocal(): Unit = {
    // Local class
    """
    class A {
      def method = {
        @JSExport
        class A

        @JSExport
        class B extends js.Object
      }
    }
    """ hasErrors
    """
      |newSource1.scala:5: error: You may not export constructors of local classes
      |        @JSExport
      |         ^
      |newSource1.scala:8: error: You may not export a local definition
      |        @JSExport
      |         ^
    """

    // Local object
    """
    class A {
      def method = {
        @JSExport
        object A

        @JSExport
        object B extends js.Object
      }
    }
    """ hasErrors
    """
      |newSource1.scala:5: error: You may not export a local definition
      |        @JSExport
      |         ^
      |newSource1.scala:8: error: You may not export a local definition
      |        @JSExport
      |         ^
    """

    // Local method
    """
    class A {
      def method = {
        @JSExport
        def foo = 1
      }
    }
    """ hasErrors
    """
      |newSource1.scala:5: error: You may not export a local definition
      |        @JSExport
      |         ^
    """

    // Local val
    """
    class A {
      def method = {
        @JSExport
        val x = 1
      }
    }
    """ hasErrors
    """
      |newSource1.scala:5: error: You may not export a local definition
      |        @JSExport
      |         ^
    """

    // Local var
    """
    class A {
      def method = {
        @JSExport
        var x = 1
      }
    }
    """ hasErrors
    """
      |newSource1.scala:5: error: You may not export a local definition
      |        @JSExport
      |         ^
    """

  }

  @Test
  def noMiddleVarArg(): Unit = {

    """
    class A {
      @JSExport
      def method(xs: Int*)(ys: String) = 1
    }
    """ hasErrors
    """
      |newSource1.scala:4: error: In an exported method or constructor, a *-parameter must come last (through all parameter lists)
      |      @JSExport
      |       ^
    """

  }

  @Test
  def noMiddleDefaultParam(): Unit = {

    """
    class A {
      @JSExport
      def method(x: Int = 1)(y: String) = 1
    }
    """ hasErrors
    """
      |newSource1.scala:4: error: In an exported method or constructor, all parameters with defaults must be at the end
      |      @JSExport
      |       ^
    """

  }

  @Test
  def noExportAbstractClass(): Unit = {

    """
    object Foo {
      @JSExport
      abstract class A

      abstract class B(x: Int) {
        @JSExport
        def this() = this(5)
      }

      @JSExport // ok!
      abstract class C extends js.Object
    }
    """ hasErrors
    """
      |newSource1.scala:4: error: You may not export an abstract class
      |      @JSExport
      |       ^
      |newSource1.scala:8: error: You may not export an abstract class
      |        @JSExport
      |         ^
    """

    """
    @JSExportTopLevel("A")
    abstract class A

    abstract class B(x: Int) {
      @JSExportTopLevel("B")
      def this() = this(5)
    }
    """ hasErrors
    """
      |newSource1.scala:3: error: You may not export an abstract class
      |    @JSExportTopLevel("A")
      |     ^
      |newSource1.scala:7: error: You may not export an abstract class
      |      @JSExportTopLevel("B")
      |       ^
    """

  }

  @Test
  def noJSExportOnTrait(): Unit = {

    """
    @JSExport
    trait Test

    @JSExport
    trait Test2 extends js.Object

    @JSExport
    @js.native
    trait Test3 extends js.Object
    """ hasErrors
    """
      |newSource1.scala:3: error: You may not export a trait
      |    @JSExport
      |     ^
      |newSource1.scala:6: error: You may not export a trait
      |    @JSExport
      |     ^
      |newSource1.scala:9: error: You may not export a trait
      |    @JSExport
      |     ^
    """

    """
    object A {
      @JSExport
      trait Test

      @JSExport
      trait Test2 extends js.Object
    }
    """ hasErrors
    """
      |newSource1.scala:4: error: You may not export a trait
      |      @JSExport
      |       ^
      |newSource1.scala:7: error: You may not export a trait
      |      @JSExport
      |       ^
    """

  }

  @Test
  def noExportNonPublicClassOrObject(): Unit = {

    """
    @JSExportTopLevel("A")
    private class A

    @JSExportTopLevel("B")
    protected[this] class B

    @JSExportTopLevel("C")
    private class C extends js.Object

    @JSExportTopLevel("D")
    protected[this] class D extends js.Object

    private class E(x: Int) {
      @JSExportTopLevel("E")
      def this() = this(1)
    }
    """ hasErrors
    """
      |newSource1.scala:3: error: You may only export constructors of public and protected classes
      |    @JSExportTopLevel("A")
      |     ^
      |newSource1.scala:6: error: You may only export constructors of public and protected classes
      |    @JSExportTopLevel("B")
      |     ^
      |newSource1.scala:9: error: You may only export public and protected definitions
      |    @JSExportTopLevel("C")
      |     ^
      |newSource1.scala:12: error: You may only export public and protected definitions
      |    @JSExportTopLevel("D")
      |     ^
      |newSource1.scala:16: error: You may only export constructors of public and protected classes
      |      @JSExportTopLevel("E")
      |       ^
    """

    """
    @JSExportTopLevel("A")
    private object A

    @JSExportTopLevel("B")
    protected[this] object B

    @JSExportTopLevel("C")
    private object C extends js.Object

    @JSExportTopLevel("D")
    protected[this] object D extends js.Object
    """ hasErrors
    """
      |newSource1.scala:3: error: You may only export public and protected definitions
      |    @JSExportTopLevel("A")
      |     ^
      |newSource1.scala:6: error: You may only export public and protected definitions
      |    @JSExportTopLevel("B")
      |     ^
      |newSource1.scala:9: error: You may only export public and protected definitions
      |    @JSExportTopLevel("C")
      |     ^
      |newSource1.scala:12: error: You may only export public and protected definitions
      |    @JSExportTopLevel("D")
      |     ^
    """

  }

  @Test
  def noExportNonPublicMember(): Unit = {

    """
    class A {
      @JSExport
      private def foo = 1

      @JSExport
      protected[this] def bar = 2
    }
    """ hasErrors
    """
      |newSource1.scala:4: error: You may only export public and protected definitions
      |      @JSExport
      |       ^
      |newSource1.scala:7: error: You may only export public and protected definitions
      |      @JSExport
      |       ^
    """

  }

  @Test
  def noExportTopLevelNestedObject(): Unit = {

    """
    class A {
      @JSExportTopLevel("Nested")
      object Nested

      @JSExportTopLevel("Nested2")
      object Nested2 extends js.Object
    }
    """ hasErrors
    """
      |newSource1.scala:4: error: Only static objects may export their members to the top level
      |      @JSExportTopLevel("Nested")
      |       ^
      |newSource1.scala:7: error: Only static objects may export their members to the top level
      |      @JSExportTopLevel("Nested2")
      |       ^
    """

  }

  @Test
  def noExportJSNative(): Unit = {

    """
    import scala.scalajs.js

    @JSExportTopLevel("A")
    @js.native
    @JSGlobal("Dummy")
    object A extends js.Object
    """ hasErrors
    """
      |newSource1.scala:5: error: You may not export a native JS definition
      |    @JSExportTopLevel("A")
      |     ^
    """

    """
    import scala.scalajs.js

    @JSExportTopLevel("A")
    @js.native
    trait A extends js.Object
    """ hasErrors
    """
      |newSource1.scala:5: error: You may not export a trait
      |    @JSExportTopLevel("A")
      |     ^
    """

    """
    import scala.scalajs.js

    @JSExportTopLevel("A")
    @js.native
    @JSGlobal("Dummy")
    class A extends js.Object {
      @JSExportTopLevel("A")
      def this(x: Int) = this()
    }
    """ hasErrors
    """
      |newSource1.scala:5: error: You may not export a native JS definition
      |    @JSExportTopLevel("A")
      |     ^
      |newSource1.scala:9: error: You may not export a member of a subclass of js.Any
      |      @JSExportTopLevel("A")
      |       ^
    """

    """
    import scala.scalajs.js

    object A {
      @JSExport("A")
      @js.native
      @JSGlobal("a")
      def a(x: Int): Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:6: error: You may not export a native JS definition
      |      @JSExport("A")
      |       ^
    """
  }

  @Test
  def noExportJSMember(): Unit = {

    """
    import scala.scalajs.js

    @js.native
    @JSGlobal("Dummy")
    class A extends js.Object {
      @JSExport
      def foo: Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:8: error: You may not export a member of a subclass of js.Any
      |      @JSExport
      |       ^
    """

    """
    import scala.scalajs.js

    class A extends js.Object {
      @JSExport
      def foo: Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:6: error: You may not export a member of a subclass of js.Any
      |      @JSExport
      |       ^
    """

  }

  @Test
  def noBadSetterType(): Unit = {

    // Bad param list
    """
    class A {
      @JSExport
      def foo_=(x: Int, y: Int) = ()
    }
    """ hasErrors
    """
      |newSource1.scala:4: error: Exported setters must have exactly one argument
      |      @JSExport
      |       ^
    """

    // Bad return type
    """
    class A {
      @JSExport
      def foo_=(x: Int) = "string"
    }
    """ hasErrors
    """
      |newSource1.scala:4: error: Exported setters must return Unit
      |      @JSExport
      |       ^
    """

    // Varargs
    """
    class A {
      @JSExport
      def foo_=(x: Int*) = ()
    }
    """ hasErrors
    """
      |newSource1.scala:4: error: Exported setters may not have repeated params
      |      @JSExport
      |       ^
    """

    // Default arguments
    """
    class A {
      @JSExport
      def foo_=(x: Int = 1) = ()
    }
    """ hasErrors
    """
      |newSource1.scala:4: error: Exported setters may not have default params
      |      @JSExport
      |       ^
    """

  }

  @Test
  def noNonSetterNameForNonSetter(): Unit = {

    """
    class A {
      @JSExport
      class A_=
    }
    """ hasErrors
    """
      |newSource1.scala:4: error: You must set an explicit name when exporting a non-setter with a name ending in _=
      |      @JSExport
      |       ^
    """

    """
    class A {
      @JSExport
      object A_=
    }
    """ hasErrors
    """
      |newSource1.scala:4: error: You must set an explicit name when exporting a non-setter with a name ending in _=
      |      @JSExport
      |       ^
    """

    // Not a Scala.js error, but we check it anyways to complete the test suite.
    """
    class A {
      @JSExport
      val A_= = 1
    }
    """.fails() // error is different on 2.12 / 2.13

  }

  @Test
  def noBadToStringExport(): Unit = {

    """
    class A {
      @JSExport("toString")
      def a(): Int = 5
    }
    """ hasErrors
    """
      |newSource1.scala:4: error: You may not export a zero-argument method named other than 'toString' under the name 'toString'
      |      @JSExport("toString")
      |       ^
    """

  }

  @Test
  def noBadNameExportAll(): Unit = {

    """
    @JSExportAll
    class A {
      val __f = 1
      def a_= = 2
    }
    """ hasErrors
    """
      |newSource1.scala:5: error: An exported name may not contain a double underscore (`__`)
      |      val __f = 1
      |          ^
      |newSource1.scala:3: error: Exported setters must return Unit
      |    @JSExportAll
      |     ^
    """

  }

  @Test
  def noConflictingMethodAndProperty(): Unit = {

    // Basic case
    """
    class A {
      @JSExport("a")
      def bar() = 2

      @JSExport("a")
      val foo = 1
    }
    """ hasErrors
    """
    |newSource1.scala:4: error: Exported property a conflicts with A.$js$exported$meth$a
    |      @JSExport("a")
    |       ^
    |newSource1.scala:7: error: Exported method a conflicts with A.$js$exported$prop$a
    |      @JSExport("a")
    |       ^
    """

    // Inherited case
    """
    class A {
      @JSExport("a")
      def bar() = 2
    }

    class B extends A {
      @JSExport("a")
      def foo_=(x: Int): Unit = ()

      @JSExport("a")
      val foo = 1
    }
    """ hasErrors
    """
      |newSource1.scala:4: error: Exported property a conflicts with A.$js$exported$meth$a
      |      @JSExport("a")
      |       ^
    """

  }

  @Test
  def gracefulDoubleDefaultFail(): Unit = {
    // This used to blow up (i.e. not just fail), because PrepJSExports asked
    // for the symbol of the default parameter getter of [[y]], and asserted its
    // not overloaded. Since the Scala compiler only fails later on this, the
    // assert got triggered and made the compiler crash
    """
    class A {
      @JSExport
      def foo(x: String, y: String = "hello") = x
      def foo(x: Int, y: String = "bar") = x
    }
    """ hasErrors
    """
      |newSource1.scala:3: error: in class A, multiple overloaded alternatives of method foo define default arguments.
      |    class A {
      |          ^
    """
  }

  @Test
  def noNonLiteralExportNames(): Unit = {

    """
    object A {
      val a = "Hello"
      final val b = "World"
    }

    class B {
      @JSExport(A.a)
      def foo = 1
      @JSExport(A.b)
      def bar = 1
    }
    """ hasErrors
    """
      |newSource1.scala:9: error: The argument to JSExport must be a literal string
      |      @JSExport(A.a)
      |                  ^
    """

  }

  @Test
  def noNonLiteralModuleID(): Unit = {

    """
    object A {
      val a = "Hello"
      final val b = "World"
    }

    object B {
      @JSExportTopLevel("foo", A.a)
      def foo() = 1
      @JSExportTopLevel("foo", A.b)
      def bar() = 1
    }
    """ hasErrors
    """
      |newSource1.scala:9: error: moduleID must be a literal string
      |      @JSExportTopLevel("foo", A.a)
      |                                 ^
    """

  }

  @Test
  def noExportImplicitApply(): Unit = {

    """
    class A {
      @JSExport
      def apply(): Int = 1
    }
    """ hasErrors
    """
      |newSource1.scala:4: error: A member cannot be exported to function application. Add @JSExport("apply") to export under the name apply.
      |      @JSExport
      |       ^
    """

    """
    @JSExportAll
    class A {
      def apply(): Int = 1
    }
    """ hasErrors
    """
      |newSource1.scala:5: error: A member cannot be exported to function application. Add @JSExport("apply") to export under the name apply.
      |      def apply(): Int = 1
      |          ^
    """

    """
    @JSExportAll
    class A {
      @JSExport("foo")
      def apply(): Int = 1
    }
    """ hasErrors
    """
      |newSource1.scala:6: error: A member cannot be exported to function application. Add @JSExport("apply") to export under the name apply.
      |      def apply(): Int = 1
      |          ^
    """

    """
    @JSExportAll
    class A {
      @JSExport("apply")
      def apply(): Int = 1
    }
    """.hasNoWarns()

    """
    class StaticContainer extends js.Object

    object StaticContainer {
      @JSExportStatic
      def apply(): Int = 1
    }
    """ hasErrors
    """
      |newSource1.scala:6: error: A member cannot be exported to function application as static. Use @JSExportStatic("apply") to export it under the name 'apply'.
      |      @JSExportStatic
      |       ^
    """

    """
    class StaticContainer extends js.Object

    object StaticContainer {
      @JSExportStatic("apply")
      def apply(): Int = 1
    }
    """.hasNoWarns()
  }

  @Test
  def exportObjectAsToString(): Unit = {

    """
    @JSExportTopLevel("toString")
    object ExportAsToString
    """.succeeds()

  }

  @Test
  def noExportTopLevelTrait(): Unit = {
    """
    @JSExportTopLevel("foo")
    trait A

    @JSExportTopLevel("bar")
    trait B extends js.Object
    """ hasErrors
    """
      |newSource1.scala:3: error: You may not export a trait
      |    @JSExportTopLevel("foo")
      |     ^
      |newSource1.scala:6: error: You may not export a trait
      |    @JSExportTopLevel("bar")
      |     ^
    """

    """
    object Container {
      @JSExportTopLevel("foo")
      trait A

      @JSExportTopLevel("bar")
      trait B extends js.Object
    }
    """ hasErrors
    """
      |newSource1.scala:4: error: You may not export a trait
      |      @JSExportTopLevel("foo")
      |       ^
      |newSource1.scala:7: error: You may not export a trait
      |      @JSExportTopLevel("bar")
      |       ^
    """
  }

  @Test
  def noExportTopLevelLazyVal(): Unit = {
    """
    object A {
      @JSExportTopLevel("foo")
      lazy val a: Int = 1
    }
    """ hasErrors
    """
      |newSource1.scala:4: error: You may not export a lazy val to the top level
      |      @JSExportTopLevel("foo")
      |       ^
    """
  }

  @Test
  def noExportTopLevelInvalidJSIdentifier(): Unit = {
    """
    @JSExportTopLevel("not-a-valid-JS-identifier-1")
    object A

    @JSExportTopLevel("not-a-valid-JS-identifier-2")
    class B

    object C {
      @JSExportTopLevel("not-a-valid-JS-identifier-3")
      val a: Int = 1

      @JSExportTopLevel("not-a-valid-JS-identifier-4")
      var b: Int = 1

      @JSExportTopLevel("not-a-valid-JS-identifier-5")
      def c(): Int = 1
    }

    @JSExportTopLevel("")
    object D
    """ hasErrors
    """
      |newSource1.scala:3: error: The top-level export name must be a valid JavaScript identifier name
      |    @JSExportTopLevel("not-a-valid-JS-identifier-1")
      |     ^
      |newSource1.scala:6: error: The top-level export name must be a valid JavaScript identifier name
      |    @JSExportTopLevel("not-a-valid-JS-identifier-2")
      |     ^
      |newSource1.scala:10: error: The top-level export name must be a valid JavaScript identifier name
      |      @JSExportTopLevel("not-a-valid-JS-identifier-3")
      |       ^
      |newSource1.scala:13: error: The top-level export name must be a valid JavaScript identifier name
      |      @JSExportTopLevel("not-a-valid-JS-identifier-4")
      |       ^
      |newSource1.scala:16: error: The top-level export name must be a valid JavaScript identifier name
      |      @JSExportTopLevel("not-a-valid-JS-identifier-5")
      |       ^
      |newSource1.scala:20: error: The top-level export name must be a valid JavaScript identifier name
      |    @JSExportTopLevel("")
      |     ^
    """
  }

  @Test
  def noExportTopLevelNamespaced(): Unit = {
    """
    @JSExportTopLevel("namespaced.export1")
    object A
    @JSExportTopLevel("namespaced.export2")
    class B
    object C {
      @JSExportTopLevel("namespaced.export3")
      val a: Int = 1
      @JSExportTopLevel("namespaced.export4")
      var b: Int = 1
      @JSExportTopLevel("namespaced.export5")
      def c(): Int = 1
    }
    """ hasErrors
    """
      |newSource1.scala:3: error: The top-level export name must be a valid JavaScript identifier name
      |    @JSExportTopLevel("namespaced.export1")
      |     ^
      |newSource1.scala:5: error: The top-level export name must be a valid JavaScript identifier name
      |    @JSExportTopLevel("namespaced.export2")
      |     ^
      |newSource1.scala:8: error: The top-level export name must be a valid JavaScript identifier name
      |      @JSExportTopLevel("namespaced.export3")
      |       ^
      |newSource1.scala:10: error: The top-level export name must be a valid JavaScript identifier name
      |      @JSExportTopLevel("namespaced.export4")
      |       ^
      |newSource1.scala:12: error: The top-level export name must be a valid JavaScript identifier name
      |      @JSExportTopLevel("namespaced.export5")
      |       ^
    """
  }

  @Test
  def noExportTopLevelGetter(): Unit = {
    """
    object A {
      @JSExportTopLevel("foo")
      def a: Int = 1
    }
    """ hasErrors
    """
      |newSource1.scala:4: error: You may not export a getter or a setter to the top level
      |      @JSExportTopLevel("foo")
      |       ^
    """
  }

  @Test
  def noExportTopLevelSetter(): Unit = {
    """
    object A {
      @JSExportTopLevel("foo")
      def a_=(x: Int): Unit = ()
    }
    """ hasErrors
    """
      |newSource1.scala:4: error: You may not export a getter or a setter to the top level
      |      @JSExportTopLevel("foo")
      |       ^
    """
  }

  @Test
  def noExportTopLevelFieldsWithSameName(): Unit = {
    """
    object A {
      @JSExportTopLevel("foo")
      val a: Int = 1

      @JSExportTopLevel("foo")
      var b: Int = 1
    }
    """ hasErrors
    """
      |newSource1.scala:4: error: export overload conflicts with export of variable b: a field may not share its exported name with another export
      |      @JSExportTopLevel("foo")
      |       ^
    """
  }

  @Test
  def noExportTopLevelFieldsAndMethodsWithSameName(): Unit = {
    """
    object A {
      @JSExportTopLevel("foo")
      val a: Int = 1

      @JSExportTopLevel("foo")
      def b(x: Int): Int = x + 1
    }
    """ hasErrors
    """
      |newSource1.scala:4: error: export overload conflicts with export of method b: they are of different types (Field / Method)
      |      @JSExportTopLevel("foo")
      |       ^
    """

    """
    object A {
      @JSExportTopLevel("foo")
      def a(x: Int): Int = x + 1

      @JSExportTopLevel("foo")
      val b: Int = 1
    }
    """ hasErrors
    """
      |newSource1.scala:4: error: export overload conflicts with export of value b: they are of different types (Method / Field)
      |      @JSExportTopLevel("foo")
      |       ^
    """
  }

  @Test
  def noExportTopLevelNonStatic(): Unit = {
    """
    class A {
      @JSExportTopLevel("foo")
      def a(): Unit = ()
    }
    """ hasErrors
    """
      |newSource1.scala:4: error: Only static objects may export their members to the top level
      |      @JSExportTopLevel("foo")
      |       ^
    """

    """
    class A {
      object B {
        @JSExportTopLevel("foo")
        def a(): Unit = ()
      }
    }
    """ hasErrors
    """
      |newSource1.scala:5: error: Only static objects may export their members to the top level
      |        @JSExportTopLevel("foo")
      |         ^
    """

    """
    class A {
      @JSExportTopLevel("Foo")
      object B
    }
    """ hasErrors
    """
      |newSource1.scala:4: error: Only static objects may export their members to the top level
      |      @JSExportTopLevel("Foo")
      |       ^
    """

    """
    class A {
      @JSExportTopLevel("Foo")
      object B extends js.Object
    }
    """ hasErrors
    """
      |newSource1.scala:4: error: Only static objects may export their members to the top level
      |      @JSExportTopLevel("Foo")
      |       ^
    """

    """
    class A {
      @JSExportTopLevel("Foo")
      class B extends js.Object
    }
    """ hasErrors
    """
      |newSource1.scala:4: error: Only static objects may export their members to the top level
      |      @JSExportTopLevel("Foo")
      |       ^
    """

    """
    class A {
      @JSExportTopLevel("Foo")
      class B
    }
    """ hasErrors
    """
      |newSource1.scala:4: error: Only static objects may export their members to the top level
      |      @JSExportTopLevel("Foo")
      |       ^
    """
  }

  @Test
  def noExportTopLevelLocal(): Unit = {
    // Local class
    """
    class A {
      def method = {
        @JSExportTopLevel("A")
        class A

        @JSExportTopLevel("B")
        class B extends js.Object
      }
    }
    """ hasErrors
    """
      |newSource1.scala:5: error: You may not export constructors of local classes
      |        @JSExportTopLevel("A")
      |         ^
      |newSource1.scala:8: error: You may not export a local definition
      |        @JSExportTopLevel("B")
      |         ^
    """

    // Local object
    """
    class A {
      def method = {
        @JSExportTopLevel("A")
        object A

        @JSExportTopLevel("B")
        object B extends js.Object
      }
    }
    """ hasErrors
    """
      |newSource1.scala:5: error: You may not export a local definition
      |        @JSExportTopLevel("A")
      |         ^
      |newSource1.scala:8: error: You may not export a local definition
      |        @JSExportTopLevel("B")
      |         ^
    """
  }

  @Test
  def noExportTopLevelJSModule(): Unit = {
    """
    object A extends js.Object {
      @JSExportTopLevel("foo")
      def a(): Unit = ()
    }
    """ hasErrors
    """
      |newSource1.scala:4: error: You may not export a member of a subclass of js.Any
      |      @JSExportTopLevel("foo")
      |       ^
    """
  }

  @Test
  def noExportStaticModule(): Unit = {
    """
    class StaticContainer extends js.Object

    object StaticContainer {
      @JSExportStatic
      object A
    }
    """ hasErrors
    """
      |newSource1.scala:6: error: Implementation restriction: cannot export a class or object as static
      |      @JSExportStatic
      |       ^
    """
  }

  @Test
  def noExportStaticTrait(): Unit = {
    """
    class StaticContainer extends js.Object

    object StaticContainer {
      @JSExportStatic
      trait A
    }
    """ hasErrors
    """
      |newSource1.scala:6: error: You may not export a trait
      |      @JSExportStatic
      |       ^
    """
  }

  @Test
  def noExportStaticClass(): Unit = {
    """
    class StaticContainer extends js.Object

    object StaticContainer {
      @JSExportStatic
      class A
    }
    """ hasErrors
    """
      |newSource1.scala:6: error: Implementation restriction: cannot export a class or object as static
      |      @JSExportStatic
      |       ^
    """

    """
    class StaticContainer extends js.Object

    object StaticContainer {
      class A {
        @JSExportStatic
        def this(x: Int) = this()
      }
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: Implementation restriction: cannot export a class or object as static
      |        @JSExportStatic
      |         ^
    """
  }

  @Test
  def noExportStaticValTwice(): Unit = {
    """
    class StaticContainer extends js.Object

    object StaticContainer {
      @JSExportStatic
      @JSExportStatic("b")
      val a: Int = 1
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: Fields (val or var) cannot be exported as static more than once
      |      @JSExportStatic("b")
      |       ^
    """
  }

  @Test
  def noExportStaticVarTwice(): Unit = {
    """
    class StaticContainer extends js.Object

    object StaticContainer {
      @JSExportStatic
      @JSExportStatic("b")
      var a: Int = 1
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: Fields (val or var) cannot be exported as static more than once
      |      @JSExportStatic("b")
      |       ^
    """
  }

  @Test
  def noExportStaticLazyVal(): Unit = {
    """
    class StaticContainer extends js.Object

    object StaticContainer {
      @JSExportStatic
      lazy val a: Int = 1
    }
    """ hasErrors
    """
      |newSource1.scala:6: error: You may not export a lazy val as static
      |      @JSExportStatic
      |       ^
    """
  }

  @Test
  def noExportValAsStaticAndTopLevel(): Unit = {
    """
    class StaticContainer extends js.Object

    object StaticContainer {
      @JSExportStatic
      @JSExportTopLevel("foo")
      val a: Int = 1
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: Fields (val or var) cannot be exported both as static and at the top-level
      |      @JSExportTopLevel("foo")
      |       ^
    """
  }

  @Test
  def noExportVarAsStaticAndTopLevel(): Unit = {
    """
    class StaticContainer extends js.Object

    object StaticContainer {
      @JSExportStatic
      @JSExportTopLevel("foo")
      var a: Int = 1
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: Fields (val or var) cannot be exported both as static and at the top-level
      |      @JSExportTopLevel("foo")
      |       ^
    """
  }

  @Test
  def noExportSetterWithBadSetterType(): Unit = {
    """
    class StaticContainer extends js.Object

    object StaticContainer {
      @JSExportStatic
      def a_=(x: Int, y: Int): Unit = ()
    }
    """ hasErrors
    """
      |newSource1.scala:6: error: Exported setters must have exactly one argument
      |      @JSExportStatic
      |       ^
    """
  }

  @Test
  def noExportStaticCollapsingMethods(): Unit = {
    """
    class StaticContainer extends js.Object

    object StaticContainer {
      @JSExportStatic
      def foo(x: Int): Int = x

      @JSExportStatic("foo")
      def bar(x: Int): Int = x + 1
    }
    """ hasErrors
    s"""
      |newSource1.scala:10: error: Cannot disambiguate overloads for exported method foo with types
      |  ${methodSig("(x: Int)", "Int")}
      |  ${methodSig("(x: Int)", "Int")}
      |      def bar(x: Int): Int = x + 1
      |          ^
    """
  }

  @Test
  def noExportStaticCollapsingGetters(): Unit = {
    """
    class StaticContainer extends js.Object

    object StaticContainer {
      @JSExportStatic
      def foo: Int = 1

      @JSExportStatic("foo")
      def bar: Int = 2
    }
    """ hasErrors
    s"""
      |newSource1.scala:10: error: Cannot disambiguate overloads for exported getter foo with types
      |  ${methodSig("()", "Int")}
      |  ${methodSig("()", "Int")}
      |      def bar: Int = 2
      |          ^
    """
  }

  @Test
  def noExportStaticCollapsingSetters(): Unit = {
    """
    class StaticContainer extends js.Object

    object StaticContainer {
      @JSExportStatic
      def foo_=(v: Int): Unit = ()

      @JSExportStatic("foo")
      def bar_=(v: Int): Unit = ()
    }
    """ hasErrors
    s"""
      |newSource1.scala:10: error: Cannot disambiguate overloads for exported setter foo with types
      |  ${methodSig("(v: Int)", "Unit")}
      |  ${methodSig("(v: Int)", "Unit")}
      |      def bar_=(v: Int): Unit = ()
      |          ^
    """
  }

  @Test
  def noExportStaticFieldsWithSameName(): Unit = {
    """
    class StaticContainer extends js.Object

    object StaticContainer {
      @JSExportStatic
      val a: Int = 1

      @JSExportStatic("a")
      var b: Int = 1
    }
    """ hasErrors
    """
      |newSource1.scala:6: error: export overload conflicts with export of variable b: a field may not share its exported name with another export
      |      @JSExportStatic
      |       ^
    """
  }

  @Test
  def noExportStaticFieldsAndMethodsWithSameName(): Unit = {
    """
    class StaticContainer extends js.Object

    object StaticContainer {
      @JSExportStatic
      val a: Int = 1

      @JSExportStatic("a")
      def b(x: Int): Int = x + 1
    }
    """ hasErrors
    """
      |newSource1.scala:6: error: export overload conflicts with export of method b: they are of different types (Field / Method)
      |      @JSExportStatic
      |       ^
    """

    """
    class StaticContainer extends js.Object

    object StaticContainer {
      @JSExportStatic
      def a(x: Int): Int = x + 1

      @JSExportStatic("a")
      val b: Int = 1
    }
    """ hasErrors
    """
      |newSource1.scala:6: error: export overload conflicts with export of value b: they are of different types (Method / Field)
      |      @JSExportStatic
      |       ^
    """
  }

  @Test
  def noExportStaticFieldsAndPropertiesWithSameName(): Unit = {
    """
    class StaticContainer extends js.Object

    object StaticContainer {
      @JSExportStatic
      val a: Int = 1

      @JSExportStatic("a")
      def b: Int = 2
    }
    """ hasErrors
    """
      |newSource1.scala:6: error: export overload conflicts with export of method b: they are of different types (Field / Property)
      |      @JSExportStatic
      |       ^
    """

    """
    class StaticContainer extends js.Object

    object StaticContainer {
      @JSExportStatic
      def a: Int = 1

      @JSExportStatic("a")
      val b: Int = 2
    }
    """ hasErrors
    """
      |newSource1.scala:6: error: export overload conflicts with export of value b: they are of different types (Property / Field)
      |      @JSExportStatic
      |       ^
    """
  }

  @Test
  def noExportStaticPropertiesAndMethodsWithSameName(): Unit = {
    """
    class StaticContainer extends js.Object

    object StaticContainer {
      @JSExportStatic
      def a: Int = 1

      @JSExportStatic("a")
      def b(x: Int): Int = x + 1
    }
    """ hasErrors
    """
      |newSource1.scala:6: error: export overload conflicts with export of method b: they are of different types (Property / Method)
      |      @JSExportStatic
      |       ^
    """

    """
    class StaticContainer extends js.Object

    object StaticContainer {
      @JSExportStatic
      def a(x: Int): Int = x + 1

      @JSExportStatic("a")
      def b: Int = 1
    }
    """ hasErrors
    """
      |newSource1.scala:6: error: export overload conflicts with export of method b: they are of different types (Method / Property)
      |      @JSExportStatic
      |       ^
    """
  }

  @Test
  def noExportStaticNonStatic(): Unit = {
    """
    class A {
      class StaticContainer extends js.Object

      object StaticContainer {
        @JSExportStatic
        def a(): Unit = ()
      }
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: Only a static object whose companion class is a non-native JS class may export its members as static.
      |        @JSExportStatic
      |         ^
    """
  }

  @Test
  def noExportStaticInJSModule(): Unit = {
    """
    class StaticContainer extends js.Object

    object StaticContainer extends js.Object {
      @JSExportStatic
      def a(): Unit = ()
    }
    """ hasErrors
    """
      |newSource1.scala:6: error: You may not export a member of a subclass of js.Any
      |      @JSExportStatic
      |       ^
    """

    """
    class StaticContainer extends js.Object

    @js.native
    @JSGlobal("Dummy")
    object StaticContainer extends js.Object {
      @JSExportStatic
      def a(): Unit = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:8: error: You may not export a member of a subclass of js.Any
      |      @JSExportStatic
      |       ^
    """
  }

  @Test
  def noExportStaticIfWrongCompanionType(): Unit = {
    """
    class StaticContainer

    object StaticContainer {
      @JSExportStatic
      def a(): Unit = ()
    }
    """ hasErrors
    """
      |newSource1.scala:6: error: Only a static object whose companion class is a non-native JS class may export its members as static.
      |      @JSExportStatic
      |       ^
    """

    """
    trait StaticContainer extends js.Object

    object StaticContainer {
      @JSExportStatic
      def a(): Unit = ()
    }
    """ hasErrors
    """
      |newSource1.scala:6: error: Only a static object whose companion class is a non-native JS class may export its members as static.
      |      @JSExportStatic
      |       ^
    """

    """
    @js.native
    @JSGlobal("Dummy")
    class StaticContainer extends js.Object

    object StaticContainer {
      @JSExportStatic
      def a(): Unit = ()
    }
    """ hasErrors
    """
      |newSource1.scala:8: error: Only a static object whose companion class is a non-native JS class may export its members as static.
      |      @JSExportStatic
      |       ^
    """
  }

  @Test
  def noExportStaticFieldAfterStatOrNonStaticField(): Unit = {
    for {
      offendingDecl <- Seq(
          "val a: Int = 1",
          "var a: Int = 1",
          """println("foo")"""
      )
    }
    s"""
    class StaticContainer extends js.Object

    object StaticContainer {
      $offendingDecl

      @JSExportStatic
      val b: Int = 1

      @JSExportStatic
      var c: Int = 1

      @JSExportStatic
      def d: Int = 1

      @JSExportStatic
      def d_=(v: Int): Unit = ()

      @JSExportStatic
      def e(): Int = 1
    }
    """ hasErrors
    """
      |newSource1.scala:9: error: @JSExportStatic vals and vars must be defined before any other val/var, and before any constructor statement.
      |      val b: Int = 1
      |          ^
      |newSource1.scala:12: error: @JSExportStatic vals and vars must be defined before any other val/var, and before any constructor statement.
      |      var c: Int = 1
      |          ^
    """

    for {
      validDecl <- Seq(
          "@JSExportStatic val a: Int = 1",
          "@JSExportStatic var a: Int = 1",
          "lazy val a: Int = 1",
          "def a: Int = 1",
          "def a_=(v: Int): Unit = ()",
          "def a(): Int = 1",
          "@JSExportStatic def a: Int = 1",
          "@JSExportStatic def a_=(v: Int): Unit = ()",
          "@JSExportStatic def a(): Int = 1",
          "class A",
          "object A",
          "trait A",
          "type A = Int"
      )
    }
    s"""
    class StaticContainer extends js.Object

    object StaticContainer {
      $validDecl

      @JSExportStatic
      val b: Int = 1

      @JSExportStatic
      var c: Int = 1
    }
    """.succeeds()
  }

  @Test
  def succeedsWhenTopLevelExportWithDefaultExportOnClass(): Unit = {
    """
    @JSExportTopLevel("A")
    @JSExportDefault
    class A
    """.succeeds()
  }

  @Test
  def succeedsWhenTopLevelExportWithDefaultExportOnObject(): Unit = {
    """
    @JSExportTopLevel("A")
    @JSExportDefault
    object A
    """.succeeds()
  }

  @Test
  def succeedsWhenTopLevelExportWithDefaultExportOnObjectMember(): Unit = {
    """
    object A {
      @JSExportTopLevel("foo")
      @JSExportDefault
      def foo(x: Int): Int = x + 1
    }
    """.succeeds()
  }

  @Test
  def failsWhenUsingOnlyDefaultExportAnnotation(): Unit = {
    """
    @JSExportDefault
    class A
    """ hasErrors
    """newSource1.scala:3: error: @DefaultExport can be used in combination with @JSTopLevelExport only
      |    @JSExportDefault
      |     ^""".stripMargin
  }

  @Test
  def failsWhenThereAreMoreThanOneDefaultExports(): Unit = {
    """
    @JSExportTopLevel("A")
    @JSExportDefault
    class A

    @JSExportTopLevel("B")
    @JSExportDefault
    class B
    """ hasErrors
    """newSource1.scala:8: error: cannot export symbol: constructor B, symbol constructor A already exported at pos source-newSource1.scala,line-4,offset=78
      |    @JSExportDefault
      |     ^""".stripMargin
  }

  @Test
  def failedWhenDefaultExportIsCombinedWithRegularExportOnly(): Unit = {
    """
    @JSExportTopLevel("A")
    object A {
      @JSExport
      @JSExportDefault
      def foo(x: Int): Int = x + 1
    }
    """ hasErrors
    """
    newSource1.scala:6: error: @DefaultExport can be used in combination with @JSTopLevelExport only
    |      @JSExportDefault
    |       ^""".stripMargin
  }
}
