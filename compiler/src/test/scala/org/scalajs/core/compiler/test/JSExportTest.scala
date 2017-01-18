package org.scalajs.core.compiler.test

import org.scalajs.core.compiler.test.util._
import org.junit.Test

// scalastyle:off line.size.limit

class JSExportTest extends DirectTest with TestHelpers {

  override def extraArgs: List[String] =
    super.extraArgs :+ "-deprecation"

  override def preamble: String =
    """import scala.scalajs.js, js.annotation._
    """

  @Test
  def noDoubleUnderscoreExport: Unit = {
    // Normal exports
    """
    class A {
      @JSExport(name = "__")
      def foo = 1

      @JSExport
      def bar__(x: Int) = x
    }

    @JSExport
    class B__

    @JSExport
    @ScalaJSDefined class C__ extends js.Object
    """ hasErrors
    """
      |newSource1.scala:4: error: An exported name may not contain a double underscore (`__`)
      |      @JSExport(name = "__")
      |                       ^
      |newSource1.scala:8: error: An exported name may not contain a double underscore (`__`)
      |      def bar__(x: Int) = x
      |          ^
      |newSource1.scala:12: error: An exported name may not contain a double underscore (`__`)
      |    class B__
      |          ^
      |newSource1.scala:15: error: An exported name may not contain a double underscore (`__`)
      |    @ScalaJSDefined class C__ extends js.Object
      |                          ^
    """

    // Inherited exports (objects)
    """
    @JSExportDescendentObjects
    trait A

    package fo__o {
      object B extends A
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: B may not have a double underscore (`__`) in its fully qualified name, since it is forced to be exported by a @JSExportDescendentObjects on trait A
      |      object B extends A
      |             ^
    """

    """
    @JSExportDescendentObjects
    @ScalaJSDefined trait A extends js.Object

    package fo__o {
      @ScalaJSDefined object B extends A
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: B may not have a double underscore (`__`) in its fully qualified name, since it is forced to be exported by a @JSExportDescendentObjects on trait A
      |      @ScalaJSDefined object B extends A
      |                             ^
    """

    // Inherited exports (classes)
    """
    @JSExportDescendentClasses
    trait A

    package fo__o {
      class B(x: Int) extends A {
        def this() = this(1)
        private def this(s: String) = this(1)
      }
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: B may not have a double underscore (`__`) in its fully qualified name, since it is forced to be exported by a @JSExportDescendentClasses on trait A
      |      class B(x: Int) extends A {
      |             ^
      |newSource1.scala:8: error: B may not have a double underscore (`__`) in its fully qualified name, since it is forced to be exported by a @JSExportDescendentClasses on trait A
      |        def this() = this(1)
      |            ^
    """

    """
    @JSExportDescendentClasses
    @ScalaJSDefined trait A extends js.Object

    package fo__o {
      @ScalaJSDefined class B(x: Int) extends A
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: B may not have a double underscore (`__`) in its fully qualified name, since it is forced to be exported by a @JSExportDescendentClasses on trait A
      |      @ScalaJSDefined class B(x: Int) extends A
      |                            ^
    """
  }

  @Test
  def noConflictingExport: Unit = {
    """
    class Confl {
      @JSExport("value")
      def hello = "foo"

      @JSExport("value")
      def world = "bar"
    }
    """ fails() // No error test, Scala version dependent error messages

    """
    class Confl {
      class Box[T](val x: T)

      @JSExport
      def ub(x: Box[String]): String = x.x
      @JSExport
      def ub(x: Box[Int]): Int = x.x
    }
    """ fails() // No error test, Scala version dependent error messages

    """
    class Confl {
      @JSExport
      def rtType(x: scala.scalajs.js.prim.Number) = x

      @JSExport
      def rtType(x: Double) = x
    }
    """ fails() // Error message depends on Scala version

    """
    class Confl {
      @JSExport
      def foo(x: Int)(ys: Int*) = x

      @JSExport
      def foo(x: Int*) = x
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: Cannot disambiguate overloads for exported method $js$exported$meth$foo with types
      |  (x: Seq)Object
      |  (x: Int, ys: Seq)Object
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
    """
      |newSource1.scala:4: error: Cannot disambiguate overloads for exported method $js$exported$meth$foo with types
      |  (x: Int)Object
      |  (x: Seq)Object
      |      @JSExport
      |       ^
    """

    """
    class Confl {
      @JSExport
      def foo(x: scala.scalajs.js.prim.Number, y: String)(z: Int = 1) = x
      @JSExport
      def foo(x: Double, y: String)(z: String*) = x
    }
    """ fails() // Error message depends on Scala version

    """
    class A {
      @JSExport
      def a(x: scala.scalajs.js.Any) = 1

      @JSExport
      def a(x: Any) = 2
    }
    """ fails() // Error message depends on Scala version

  }

  @Test
  def noExportLocal: Unit = {
    // Local class
    """
    class A {
      def method = {
        @JSExport
        class A

        @JSExport
        @ScalaJSDefined class B extends js.Object
      }
    }
    """ hasErrors
    """
      |newSource1.scala:5: error: You may not export a local class
      |        @JSExport
      |         ^
      |newSource1.scala:8: error: You may not export a local class
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
        @ScalaJSDefined object B extends js.Object
      }
    }
    """ hasErrors
    """
      |newSource1.scala:5: error: You may not export a local object
      |        @JSExport
      |         ^
      |newSource1.scala:8: error: You may not export a local object
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
  def noMiddleVarArg: Unit = {

    """
    class A {
      @JSExport
      def method(xs: Int*)(ys: String) = 1
    }
    """ hasErrors
    """
      |newSource1.scala:4: error: In an exported method, a *-parameter must come last (through all parameter lists)
      |      @JSExport
      |       ^
    """

  }

  @Test
  def noMiddleDefaultParam: Unit = {

    """
    class A {
      @JSExport
      def method(x: Int = 1)(y: String) = 1
    }
    """ hasErrors
    """
      |newSource1.scala:4: error: In an exported method, all parameters with defaults must be at the end
      |      @JSExport
      |       ^
    """

  }

  @Test
  def noExportAbstractClass: Unit = {

    """
    @JSExport
    abstract class A

    abstract class B(x: Int) {
      @JSExport
      def this() = this(5)
    }

    @JSExport
    @ScalaJSDefined abstract class C extends js.Object
    """ hasErrors
    """
      |newSource1.scala:3: error: You may not export an abstract class
      |    @JSExport
      |     ^
      |newSource1.scala:7: error: You may not export an abstract class
      |      @JSExport
      |       ^
      |newSource1.scala:11: error: You may not export an abstract class
      |    @JSExport
      |     ^
    """

  }

  @Test
  def noExportTrait: Unit = {

    """
    @JSExport
    trait Test

    @JSExport
    @ScalaJSDefined trait Test2 extends js.Object

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

  }

  @Test
  def noExportNonPublicClassOrObject: Unit = {

    """
    @JSExport
    private class A

    @JSExport
    protected[this] class B

    @JSExport
    @ScalaJSDefined private class C extends js.Object

    @JSExport
    @ScalaJSDefined protected[this] class D extends js.Object
    """ hasErrors
    """
      |newSource1.scala:3: error: You may only export public and protected classes
      |    @JSExport
      |     ^
      |newSource1.scala:6: error: You may only export public and protected classes
      |    @JSExport
      |     ^
      |newSource1.scala:9: error: You may only export public and protected classes
      |    @JSExport
      |     ^
      |newSource1.scala:12: error: You may only export public and protected classes
      |    @JSExport
      |     ^
    """

    """
    @JSExport
    private object A

    @JSExport
    protected[this] object B

    @JSExport
    @ScalaJSDefined private object C extends js.Object

    @JSExport
    @ScalaJSDefined protected[this] object D extends js.Object
    """ hasErrors
    """
      |newSource1.scala:3: error: You may only export public and protected objects
      |    @JSExport
      |     ^
      |newSource1.scala:6: error: You may only export public and protected objects
      |    @JSExport
      |     ^
      |newSource1.scala:9: error: You may only export public and protected objects
      |    @JSExport
      |     ^
      |newSource1.scala:12: error: You may only export public and protected objects
      |    @JSExport
      |     ^
    """

  }

  @Test
  def noExportNonPublicMember: Unit = {

    """
    class A {
      @JSExport
      private def foo = 1

      @JSExport
      protected[this] def bar = 2
    }
    """ hasErrors
    """
      |newSource1.scala:4: error: You may only export public and protected methods
      |      @JSExport
      |       ^
      |newSource1.scala:7: error: You may only export public and protected methods
      |      @JSExport
      |       ^
    """

  }

  @Test
  def noExportNestedClass: Unit = {

    """
    class A {
      @JSExport
      class Nested {
        @JSExport
        def this(x: Int) = this()
      }

      @JSExport
      @ScalaJSDefined class Nested2 extends js.Object
    }
    """ hasErrors
    """
      |newSource1.scala:4: error: You may not export a nested class. Create an exported factory method in the outer class to work around this limitation.
      |      @JSExport
      |       ^
      |newSource1.scala:6: error: You may not export a nested class. Create an exported factory method in the outer class to work around this limitation.
      |        @JSExport
      |         ^
      |newSource1.scala:10: error: You may not export a nested class. Create an exported factory method in the outer class to work around this limitation.
      |      @JSExport
      |       ^
    """

  }

  @Test
  def noImplicitNameNestedExportClass: Unit = {

    """
    object A {
      @JSExport
      class Nested {
        @JSExport
        def this(x: Int) = this
      }

      @JSExport
      @ScalaJSDefined class Nested2 extends js.Object
    }
    """ hasErrors
    """
      |newSource1.scala:4: error: You must set an explicit name for exports of nested classes.
      |      @JSExport
      |       ^
      |newSource1.scala:6: error: You must set an explicit name for exports of nested classes.
      |        @JSExport
      |         ^
      |newSource1.scala:10: error: You must set an explicit name for exports of nested classes.
      |      @JSExport
      |       ^
    """

  }

  @Test
  def noExportNestedObject: Unit = {

    """
    class A {
      @JSExport
      object Nested

      @JSExport
      @ScalaJSDefined object Nested2 extends js.Object
    }
    """ hasErrors
    """
      |newSource1.scala:4: error: You may not export a nested object
      |      @JSExport
      |       ^
      |newSource1.scala:7: error: You may not export a nested object
      |      @JSExport
      |       ^
    """

  }

  @Test
  def noImplicitNameNestedExportObject: Unit = {

    """
    object A {
      @JSExport
      object Nested

      @JSExport
      @ScalaJSDefined object Nested2 extends js.Object
    }
    """ hasErrors
    """
      |newSource1.scala:4: error: You must set an explicit name for exports of nested classes.
      |      @JSExport
      |       ^
      |newSource1.scala:7: error: You must set an explicit name for exports of nested classes.
      |      @JSExport
      |       ^
    """

  }

  @Test
  def noExportJSRaw: Unit = {

    """
    import scala.scalajs.js

    @JSExport
    @js.native
    object A extends js.Object
    """ hasErrors
    """
      |newSource1.scala:5: error: You may not export a native JS class or object
      |    @JSExport
      |     ^
    """

    """
    import scala.scalajs.js

    @JSExport
    @js.native
    trait A extends js.Object
    """ hasErrors
    """
      |newSource1.scala:5: error: You may not export a trait
      |    @JSExport
      |     ^
    """

    """
    import scala.scalajs.js

    @JSExport
    @js.native
    class A extends js.Object {
      @JSExport
      def this(x: Int) = this()
    }
    """ hasErrors
    """
      |newSource1.scala:5: error: You may not export a native JS class or object
      |    @JSExport
      |     ^
      |newSource1.scala:8: error: You may not export a constructor of a subclass of js.Any
      |      @JSExport
      |       ^
    """

  }

  @Test
  def noExportJSRawMember: Unit = {

    """
    import scala.scalajs.js

    @js.native
    class A extends js.Object {
      @JSExport
      def foo: Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: You may not export a method of a subclass of js.Any
      |      @JSExport
      |       ^
    """

    """
    import scala.scalajs.js

    @ScalaJSDefined
    class A extends js.Object {
      @JSExport
      def foo: Int = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: You may not export a method of a subclass of js.Any
      |      @JSExport
      |       ^
    """

  }

  @Test
  def noBadSetterType: Unit = {

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
    """ hasWarns
    """
      |newSource1.scala:4: warning: Exported setters may not have default params. This will be enforced in 1.0.
      |      @JSExport
      |       ^
    """

  }

  @Test
  def noBadToStringExport: Unit = {

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
  def noBadNameExportAll: Unit = {

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
  def noConflictingMethodAndProperty: Unit = {

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
  def namedExportIsDeprecated: Unit = {

    """
    class A {
      @JSExportNamed
      def foo(x: Int, y: Int) = 1
    }
    """ hasWarns
    s"""
      |newSource1.scala:5: warning: class JSExportNamed in package annotation is deprecated${since("0.6.11")}: Use @JSExport with an explicit option bag instead. See the Scaladoc for more details.
      |      def foo(x: Int, y: Int) = 1
      |          ^
      |newSource1.scala:4: warning: class JSExportNamed in package annotation is deprecated${since("0.6.11")}: Use @JSExport with an explicit option bag instead. See the Scaladoc for more details.
      |      @JSExportNamed
      |       ^
    """

  }

  @Test
  def noOverrideNamedExport: Unit = {

    """
    class A {
      @JSExportNamed
      def foo(x: Int, y: Int) = 1
    }

    class B extends A {
      @JSExportNamed
      override def foo(x: Int, y: Int) = 2
    }
    """ hasErrors
    s"""
      |newSource1.scala:5: warning: class JSExportNamed in package annotation is deprecated${since("0.6.11")}: Use @JSExport with an explicit option bag instead. See the Scaladoc for more details.
      |      def foo(x: Int, y: Int) = 1
      |          ^
      |newSource1.scala:4: warning: class JSExportNamed in package annotation is deprecated${since("0.6.11")}: Use @JSExport with an explicit option bag instead. See the Scaladoc for more details.
      |      @JSExportNamed
      |       ^
      |newSource1.scala:9: error: overriding method $$js$$exported$$meth$$foo in class A of type (namedArgs: Any)Any;
      | method $$js$$exported$$meth$$foo cannot override final member
      |      @JSExportNamed
      |       ^
      |newSource1.scala:10: warning: class JSExportNamed in package annotation is deprecated${since("0.6.11")}: Use @JSExport with an explicit option bag instead. See the Scaladoc for more details.
      |      override def foo(x: Int, y: Int) = 2
      |                   ^
    """

  }

  @Test
  def noConflictNamedExport: Unit = {

    // Normal method
    """
    class A {
      @JSExportNamed
      def foo(x: Int, y: Int) = 1

      @JSExport
      def foo(x: scala.scalajs.js.Any) = 2
    }
    """ fails() // No error test, Scala version dependent error messages

    // Ctors
    """
    class A {
      @JSExportNamed
      def this(x: Int) = this()

      @JSExport
      def this(x: scala.scalajs.js.Any) = this

      @JSExportNamed
      def this(x: Long) = this()
    }
    """ fails() // No error test, Scala version dependent error messages

  }

  @Test
  def noNamedExportObject: Unit = {

    """
    @JSExportNamed
    object A

    @JSExportNamed
    @ScalaJSDefined object B extends js.Object
    """ hasErrors
    """
      |newSource1.scala:3: error: You may not use @JSNamedExport on an object
      |    @JSExportNamed
      |     ^
      |newSource1.scala:6: error: You may not use @JSNamedExport on an object
      |    @JSExportNamed
      |     ^
    """

  }

  @Test
  def noNamedExportSJSDefinedClass: Unit = {

    """
    @JSExportNamed
    @ScalaJSDefined class A extends js.Object
    """ hasErrors
    """
      |newSource1.scala:3: error: You may not use @JSNamedExport on a Scala.js-defined JS class
      |    @JSExportNamed
      |     ^
    """

  }

  @Test
  def noNamedExportVarArg: Unit = {

    """
    class A {
      @JSExportNamed
      def foo(a: Int*) = 1
    }
    """ hasErrors
    s"""
      |newSource1.scala:5: warning: class JSExportNamed in package annotation is deprecated${since("0.6.11")}: Use @JSExport with an explicit option bag instead. See the Scaladoc for more details.
      |      def foo(a: Int*) = 1
      |          ^
      |newSource1.scala:4: warning: class JSExportNamed in package annotation is deprecated${since("0.6.11")}: Use @JSExport with an explicit option bag instead. See the Scaladoc for more details.
      |      @JSExportNamed
      |       ^
      |newSource1.scala:4: error: You may not name-export a method with a *-parameter
      |      @JSExportNamed
      |       ^
    """

  }

  @Test
  def noNamedExportProperty: Unit = {

    // Getter
    """
    class A {
      @JSExportNamed
      def a = 1
    }
    """ hasErrors
    """
      |newSource1.scala:4: error: You may not export a getter or a setter as a named export
      |      @JSExportNamed
      |       ^
    """


    // Setter
    """
    class A {
      @JSExportNamed
      def a_=(x: Int) = ()
    }
    """ hasErrors
    """
      |newSource1.scala:4: error: You may not export a getter or a setter as a named export
      |      @JSExportNamed
      |       ^
    """

  }

  @Test
  def gracefulDoubleDefaultFail: Unit = {
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
    """ fails()
  }

  @Test
  def noNonLiteralExportNames: Unit = {

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
      |       ^
    """

  }

  @Test
  def noInheritIgnoreInvalidDescendants: Unit = {

    """
    @JSExportDescendentClasses
    trait A

    @JSExportDescendentClasses(ignoreInvalidDescendants = true)
    trait B

    object A {
      // Local class is not allowed
      def foo = { new A with B }
    }
    """ hasErrors
    """
      |newSource1.scala:11: error: You may not export a local class
      |      def foo = { new A with B }
      |                      ^
    """
  }

  @Test
  def noExportImplicitApply: Unit = {

    """
    class A {
      @JSExport
      def apply(): Int = 1
    }
    """ hasWarns
    """
      |newSource1.scala:4: warning: Member cannot be exported to function application. It is available under the name apply instead. Add @JSExport("apply") to silence this warning. This will be enforced in 1.0.
      |      @JSExport
      |       ^
    """

    """
    @JSExportAll
    class A {
      def apply(): Int = 1
    }
    """ hasWarns
    """
      |newSource1.scala:5: warning: Member cannot be exported to function application. It is available under the name apply instead. Add @JSExport("apply") to silence this warning. This will be enforced in 1.0.
      |      def apply(): Int = 1
      |          ^
    """

    // For this case, deprecation warnings are not exactly the same in 2.10.x
    """
    @JSExportAll
    class A {
      @JSExportNamed("apply")
      @JSExport("foo")
      def apply(): Int = 1
    }
    """ containsWarns
    """
      |newSource1.scala:7: warning: Member cannot be exported to function application. It is available under the name apply instead. Add @JSExport("apply") to silence this warning. This will be enforced in 1.0.
      |      def apply(): Int = 1
      |          ^
    """

    """
    @JSExportAll
    class A {
      @JSExport("apply")
      def apply(): Int = 1
    }
    """.hasNoWarns

  }

  @Test
  def exportObjectAsToString: Unit = {

    """
    @JSExport("toString")
    object ExportAsToString
    """.succeeds

  }

  private def since(v: String): String = {
    val version = scala.util.Properties.versionNumberString
    if (version.startsWith("2.10.") || version.startsWith("2.11.")) ""
    else s" (since $v)"
  }

  @Test
  def noExportTopLevelTrait: Unit = {
    """
    @JSExportTopLevel("foo")
    trait A

    @JSExportTopLevel("bar")
    @ScalaJSDefined
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
      @ScalaJSDefined
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
  def noExportTopLevelGetter: Unit = {
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
  def noExportTopLevelSetter: Unit = {
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
  def noExportTopLevelFieldsWithSameName: Unit = {
    """
    object A {
      @JSExportTopLevel("foo")
      val a: Int = 1

      @JSExportTopLevel("foo")
      var b: Int = 1
    }
    """ hasErrors
    """
      |newSource1.scala:5: error: Duplicate top-level export with name 'foo': a field may not share its exported name with another field or method
      |      val a: Int = 1
      |          ^
    """
  }

  @Test
  def noExportTopLevelFieldsAndMethodsWithSameName: Unit = {
    """
    object A {
      @JSExportTopLevel("foo")
      val a: Int = 1

      @JSExportTopLevel("foo")
      def b(x: Int): Int = x + 1
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: Duplicate top-level export with name 'foo': a field may not share its exported name with another field or method
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
      |newSource1.scala:4: error: Duplicate top-level export with name 'foo': a field may not share its exported name with another field or method
      |      @JSExportTopLevel("foo")
      |       ^
    """
  }

  @Test
  def noExportTopLevelNonStatic: Unit = {
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
      @ScalaJSDefined
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
      @ScalaJSDefined
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
  def noExportTopLevelJSModule: Unit = {
    """
    @ScalaJSDefined
    object A extends js.Object {
      @JSExportTopLevel("foo")
      def a(): Unit = ()
    }
    """ hasErrors
    """
      |newSource1.scala:5: error: You may not export a method of a subclass of js.Any
      |      @JSExportTopLevel("foo")
      |       ^
    """
  }

  @Test
  def noExportStaticModule: Unit = {
    """
    @ScalaJSDefined
    class StaticContainer extends js.Object

    object StaticContainer {
      @JSExportStatic
      object A
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: Implementation restriction: cannot export a class or object as static
      |      @JSExportStatic
      |       ^
    """
  }

  @Test
  def noExportStaticTrait: Unit = {
    """
    @ScalaJSDefined
    class StaticContainer extends js.Object

    object StaticContainer {
      @JSExportStatic
      trait A
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: You may not export a trait as static.
      |      @JSExportStatic
      |       ^
    """
  }

  @Test
  def noExportStaticClass: Unit = {
    """
    @ScalaJSDefined
    class StaticContainer extends js.Object

    object StaticContainer {
      @JSExportStatic
      class A
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: Implementation restriction: cannot export a class or object as static
      |      @JSExportStatic
      |       ^
    """

    """
    @ScalaJSDefined
    class StaticContainer extends js.Object

    object StaticContainer {
      class A {
        @JSExportStatic
        def this(x: Int) = this()
      }
    }
    """ hasErrors
    """
      |newSource1.scala:8: error: Implementation restriction: cannot export a class or object as static
      |        @JSExportStatic
      |         ^
    """
  }

  @Test
  def noExportStaticValTwice: Unit = {
    """
    @ScalaJSDefined
    class StaticContainer extends js.Object

    object StaticContainer {
      @JSExportStatic
      @JSExportStatic("b")
      val a: Int = 1
    }
    """ hasErrors
    """
      |newSource1.scala:8: error: Fields (val or var) cannot be exported as static more than once
      |      @JSExportStatic("b")
      |       ^
    """
  }

  @Test
  def noExportStaticVarTwice: Unit = {
    """
    @ScalaJSDefined
    class StaticContainer extends js.Object

    object StaticContainer {
      @JSExportStatic
      @JSExportStatic("b")
      var a: Int = 1
    }
    """ hasErrors
    """
      |newSource1.scala:8: error: Fields (val or var) cannot be exported as static more than once
      |      @JSExportStatic("b")
      |       ^
    """
  }

  @Test
  def noExportValAsStaticAndTopLevel: Unit = {
    """
    @ScalaJSDefined
    class StaticContainer extends js.Object

    object StaticContainer {
      @JSExportStatic
      @JSExportTopLevel("foo")
      val a: Int = 1
    }
    """ hasErrors
    """
      |newSource1.scala:8: error: Fields (val or var) cannot be exported both as static and at the top-level
      |      @JSExportTopLevel("foo")
      |       ^
    """
  }

  @Test
  def noExportVarAsStaticAndTopLevel: Unit = {
    """
    @ScalaJSDefined
    class StaticContainer extends js.Object

    object StaticContainer {
      @JSExportStatic
      @JSExportTopLevel("foo")
      var a: Int = 1
    }
    """ hasErrors
    """
      |newSource1.scala:8: error: Fields (val or var) cannot be exported both as static and at the top-level
      |      @JSExportTopLevel("foo")
      |       ^
    """
  }

  @Test
  def noExportSetterWithBadSetterType: Unit = {
    """
    @ScalaJSDefined
    class StaticContainer extends js.Object

    object StaticContainer {
      @JSExportStatic
      def a_=(x: Int, y: Int): Unit = ()
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: Exported setters must have exactly one argument
      |      @JSExportStatic
      |       ^
    """
  }

  @Test
  def noExportStaticCollapsingMethods: Unit = {
    """
    @ScalaJSDefined
    class StaticContainer extends js.Object

    object StaticContainer {
      @JSExportStatic
      def foo(x: Int): Int = x

      @JSExportStatic("foo")
      def bar(x: Int): Int = x + 1
    }
    """ hasErrors
    """
      |newSource1.scala:11: error: Cannot disambiguate overloads for exported method bar with types
      |  (x: Int)Int
      |  (x: Int)Int
      |      def bar(x: Int): Int = x + 1
      |          ^
    """
  }

  @Test
  def noExportStaticCollapsingGetters: Unit = {
    """
    @ScalaJSDefined
    class StaticContainer extends js.Object

    object StaticContainer {
      @JSExportStatic
      def foo: Int = 1

      @JSExportStatic("foo")
      def bar: Int = 2
    }
    """ hasErrors
    """
      |newSource1.scala:8: error: Duplicate static getter export with name 'foo'
      |      def foo: Int = 1
      |          ^
    """
  }

  @Test
  def noExportStaticCollapsingSetters: Unit = {
    """
    @ScalaJSDefined
    class StaticContainer extends js.Object

    object StaticContainer {
      @JSExportStatic
      def foo_=(v: Int): Unit = ()

      @JSExportStatic("foo")
      def bar_=(v: Int): Unit = ()
    }
    """ hasErrors
    """
      |newSource1.scala:11: error: Cannot disambiguate overloads for exported method bar_$eq with types
      |  (v: Int)Unit
      |  (v: Int)Unit
      |      def bar_=(v: Int): Unit = ()
      |          ^
    """
  }

  @Test
  def noExportStaticFieldsWithSameName: Unit = {
    """
    @ScalaJSDefined
    class StaticContainer extends js.Object

    object StaticContainer {
      @JSExportStatic
      val a: Int = 1

      @JSExportStatic("a")
      var b: Int = 1
    }
    """ hasErrors
    """
      |newSource1.scala:8: error: Duplicate static export with name 'a': a field may not share its exported name with another field or method
      |      val a: Int = 1
      |          ^
    """
  }

  @Test
  def noExportStaticFieldsAndMethodsWithSameName: Unit = {
    """
    @ScalaJSDefined
    class StaticContainer extends js.Object

    object StaticContainer {
      @JSExportStatic
      val a: Int = 1

      @JSExportStatic("a")
      def b(x: Int): Int = x + 1
    }
    """ hasErrors
    """
      |newSource1.scala:10: error: Duplicate static export with name 'a': a field may not share its exported name with another field or method
      |      @JSExportStatic("a")
      |       ^
    """

    """
    @ScalaJSDefined
    class StaticContainer extends js.Object

    object StaticContainer {
      @JSExportStatic
      def a(x: Int): Int = x + 1

      @JSExportStatic("a")
      val b: Int = 1
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: Duplicate static export with name 'a': a field may not share its exported name with another field or method
      |      @JSExportStatic
      |       ^
    """
  }

  @Test
  def noExportStaticFieldsAndPropertiesWithSameName: Unit = {
    """
    @ScalaJSDefined
    class StaticContainer extends js.Object

    object StaticContainer {
      @JSExportStatic
      val a: Int = 1

      @JSExportStatic("a")
      def b: Int = 2
    }
    """ hasErrors
    """
      |newSource1.scala:10: error: Duplicate static export with name 'a': a field may not share its exported name with another field or method
      |      @JSExportStatic("a")
      |       ^
    """

    """
    @ScalaJSDefined
    class StaticContainer extends js.Object

    object StaticContainer {
      @JSExportStatic
      def a: Int = 1

      @JSExportStatic("a")
      val b: Int = 2
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: Duplicate static export with name 'a': a field may not share its exported name with another field or method
      |      @JSExportStatic
      |       ^
    """
  }

  @Test
  def noExportStaticPropertiesAndMethodsWithSameName: Unit = {
    """
    @ScalaJSDefined
    class StaticContainer extends js.Object

    object StaticContainer {
      @JSExportStatic
      def a: Int = 1

      @JSExportStatic("a")
      def b(x: Int): Int = x + 1
    }
    """ hasErrors
    """
      |newSource1.scala:8: error: Exported property a conflicts with b
      |      def a: Int = 1
      |          ^
    """

    """
    @ScalaJSDefined
    class StaticContainer extends js.Object

    object StaticContainer {
      @JSExportStatic
      def a(x: Int): Int = x + 1

      @JSExportStatic("a")
      def b: Int = 1
    }
    """ hasErrors
    """
      |newSource1.scala:8: error: Exported method a conflicts with b
      |      def a(x: Int): Int = x + 1
      |          ^
    """
  }

  @Test
  def noExportStaticNonStatic: Unit = {
    """
    class A {
      @ScalaJSDefined
      class StaticContainer extends js.Object

      object StaticContainer {
        @JSExportStatic
        def a(): Unit = ()
      }
    }
    """ hasErrors
    """
      |newSource1.scala:8: error: Only a static object whose companion class is a Scala.js-defined JS class may export its members as static.
      |        @JSExportStatic
      |         ^
    """
  }

  @Test
  def noExportStaticInJSModule: Unit = {
    """
    @ScalaJSDefined
    class StaticContainer extends js.Object

    @ScalaJSDefined
    object StaticContainer extends js.Object {
      @JSExportStatic
      def a(): Unit = ()
    }
    """ hasErrors
    """
      |newSource1.scala:8: error: You may not export a method of a subclass of js.Any
      |      @JSExportStatic
      |       ^
    """

    """
    @ScalaJSDefined
    class StaticContainer extends js.Object

    @js.native
    object StaticContainer extends js.Object {
      @JSExportStatic
      def a(): Unit = js.native
    }
    """ hasErrors
    """
      |newSource1.scala:8: error: You may not export a method of a subclass of js.Any
      |      @JSExportStatic
      |       ^
    """
  }

  @Test
  def noExportStaticIfWrongCompanionType: Unit = {
    """
    class StaticContainer

    object StaticContainer {
      @JSExportStatic
      def a(): Unit = ()
    }
    """ hasErrors
    """
      |newSource1.scala:6: error: Only a static object whose companion class is a Scala.js-defined JS class may export its members as static.
      |      @JSExportStatic
      |       ^
    """

    """
    @ScalaJSDefined
    trait StaticContainer extends js.Object

    object StaticContainer {
      @JSExportStatic
      def a(): Unit = ()
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: Only a static object whose companion class is a Scala.js-defined JS class may export its members as static.
      |      @JSExportStatic
      |       ^
    """

    """
    @js.native
    class StaticContainer extends js.Object

    object StaticContainer {
      @JSExportStatic
      def a(): Unit = ()
    }
    """ hasErrors
    """
      |newSource1.scala:7: error: Only a static object whose companion class is a Scala.js-defined JS class may export its members as static.
      |      @JSExportStatic
      |       ^
    """
  }
}
