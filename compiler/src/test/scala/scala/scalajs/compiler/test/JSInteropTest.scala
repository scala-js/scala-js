package scala.scalajs.compiler.test

import scala.scalajs.compiler.test.util._

import org.junit.Test
import org.junit.Ignore

class JSInteropTest extends DirectTest with TestHelpers {

  override def preamble =
    """import scala.scalajs.js
    """

  @Test
  def noInnerClassTraitObject: Unit = {

    val objs = List("class", "trait", "object")

    for {
      outer <- objs
      inner <- objs
    } yield {
      s"""
      $outer A extends js.Object {
        $inner A
      }
      """ hasErrors
      s"""
        |newSource1.scala:4: error: Traits, classes and objects extending js.Any may not have inner traits, classes or objects
        |        $inner A
        |         ${" " * inner.length}^
      """
    }

  }

  @Test
  def noBadSetters = {

    """
    class A extends js.Object {
      def foo_=(x: Int) = x
    }
    """ hasErrors
    """
      |newSource1.scala:4: error: Setters that do not return Unit are not allowed in types extending js.Any
      |      def foo_=(x: Int) = x
      |          ^
    """

  }

  @Test
  def onlyJSRawTraits = {

    """
    trait A
    class B extends js.Object with A
    """ hasErrors
    """
      |newSource1.scala:4: error: B extends A which does not extend js.Any.
      |    class B extends js.Object with A
      |          ^
    """

  }

  @Test
  def noAnonymousClass = {

    """
    class A {
      val x = new js.Object {
        def a: Int = ???
      }
    }
    """ hasErrors
    """
      |newSource1.scala:4: error: Anonymous classes may not extend js.Any
      |      val x = new js.Object {
      |                  ^
    """

  }

  @Test
  def noCaseClassObject = {

    """
    case class A(x: Int) extends js.Object
    """ hasErrors
    """
      |newSource1.scala:3: error: Classes and objects extending js.Any may not have a case modifier
      |    case class A(x: Int) extends js.Object
      |               ^
    """

    """
    case object B extends js.Object
    """ hasErrors
    """
      |newSource1.scala:3: error: Classes and objects extending js.Any may not have a case modifier
      |    case object B extends js.Object
      |                ^
    """

  }

  @Test
  def notNested: Unit = {

    val outers = List("class", "trait")
    val inners = List("trait", "class", "object")

    for {
      outer <- outers
      inner <- inners
    } yield {

      val errTrg = if (inner == "object") "Objects" else "Classes"

      s"""
      $outer A {
        $inner Inner extends js.Object
      }
      """ hasErrors
      s"""
        |newSource1.scala:4: error: $errTrg extending js.Any may not be defined inside a class or trait
        |        $inner Inner extends js.Object
        |         ${" " * inner.length}^
      """
    }

  }

  @Test
  def noGlobalScopeClass = {

    """
    class A extends js.GlobalScope
    """ hasErrors
    """
      |newSource1.scala:3: error: Only objects may extend js.GlobalScope
      |    class A extends js.GlobalScope
      |          ^
    """

    """
    trait A extends js.GlobalScope
    """ hasErrors
    """
      |newSource1.scala:3: error: Only objects may extend js.GlobalScope
      |    trait A extends js.GlobalScope
      |          ^
    """

  }

  @Test
  def noLocalClass = {

    """
    object A {
      def a = {
        class B extends js.Object
      }
    }
    """ hasErrors
    """
      |newSource1.scala:5: error: Local classes and objects may not extend js.Any
      |        class B extends js.Object
      |              ^
    """

  }

  @Test
  def noLocalObject = {

    """
    object A {
      def a = {
        object B extends js.Object
      }
    }
    """ hasErrors
    """
      |newSource1.scala:5: error: Local classes and objects may not extend js.Any
      |        object B extends js.Object
      |               ^
    """

  }

}
