package org.scalajs.core.compiler.test

import org.scalajs.core.compiler.test.util._
import org.junit.Test

// scalastyle:off line.size.limit

class JSDynamicLiteralTest extends DirectTest with TestHelpers {

  override def preamble: String =
    """import scala.scalajs.js.Dynamic.{ literal => lit }
    """

  @Test
  def callApplyOnly: Unit = {

    // selectDynamic (with any name)
    expr"""
    lit.helloWorld
    """.fails() // Scala error, no string checking due to versions

    // applyDynamicNamed with wrong method name
    expr"""
    lit.helloWorld(a = "a")
    """ hasErrors
    """
      |newSource1.scala:3: error: js.Dynamic.literal does not have a method named helloWorld
      |    lit.helloWorld(a = "a")
      |                  ^
    """

    // applyDynamic with wrong method name
    expr"""
    lit.helloWorld("a" -> "a")
    """ hasErrors
    """
      |newSource1.scala:3: error: js.Dynamic.literal does not have a method named helloWorld
      |    lit.helloWorld("a" -> "a")
      |                  ^
    """

  }

  @Test
  def goodTypesOnly: Unit = {

    // Bad value type (applyDynamic)
    """
    class A {
      val x = new Object()
      def foo = lit("a" -> x)
    }
    """.fails()

    // Bad key type (applyDynamic)
    """
    class A {
      val x = Seq()
      def foo = lit(x -> "a")
    }
    """.fails()

    // Bad value type (applyDynamicNamed)
    """
    class A {
      val x = new Object()
      def foo = lit(a = x)
    }
    """.fails()

  }

  @Test
  def noNonLiteralMethodName: Unit = {

    // applyDynamicNamed
    """
    class A {
      val x = "string"
      def foo = lit.applyDynamicNamed(x)()
    }
    """ hasErrors
    """
      |newSource1.scala:5: error: js.Dynamic.literal.applyDynamicNamed may not be called directly
      |      def foo = lit.applyDynamicNamed(x)()
      |                                        ^
    """

    // applyDynamic
    """
    class A {
      val x = "string"
      def foo = lit.applyDynamic(x)()
    }
    """ hasErrors
    """
      |newSource1.scala:5: error: js.Dynamic.literal.applyDynamic may not be called directly
      |      def foo = lit.applyDynamic(x)()
      |                                   ^
    """

  }

  @Test
  def keyDuplicationWarning: Unit = {

    // detects duplicate named keys
    expr"""
    lit(a = "1", b = "2", a = "3")
    """ hasWarns
    """
      |newSource1.scala:3: warning: Duplicate keys in object literal: "a" defined 2 times. Only the last occurrence is assigned.
      |    lit(a = "1", b = "2", a = "3")
      |       ^
    """

    // detects duplicate named keys (check the arrow indentation)
    expr"""
    lit(aaa = "1", b = "2", aaa = "3")
    """ hasWarns
    """
      |newSource1.scala:3: warning: Duplicate keys in object literal: "aaa" defined 2 times. Only the last occurrence is assigned.
      |    lit(aaa = "1", b = "2", aaa = "3")
      |       ^
    """

    // detects duplicate named keys (check the arrow indentation)
    expr"""
    lit(aaa = "1",
        bb = "2",
        bb = "3")
    """ hasWarns
    """
      |newSource1.scala:3: warning: Duplicate keys in object literal: "bb" defined 2 times. Only the last occurrence is assigned.
      |    lit(aaa = "1",
      |       ^
    """

    // detects duplicate named keys (check the arrow indentation)
    expr"""
    lit(aaa = "1",
        b = "2",
        aaa = "3")
    """ hasWarns
    """
      |newSource1.scala:3: warning: Duplicate keys in object literal: "aaa" defined 2 times. Only the last occurrence is assigned.
      |    lit(aaa = "1",
      |       ^
    """

    // detects triplicated named keys
    expr"""
    lit(a = "1", a = "2", a = "3")
    """ hasWarns
    """
      |newSource1.scala:3: warning: Duplicate keys in object literal: "a" defined 3 times. Only the last occurrence is assigned.
      |    lit(a = "1", a = "2", a = "3")
      |       ^
    """

    // detects two different duplicates named keys
    expr"""
    lit(a = "1", b = "2", a = "3", b = "4", c = "5", c = "6", c = "7")
    """ hasWarns
    """
      |newSource1.scala:3: warning: Duplicate keys in object literal: "a" defined 2 times, "b" defined 2 times, "c" defined 3 times. Only the last occurrence is assigned.
      |    lit(a = "1", b = "2", a = "3", b = "4", c = "5", c = "6", c = "7")
      |       ^
    """

    // detects duplicate keys when represented with arrows
    expr"""
    lit("a" -> "1", "b" -> "2", "a" -> "3")
    """ hasWarns
    """
      |newSource1.scala:3: warning: Duplicate keys in object literal: "a" defined 2 times. Only the last occurrence is assigned.
      |    lit("a" -> "1", "b" -> "2", "a" -> "3")
      |       ^
    """

    // detects duplicate keys when represented with tuples
    expr"""
    lit(("a", "1"), ("b", "2"), ("a", "3"))
    """ hasWarns
    """
      |newSource1.scala:3: warning: Duplicate keys in object literal: "a" defined 2 times. Only the last occurrence is assigned.
      |    lit(("a", "1"), ("b", "2"), ("a", "3"))
      |       ^
    """

    // detects duplicate keys when represented with mixed tuples and arrows
    expr"""
    lit("a" -> "1", ("b", "2"), ("a", "3"))
    """ hasWarns
    """
      |newSource1.scala:3: warning: Duplicate keys in object literal: "a" defined 2 times. Only the last occurrence is assigned.
      |    lit("a" -> "1", ("b", "2"), ("a", "3"))
      |       ^
    """

    // should not warn if the key is not literal
    expr"""
    val a = "x"
    lit("a" -> "1", a -> "2", a -> "3")
    """ hasWarns
    """
    """

    // should not warn if the key/value pairs are not literal
    """
    class A {
      val tup = "x" -> lit()
      def foo = lit(tup, tup)
    }
    """ hasWarns
    """
    """

    // should warn only for the literal keys when in
    // the presence of non literal keys
    """
    class A {
      val b = "b"
      val tup = b -> lit()
      lit("a" -> "2", tup, ("a", "3"), b -> "5", tup, b -> "6")
    }
    """ hasWarns
    """
      |newSource1.scala:6: warning: Duplicate keys in object literal: "a" defined 2 times. Only the last occurrence is assigned.
      |      lit("a" -> "2", tup, ("a", "3"), b -> "5", tup, b -> "6")
      |         ^
    """
  }

}
