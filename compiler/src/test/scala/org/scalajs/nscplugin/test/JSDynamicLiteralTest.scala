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
import org.junit.Test

// scalastyle:off line.size.limit

class JSDynamicLiteralTest extends DirectTest with TestHelpers {

  override def preamble: String =
    """import scala.scalajs.js.Dynamic.{ literal => lit }
    """

  @Test
  def callApplyOnly(): Unit = {

    // selectDynamic (with any name)
    expr"""
    lit.helloWorld
    """ hasErrors
    """
      |newSource1.scala:3: error: value selectDynamic is not a member of object scalajs.js.Dynamic.literal
      |error after rewriting to scala.scalajs.js.Dynamic.literal.<selectDynamic: error>("helloWorld")
      |possible cause: maybe a wrong Dynamic method signature?
      |    lit.helloWorld
      |    ^
    """

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
  def goodTypesOnly(): Unit = {

    // Bad value type (applyDynamic)
    """
    class A {
      val x = new Object()
      def foo = lit("a" -> x)
    }
    """ hasErrors
    """
      |newSource1.scala:5: error: type mismatch;
      | found   : Object
      | required: scala.scalajs.js.Any
      |      def foo = lit("a" -> x)
      |                           ^
    """

    // Bad key type (applyDynamic)
    """
    class A {
      val x = Seq()
      def foo = lit(x -> "a")
    }
    """ hasErrors
    """
      |newSource1.scala:5: error: type mismatch;
      | found   : (Seq[Nothing], String)
      | required: (String, scala.scalajs.js.Any)
      |      def foo = lit(x -> "a")
      |                      ^
    """

    // Bad value type (applyDynamicNamed)
    """
    class A {
      val x = new Object()
      def foo = lit(a = x)
    }
    """ hasErrors
    """
      |newSource1.scala:5: error: type mismatch;
      | found   : Object
      | required: scala.scalajs.js.Any
      |error after rewriting to scala.scalajs.js.Dynamic.literal.applyDynamicNamed("apply")(scala.Tuple2("a", x))
      |possible cause: maybe a wrong Dynamic method signature?
      |      def foo = lit(a = x)
      |                        ^
    """

  }

  @Test
  def noNonLiteralMethodName(): Unit = {

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
  def keyDuplicationWarning(): Unit = {
    // detects duplicate named keys
    expr"""
    lit(a = "1", b = "2", a = "3")
    """ hasWarns
    """
      |newSource1.scala:3: warning: Duplicate property "a" shadows a previously defined one
      |    lit(a = "1", b = "2", a = "3")
      |                          ^
    """

    // detects duplicate named keys
    expr"""
    lit(aaa = "1", b = "2", aaa = "3")
    """ hasWarns
    """
      |newSource1.scala:3: warning: Duplicate property "aaa" shadows a previously defined one
      |    lit(aaa = "1", b = "2", aaa = "3")
      |                            ^
    """

    // detects duplicate named keys
    expr"""
    lit(aaa = "1",
        bb = "2",
        bb = "3")
    """ hasWarns
    """
      |newSource1.scala:5: warning: Duplicate property "bb" shadows a previously defined one
      |        bb = "3")
      |        ^
    """

    // detects duplicate named keys
    expr"""
    lit(aaa = "1",
        b = "2",
        aaa = "3")
    """ hasWarns
    """
      |newSource1.scala:5: warning: Duplicate property "aaa" shadows a previously defined one
      |        aaa = "3")
      |        ^
    """

    // detects triplicated named keys
    expr"""
    lit(a = "1", a = "2", a = "3")
    """ hasWarns
    """
      |newSource1.scala:3: warning: Duplicate property "a" shadows a previously defined one
      |    lit(a = "1", a = "2", a = "3")
      |                 ^
      |newSource1.scala:3: warning: Duplicate property "a" shadows a previously defined one
      |    lit(a = "1", a = "2", a = "3")
      |                          ^
    """

    // detects two different duplicates named keys
    expr"""
    lit(a = "1", b = "2", a = "3", b = "4", c = "5", c = "6", c = "7")
    """ hasWarns
    """
      |newSource1.scala:3: warning: Duplicate property "a" shadows a previously defined one
      |    lit(a = "1", b = "2", a = "3", b = "4", c = "5", c = "6", c = "7")
      |                          ^
      |newSource1.scala:3: warning: Duplicate property "b" shadows a previously defined one
      |    lit(a = "1", b = "2", a = "3", b = "4", c = "5", c = "6", c = "7")
      |                                   ^
      |newSource1.scala:3: warning: Duplicate property "c" shadows a previously defined one
      |    lit(a = "1", b = "2", a = "3", b = "4", c = "5", c = "6", c = "7")
      |                                                     ^
      |newSource1.scala:3: warning: Duplicate property "c" shadows a previously defined one
      |    lit(a = "1", b = "2", a = "3", b = "4", c = "5", c = "6", c = "7")
      |                                                              ^
    """

    // detects duplicate keys when represented with arrows
    expr"""
    lit("a" -> "1", "b" -> "2", "a" -> "3")
    """ hasWarns
    """
      |newSource1.scala:3: warning: Duplicate property "a" shadows a previously defined one
      |    lit("a" -> "1", "b" -> "2", "a" -> "3")
      |                                ^
    """

    // detects duplicate keys when represented with tuples
    expr"""
    lit(("a", "1"), ("b", "2"), ("a", "3"))
    """ hasWarns
    """
      |newSource1.scala:3: warning: Duplicate property "a" shadows a previously defined one
      |    lit(("a", "1"), ("b", "2"), ("a", "3"))
      |                                 ^
    """

    // detects duplicate keys when represented with mixed tuples and arrows
    expr"""
    lit("a" -> "1", ("b", "2"), ("a", "3"))
    """ hasWarns
    """
      |newSource1.scala:3: warning: Duplicate property "a" shadows a previously defined one
      |    lit("a" -> "1", ("b", "2"), ("a", "3"))
      |                                 ^
    """

    // should not warn if the key is not literal
    expr"""
    val a = "x"
    lit("a" -> "1", a -> "2", a -> "3")
    """.hasNoWarns()

    // should not warn if the key/value pairs are not literal
    """
    class A {
      val tup = "x" -> lit()
      def foo = lit(tup, tup)
    }
    """.hasNoWarns()

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
      |newSource1.scala:6: warning: Duplicate property "a" shadows a previously defined one
      |      lit("a" -> "2", tup, ("a", "3"), b -> "5", tup, b -> "6")
      |                            ^
    """
  }

}
