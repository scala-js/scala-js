package org.scalajs.core.compiler.test

import org.scalajs.core.compiler.test.util._

import org.junit._

// scalastyle:off line.size.limit

class InternalAnnotationsTest extends DirectTest with TestHelpers {

  override def preamble: String =
    "import scala.scalajs.js, js.annotation._"

  @Test
  def exposedJSMember: Unit = {
    test("ExposedJSMember")
  }

  @Test
  def jsFullName: Unit = {
    test("JSFullName(\"abc\")")
  }

  @Test
  def rawJSType: Unit = {
    test("RawJSType")
  }

  @Test
  def sjsDefinedAnonymousClass: Unit = {
    test("SJSDefinedAnonymousClass")
  }

  private def test(annotation: String): Unit = {
    s"""
       @$annotation trait A
       @$annotation class B {
         @$annotation val a = ???
         @$annotation var b = ???
         @$annotation def c = ???
         def d(@$annotation i: Int) = ???
         @$annotation class X
         @$annotation trait Y
       }
    """ hasErrors
    s"""
       |newSource1.scala:2: error: scala.scalajs.js.annotation.$annotation is for compiler internal use only. Do not use it yourself.
       |       @$annotation trait A
       |        ^
       |newSource1.scala:3: error: scala.scalajs.js.annotation.$annotation is for compiler internal use only. Do not use it yourself.
       |       @$annotation class B {
       |        ^
       |newSource1.scala:4: error: scala.scalajs.js.annotation.$annotation is for compiler internal use only. Do not use it yourself.
       |         @$annotation val a = ???
       |          ^
       |newSource1.scala:5: error: scala.scalajs.js.annotation.$annotation is for compiler internal use only. Do not use it yourself.
       |         @$annotation var b = ???
       |          ^
       |newSource1.scala:6: error: scala.scalajs.js.annotation.$annotation is for compiler internal use only. Do not use it yourself.
       |         @$annotation def c = ???
       |          ^
       |newSource1.scala:7: error: scala.scalajs.js.annotation.$annotation is for compiler internal use only. Do not use it yourself.
       |         def d(@$annotation i: Int) = ???
       |                ^
       |newSource1.scala:8: error: scala.scalajs.js.annotation.$annotation is for compiler internal use only. Do not use it yourself.
       |         @$annotation class X
       |          ^
       |newSource1.scala:9: error: scala.scalajs.js.annotation.$annotation is for compiler internal use only. Do not use it yourself.
       |         @$annotation trait Y
       |          ^
    """
  }
}
