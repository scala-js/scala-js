/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.lang

import org.junit.Test
import org.junit.Assert._

import org.scalajs.testsuite.utils.AssertThrows._
import org.scalajs.testsuite.utils.Platform.executingInJVM

class StringBufferTest {

  def newBuf: java.lang.StringBuffer =
    new java.lang.StringBuffer

  def initBuf(str: String): java.lang.StringBuffer =
    new java.lang.StringBuffer(str)

  @Test def append(): Unit = {
    assertEquals("asdf", newBuf.append("asdf").toString)
    assertEquals("null", newBuf.append(null: AnyRef).toString)
    assertEquals("null", newBuf.append(null: String).toString)
    assertEquals("nu", newBuf.append(null: CharSequence,0,2).toString)
    assertEquals("true", newBuf.append(true).toString)
    assertEquals("a", newBuf.append('a').toString)
    assertEquals("abcd", newBuf.append(Array('a','b','c','d')).toString)
    assertEquals("bc", newBuf.append(Array('a','b','c','d'), 1, 2).toString)
    assertEquals("4", newBuf.append(4.toByte).toString)
    assertEquals("304", newBuf.append(304.toShort).toString)
    assertEquals("100000", newBuf.append(100000).toString)
    assertEquals("2.5", newBuf.append(2.5f).toString)
    assertEquals("3.5", newBuf.append(3.5).toString)
  }

  @Test def insert(): Unit = {
    assertEquals("asdf", newBuf.insert(0, "asdf").toString)
    assertEquals("null", newBuf.insert(0, null: AnyRef).toString)
    assertEquals("null", newBuf.insert(0, null: String).toString)
    assertEquals("nu", newBuf.insert(0, null: CharSequence,0,2).toString)
    assertEquals("true", newBuf.insert(0, true).toString)
    assertEquals("a", newBuf.insert(0, 'a').toString)
    assertEquals("abcd", newBuf.insert(0, Array('a','b','c','d')).toString)
    assertEquals("bc", newBuf.insert(0, Array('a','b','c','d'), 1, 2).toString)
    assertEquals("4", newBuf.insert(0, 4.toByte).toString)
    assertEquals("304", newBuf.insert(0, 304.toShort).toString)
    assertEquals("100000", newBuf.insert(0, 100000).toString)
    assertEquals("2.5", newBuf.insert(0, 2.5f).toString)
    assertEquals("3.5", newBuf.insert(0, 3.5).toString)

    assertEquals("abcdef", initBuf("adef").insert(1, "bc").toString)
    assertEquals("abcdef", initBuf("abcd").insert(4, "ef").toString)
    assertEquals("abcdef", initBuf("adef").insert(1, Array('b','c')).toString)
    assertEquals("abcdef", initBuf("adef").insert(1, initBuf("bc")).toString)
    assertEquals("abcdef", initBuf("abef").insert(2, Array('a','b','c','d','e'), 2, 2).toString)
    assertEquals("abcdef", initBuf("abef").insert(2, initBuf("abcde"), 2, 4).toString)

    expectThrows(classOf[StringIndexOutOfBoundsException],
        initBuf("abcd").insert(5, "whatever"))
    expectThrows(classOf[StringIndexOutOfBoundsException],
        initBuf("abcd").insert(-1, "whatever"))
  }

  @Test def deleteCharAt(): Unit = {
    assertEquals("023", initBuf("0123").deleteCharAt(1).toString)
    assertEquals("123", initBuf("0123").deleteCharAt(0).toString)
    assertEquals("012", initBuf("0123").deleteCharAt(3).toString)
    expectThrows(classOf[StringIndexOutOfBoundsException],
        initBuf("0123").deleteCharAt(-1))
    expectThrows(classOf[StringIndexOutOfBoundsException],
        initBuf("0123").deleteCharAt(4))
  }

  @Test def replace(): Unit = {
    assertEquals("0bc3", initBuf("0123").replace(1,3,"bc").toString)
    assertEquals("abcd", initBuf("0123").replace(0,4,"abcd").toString)
    assertEquals("abcd", initBuf("0123").replace(0,10,"abcd").toString)
    assertEquals("012defg", initBuf("0123").replace(3,10,"defg").toString)
    assertEquals("xxxx123", initBuf("0123").replace(0,1,"xxxx").toString)
    assertEquals("0xxxx123", initBuf("0123").replace(1,1,"xxxx").toString)
    assertEquals("0123x", initBuf("0123").replace(4, 5, "x").toString)

    expectThrows(classOf[StringIndexOutOfBoundsException],
        initBuf("0123").replace(-1,3,"x"))
  }

  @Test def setCharAt(): Unit = {
    val buf = newBuf
    buf.append("foobar")

    buf.setCharAt(2, 'x')
    assertEquals("foxbar", buf.toString)

    buf.setCharAt(5, 'h')
    assertEquals("foxbah", buf.toString)

    expectThrows(classOf[StringIndexOutOfBoundsException],
        buf.setCharAt(-1, 'h'))
    expectThrows(classOf[StringIndexOutOfBoundsException],
        buf.setCharAt(6,  'h'))
  }

  @Test def ensureCapacity(): Unit = {
    // test that ensureCapacity is linking
    newBuf.ensureCapacity(10)
  }

  @Test def should_properly_setLength(): Unit = {
    val buf = newBuf
    buf.append("foobar")

    expectThrows(classOf[StringIndexOutOfBoundsException], buf.setLength(-3))

    assertEquals("foo", { buf.setLength(3); buf.toString })
    assertEquals("foo\u0000\u0000\u0000", { buf.setLength(6); buf.toString })
  }

  @Test def appendCodePoint(): Unit = {
    val buf = newBuf
    buf.appendCodePoint(0x61)
    assertEquals("a", buf.toString)
    buf.appendCodePoint(0x10000)
    assertEquals("a\uD800\uDC00", buf.toString)
    buf.append("fixture")
    buf.appendCodePoint(0x00010FFFF)
    assertEquals("a\uD800\uDC00fixture\uDBFF\uDFFF", buf.toString)
  }
}

class StringBuilderTest {

  def newBuilder: java.lang.StringBuilder =
    new java.lang.StringBuilder

  def initBuilder(str: String): java.lang.StringBuilder =
    new java.lang.StringBuilder(str)

  @Test def append(): Unit = {
    assertEquals("asdf", newBuilder.append("asdf").toString)
    assertEquals("null", newBuilder.append(null: AnyRef).toString)
    assertEquals("null", newBuilder.append(null: String).toString)
    assertEquals("nu", newBuilder.append(null: CharSequence,0,2).toString)
    assertEquals("true", newBuilder.append(true).toString)
    assertEquals("a", newBuilder.append('a').toString)
    assertEquals("abcd", newBuilder.append(Array('a','b','c','d')).toString)
    assertEquals("bc", newBuilder.append(Array('a','b','c','d'), 1, 2).toString)
    assertEquals("4", newBuilder.append(4.toByte).toString)
    assertEquals("304", newBuilder.append(304.toShort).toString)
    assertEquals("100000", newBuilder.append(100000).toString)
    assertEquals("2.5", newBuilder.append(2.5f).toString)
    assertEquals("3.5", newBuilder.append(3.5).toString)
  }

  @Test def insert(): Unit = {
    assertEquals("asdf", newBuilder.insert(0, "asdf").toString)
    assertEquals("null", newBuilder.insert(0, null: AnyRef).toString)
    assertEquals("null", newBuilder.insert(0, null: String).toString)
    assertEquals("nu", newBuilder.insert(0, null: CharSequence,0,2).toString)
    assertEquals("true", newBuilder.insert(0, true).toString)
    assertEquals("a", newBuilder.insert(0, 'a').toString)
    assertEquals("abcd", newBuilder.insert(0, Array('a','b','c','d')).toString)
    assertEquals("bc", newBuilder.insert(0, Array('a','b','c','d'), 1, 2).toString)
    assertEquals("4", newBuilder.insert(0, 4.toByte).toString)
    assertEquals("304", newBuilder.insert(0, 304.toShort).toString)
    assertEquals("100000", newBuilder.insert(0, 100000).toString)
    assertEquals("2.5", newBuilder.insert(0, 2.5f).toString)
    assertEquals("3.5", newBuilder.insert(0, 3.5).toString)

    assertEquals("abcdef", initBuilder("adef").insert(1, "bc").toString)
    assertEquals("abcdef", initBuilder("abcd").insert(4, "ef").toString)
    assertEquals("abcdef", initBuilder("adef").insert(1, Array('b','c')).toString)
    assertEquals("abcdef", initBuilder("adef").insert(1, initBuilder("bc")).toString)
    assertEquals("abcdef", initBuilder("abef").insert(2, Array('a','b','c','d','e'), 2, 2).toString)
    assertEquals("abcdef", initBuilder("abef").insert(2, initBuilder("abcde"), 2, 4).toString)

    expectThrows(classOf[StringIndexOutOfBoundsException],
        initBuilder("abcd").insert(5, "whatever"))
    expectThrows(classOf[StringIndexOutOfBoundsException],
        initBuilder("abcd").insert(-1, "whatever"))
  }

  @Test def should_allow_string_interpolation_to_survive_null_and_undefined(): Unit = {
    assertEquals("null", s"${null}")
  }

  @Test def deleteCharAt(): Unit = {
    assertEquals("023", initBuilder("0123").deleteCharAt(1).toString)
    assertEquals("123", initBuilder("0123").deleteCharAt(0).toString)
    assertEquals("012", initBuilder("0123").deleteCharAt(3).toString)
    expectThrows(classOf[StringIndexOutOfBoundsException],
        initBuilder("0123").deleteCharAt(-1))
    expectThrows(classOf[StringIndexOutOfBoundsException],
        initBuilder("0123").deleteCharAt(4))
  }

  @Test def replace(): Unit = {
    assertEquals("0bc3", initBuilder("0123").replace(1,3,"bc").toString)
    assertEquals("abcd", initBuilder("0123").replace(0,4,"abcd").toString)
    assertEquals("abcd", initBuilder("0123").replace(0,10,"abcd").toString)
    assertEquals("012defg", initBuilder("0123").replace(3,10,"defg").toString)
    assertEquals("xxxx123", initBuilder("0123").replace(0,1,"xxxx").toString)
    assertEquals("0xxxx123", initBuilder("0123").replace(1,1,"xxxx").toString)
    assertEquals("0123x", initBuilder("0123").replace(4, 5, "x").toString)

    expectThrows(classOf[StringIndexOutOfBoundsException],
        initBuilder("0123").replace(-1,3,"x"))
  }

  @Test def setCharAt(): Unit = {
    val b = newBuilder
    b.append("foobar")

    b.setCharAt(2, 'x')
    assertEquals("foxbar", b.toString)

    b.setCharAt(5, 'h')
    assertEquals("foxbah", b.toString)

    expectThrows(classOf[StringIndexOutOfBoundsException],
        b.setCharAt(-1, 'h'))
    expectThrows(classOf[StringIndexOutOfBoundsException],
        b.setCharAt(6,  'h'))
  }

  @Test def ensureCapacity(): Unit = {
    // test that ensureCapacity is linking
    newBuilder.ensureCapacity(10)
  }

  @Test def should_properly_setLength(): Unit = {
    val b = newBuilder
    b.append("foobar")

    expectThrows(classOf[StringIndexOutOfBoundsException], b.setLength(-3))

    assertEquals("foo", { b.setLength(3); b.toString })
    assertEquals("foo\u0000\u0000\u0000", { b.setLength(6); b.toString })
  }

  @Test def appendCodePoint(): Unit = {
    val b = newBuilder
    b.appendCodePoint(0x61)
    assertEquals("a", b.toString)
    b.appendCodePoint(0x10000)
    assertEquals("a\uD800\uDC00", b.toString)
    b.append("fixture")
    b.appendCodePoint(0x00010FFFF)
    assertEquals("a\uD800\uDC00fixture\uDBFF\uDFFF", b.toString)
  }
}
