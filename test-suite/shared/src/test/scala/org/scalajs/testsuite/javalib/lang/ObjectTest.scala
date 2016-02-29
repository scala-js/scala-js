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

// scalastyle:off disallow.space.before.token

class ObjectTest {

  @Test def testGetClass(): Unit = {
    class Foo
    val foo = new Foo

    @noinline def fooAny: Any = foo

    assertSame(classOf[Foo], foo.getClass)
    assertSame(classOf[Foo], fooAny.getClass)
  }

  @Test def equals(): Unit = {
    case class XY(x: Int, y: Int)

    val l = List(XY(1, 2), XY(2, 1))
    val xy12 = XY(1, 2)

    assertTrue(l.contains(xy12))
    assertTrue(l.exists(_ == xy12)) // the workaround
  }

  @Test def everything_but_null_should_be_an_Object(): Unit = {
    assertTrue((()             : Any).isInstanceOf[Object])
    assertTrue((true           : Any).isInstanceOf[Object])
    assertTrue(('a'            : Any).isInstanceOf[Object])
    assertTrue((1.toByte       : Any).isInstanceOf[Object])
    assertTrue((658.toShort    : Any).isInstanceOf[Object])
    assertTrue((60000          : Any).isInstanceOf[Object])
    assertTrue((12345678910112L: Any).isInstanceOf[Object])
    assertTrue((6.5f           : Any).isInstanceOf[Object])
    assertTrue((12.4           : Any).isInstanceOf[Object])
    assertTrue((new Object     : Any).isInstanceOf[Object])
    assertTrue(("hello"        : Any).isInstanceOf[Object])
    assertTrue((List(1)        : Any).isInstanceOf[Object])
    assertTrue((Array(1)       : Any).isInstanceOf[Object])
    assertTrue((Array(Nil)     : Any).isInstanceOf[Object])
  }

  @Test def null_should_not_be_an_Object(): Unit = {
    assertFalse((null: Any).isInstanceOf[Object])
  }

  @Test def everything_should_cast_to_Object_successfully_including_null(): Unit = {
    (()             : Any).asInstanceOf[Object]
    (true           : Any).asInstanceOf[Object]
    ('a'            : Any).asInstanceOf[Object]
    (1.toByte       : Any).asInstanceOf[Object]
    (658.toShort    : Any).asInstanceOf[Object]
    (60000          : Any).asInstanceOf[Object]
    (12345678910112L: Any).asInstanceOf[Object]
    (6.5f           : Any).asInstanceOf[Object]
    (12.4           : Any).asInstanceOf[Object]
    (new Object     : Any).asInstanceOf[Object]
    ("hello"        : Any).asInstanceOf[Object]
    (List(1)        : Any).asInstanceOf[Object]
    (Array(1)       : Any).asInstanceOf[Object]
    (Array(Nil)     : Any).asInstanceOf[Object]
    (null           : Any).asInstanceOf[Object]
  }
}
