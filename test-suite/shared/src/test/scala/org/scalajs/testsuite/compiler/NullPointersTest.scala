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

package org.scalajs.testsuite.compiler

import org.junit.{BeforeClass, Test}
import org.junit.Assert._
import org.junit.Assume._

import org.scalajs.testsuite.utils.AssertThrows.assertThrows
import org.scalajs.testsuite.utils.Platform._

class NullPointersTest {
  import NullPointersTest._

  // Instantiate Tester somewhere, otherwise plenty of tests are moot
  @noinline def keep(x: Any): Unit = ()
  keep(new Tester(0))

  @noinline
  private def nullOf[T >: Null]: T = null

  @inline
  private def inlineNullOf[T >: Null]: T = null

  @noinline
  private def assertNPE[U](body: => U): Unit =
    assertThrows(classOf[NullPointerException], body)

  @noinline
  private def throwIllegalArgAsInt(): Int =
    throw new IllegalArgumentException

  @inline
  private def throwIllegalArgAsIntInline(): Int =
    throw new IllegalArgumentException

  @noinline
  private def throwIllegalArgAsString(): String =
    throw new IllegalArgumentException

  @inline
  private def throwIllegalArgAsStringInline(): String =
    throw new IllegalArgumentException

  @Test def methodCallsWithRegularClasses(): Unit = {
    assertNPE(nullOf[Tester].x)
    assertNPE(inlineNullOf[Tester].x)

    assertNPE(nullOf[Tester].noInlineMethod(1))
    assertNPE(inlineNullOf[Tester].noInlineMethod(1))

    assertNPE(nullOf[Tester].inlineMethod(1))
    assertNPE(inlineNullOf[Tester].inlineMethod(1))

    assertNPE(nullOf[Tester].inlineMethodWithField(1))
    assertNPE(inlineNullOf[Tester].inlineMethodWithField(1))

    assertNPE(nullOf[Tester].toString())
    assertNPE(inlineNullOf[Tester].toString())
    assertNPE(nullOf[AnyRef].toString())
    assertNPE(inlineNullOf[AnyRef].toString())

    assertNPE(nullOf[Tester].synchronized("foo"))
    assertNPE(inlineNullOf[Tester].synchronized("foo"))
    assertNPE(nullOf[AnyRef].synchronized("foo"))
    assertNPE(inlineNullOf[AnyRef].synchronized("foo"))

    assertNPE(nullOf[Tester].getClass())
    assertNPE(inlineNullOf[Tester].getClass())
    assertNPE(nullOf[AnyRef].getClass())
    assertNPE(inlineNullOf[AnyRef].getClass())

    assertNPE(nullOf[Tester].clone())
    assertNPE(inlineNullOf[Tester].clone())

    // NPE takes precedence over evaluating arguments, unlike on the JVM

    if (executingInJVM) {
      assertThrows(classOf[IllegalArgumentException],
          nullOf[Tester].noInlineMethod(throwIllegalArgAsInt()))
    } else {
      assertNPE(nullOf[Tester].noInlineMethod(throwIllegalArgAsInt()))
      assertNPE(nullOf[Tester].noInlineMethod(throwIllegalArgAsIntInline()))
      assertNPE(inlineNullOf[Tester].noInlineMethod(throwIllegalArgAsInt()))
      assertNPE(inlineNullOf[Tester].noInlineMethod(throwIllegalArgAsIntInline()))

      assertNPE(nullOf[Tester].inlineMethod(throwIllegalArgAsInt()))
      assertNPE(nullOf[Tester].inlineMethod(throwIllegalArgAsIntInline()))
      assertNPE(inlineNullOf[Tester].inlineMethod(throwIllegalArgAsInt()))
      assertNPE(inlineNullOf[Tester].inlineMethod(throwIllegalArgAsIntInline()))
    }
  }

  @Test def methodCallsWithHijackedClasses(): Unit = {
    assertNPE(nullOf[Integer].intValue())
    assertNPE(inlineNullOf[Integer].intValue())
    assertNPE(nullOf[Integer].toString())
    assertNPE(inlineNullOf[Integer].toString())
    assertNPE(nullOf[Integer].synchronized("foo"))
    assertNPE(inlineNullOf[Integer].synchronized("foo"))
    assertNPE(nullOf[Integer].getClass())
    assertNPE(inlineNullOf[Integer].getClass())

    assertNPE(nullOf[Character].charValue())
    assertNPE(inlineNullOf[Character].charValue())
    assertNPE(nullOf[Character].toString())
    assertNPE(inlineNullOf[Character].toString())

    assertNPE(nullOf[String].length())
    assertNPE(inlineNullOf[String].length())
    assertNPE(nullOf[String].charAt(3))
    assertNPE(inlineNullOf[String].charAt(3))
    assertNPE(nullOf[String].toString())
    assertNPE(inlineNullOf[String].toString())
    assertNPE(nullOf[String].concat("foo"))
    assertNPE(inlineNullOf[String].concat("foo"))

    // NPE takes precedence over evaluating arguments, unlike on the JVM

    if (executingInJVM) {
      assertThrows(classOf[IllegalArgumentException],
          nullOf[String].substring(throwIllegalArgAsInt()))
    } else {
      // The implementation of charAt in Scala.js is a bit special, so it deserves its own test
      assertNPE(nullOf[String].charAt(throwIllegalArgAsInt()))
      assertNPE(nullOf[String].charAt(throwIllegalArgAsIntInline()))
      assertNPE(inlineNullOf[String].charAt(throwIllegalArgAsInt()))
      assertNPE(inlineNullOf[String].charAt(throwIllegalArgAsIntInline()))

      // regular no-inline method
      assertNPE(nullOf[String].compareTo(throwIllegalArgAsString()))
      assertNPE(nullOf[String].compareTo(throwIllegalArgAsStringInline()))
      assertNPE(inlineNullOf[String].compareTo(throwIllegalArgAsString()))
      assertNPE(inlineNullOf[String].compareTo(throwIllegalArgAsStringInline()))

      // regular inline method
      assertNPE(nullOf[String].substring(throwIllegalArgAsInt()))
      assertNPE(nullOf[String].substring(throwIllegalArgAsIntInline()))
      assertNPE(inlineNullOf[String].substring(throwIllegalArgAsInt()))
      assertNPE(inlineNullOf[String].substring(throwIllegalArgAsIntInline()))
    }
  }

  @Test def arrays(): Unit = {
    assertNPE(nullOf[Array[Int]].length)
    assertNPE(nullOf[Array[Char]].length)
    assertNPE(nullOf[Array[AnyRef]].length)
    assertNPE(nullOf[Array[String]].length)
    assertNPE(nullOf[Array[List[Any]]].length)

    assertNPE(inlineNullOf[Array[Int]].length)
    assertNPE(inlineNullOf[Array[Char]].length)
    assertNPE(inlineNullOf[Array[AnyRef]].length)
    assertNPE(inlineNullOf[Array[String]].length)
    assertNPE(inlineNullOf[Array[List[Any]]].length)

    assertNPE(nullOf[Array[Int]](5))
    assertNPE(nullOf[Array[Char]](5))
    assertNPE(nullOf[Array[AnyRef]](5))
    assertNPE(nullOf[Array[String]](5))
    assertNPE(nullOf[Array[List[Any]]](5))

    assertNPE(inlineNullOf[Array[Int]](5))
    assertNPE(inlineNullOf[Array[Char]](5))
    assertNPE(inlineNullOf[Array[AnyRef]](5))
    assertNPE(inlineNullOf[Array[String]](5))
    assertNPE(inlineNullOf[Array[List[Any]]](5))

    assertNPE(nullOf[Array[Int]](-5))
    assertNPE(nullOf[Array[Char]](-5))
    assertNPE(nullOf[Array[AnyRef]](-5))
    assertNPE(nullOf[Array[String]](-5))
    assertNPE(nullOf[Array[List[Any]]](-5))

    assertNPE(inlineNullOf[Array[Int]](-5))
    assertNPE(inlineNullOf[Array[Char]](-5))
    assertNPE(inlineNullOf[Array[AnyRef]](-5))
    assertNPE(inlineNullOf[Array[String]](-5))
    assertNPE(inlineNullOf[Array[List[Any]]](-5))

    assertNPE(nullOf[Array[Int]](5) = 1)
    assertNPE(nullOf[Array[Char]](5) = 'A')
    assertNPE(nullOf[Array[AnyRef]](5) = None)
    assertNPE(nullOf[Array[String]](5) = "foo")
    assertNPE(nullOf[Array[List[Any]]](5) = List(1))

    assertNPE(inlineNullOf[Array[Int]](5) = 1)
    assertNPE(inlineNullOf[Array[Char]](5) = 'A')
    assertNPE(inlineNullOf[Array[AnyRef]](5) = None)
    assertNPE(inlineNullOf[Array[String]](5) = "foo")
    assertNPE(inlineNullOf[Array[List[Any]]](5) = List(1))

    assertNPE(nullOf[Array[Int]].clone())
    assertNPE(nullOf[Array[Char]].clone())
    assertNPE(nullOf[Array[AnyRef]].clone())
    assertNPE(nullOf[Array[String]].clone())
    assertNPE(nullOf[Array[List[Any]]].clone())

    assertNPE(inlineNullOf[Array[Int]].clone())
    assertNPE(inlineNullOf[Array[Char]].clone())
    assertNPE(inlineNullOf[Array[AnyRef]].clone())
    assertNPE(inlineNullOf[Array[String]].clone())
    assertNPE(inlineNullOf[Array[List[Any]]].clone())
  }

  @Test def genericArrays(): Unit = {
    // Tests for the intrinsics for ScalaRunTime.array_{apply,update,select}.

    @inline def testGeneric[T](array: Array[T], value: T): Unit = {
      assertNPE(array.length)
      assertNPE(array(1))
      assertNPE(array(1) = value)
      assertNPE(array(-1))
      assertNPE(array(-1) = value)

      assertThrows(classOf[IllegalArgumentException], array(throwIllegalArgAsInt()))
      assertThrows(classOf[IllegalArgumentException], array(throwIllegalArgAsIntInline()))

      assertThrows(classOf[IllegalArgumentException], array(throwIllegalArgAsInt()) = value)
      assertThrows(classOf[IllegalArgumentException], array(throwIllegalArgAsIntInline()) = value)

      assertThrows(classOf[IllegalArgumentException], array(1) = (throw new IllegalArgumentException()))
    }

    @noinline def testNoInline[T](array: Array[T], value: T): Unit = {
      testGeneric(array, value)
    }

    @inline def test[T](array: Array[T], value: T): Unit = {
      testNoInline(array, value)
      testGeneric(array, value)
    }

    val nullArrayRef = nullOf[Array[AnyRef]]
    test(nullArrayRef, List(1))
    test(inlineNullOf[Array[AnyRef]], List(1))

    val nullArrayInt = nullOf[Array[Int]]
    test(nullArrayInt, 1)
    test(inlineNullOf[Array[Int]], 1)
  }

  @Test def throwNull(): Unit = {
    assertNPE(throw null)
    assertNPE(throw (null: Throwable))
    assertNPE(throw (null: IllegalArgumentException))

    @noinline def nullThrowable(): Throwable = null
    @noinline def nullIllegalArg(): IllegalArgumentException = null

    assertNPE(throw nullThrowable())
    assertNPE(throw nullIllegalArg())
  }
}

object NullPointersTest {
  @BeforeClass
  def beforeClass(): Unit = {
    assumeTrue("assuming compliant null pointer checks", hasCompliantNullPointers)
  }

  class Tester(val x: Int) extends java.lang.Cloneable {
    @noinline def noInlineMethod(a: Int): Int = a

    @inline def inlineMethod(a: Int): Int = a

    @inline def inlineMethodWithField(a: Int): Int = a + x

    @inline override def clone(): Tester =
      super.clone().asInstanceOf[Tester]
  }
}
