/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2016, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.compiler

import java.util.Comparator
import java.util.concurrent.Callable

import org.junit.Assert._
import org.junit.Assume._
import org.junit.Test

class SAMTest {

  import SAMTest._

  @Test def javaCallable(): Unit = {
    val c1: Callable[Int] = () => 4
    assertEquals(4, c1.call())

    val c2: Callable[Char] = () => 'A'
    assertEquals('A', c2.call())

    val c3: Callable[String] = () => "hello"
    assertEquals("hello", c3.call())

    val c4: Callable[VC] = () => new VC(5)
    assertEquals(new VC(5), c4.call())
  }

  @Test def specialResultTypes(): Unit = {
    val f1: SAMReturningChar = () => 'A'
    assertEquals('A', f1.call())

    val f2: SAMReturningVC = () => new VC(5)
    assertEquals(new VC(5), f2.call())
  }

  @Test def javaComparator(): Unit = {
    val c1: Comparator[Int] = (a, b) => b.compareTo(a)
    assertTrue(c1.compare(5, 7) > 0)

    val c2: Comparator[Char] = (a, b) => b.compareTo(a)
    assertTrue(c2.compare('C', 'E') > 0)

    val c3: Comparator[String] = (a, b) => b.compareTo(a)
    assertTrue(c3.compare("hello", "world") > 0)

    val c4: Comparator[VC] = (a, b) => b.x.compareTo(a.x)
    assertTrue(c4.compare(new VC(5), new VC(7)) > 0)
  }

  @Test def samHasDefaultMethod(): Unit = {
    val c1: Comparator[Int] = (a, b) => b.compareTo(a)
    assertTrue(c1.reversed().compare(5, 7) < 0)

    val c2: Comparator[Char] = (a, b) => b.compareTo(a)
    assertTrue(c2.reversed().compare('C', 'E') < 0)

    val c3: Comparator[String] = (a, b) => b.compareTo(a)
    assertTrue(c3.reversed().compare("hello", "world") < 0)

    val c4: Comparator[VC] = (a, b) => b.x.compareTo(a.x)
    assertTrue(c4.reversed().compare(new VC(5), new VC(7)) < 0)
  }

  @Test def specialParamTypes(): Unit = {
    val f1: SAMTakingChar = c => c.toInt + 3
    assertEquals(68, f1.call('A'))

    val f2: SAMTakingVC = vc => vc.x + 3
    assertEquals(8, f2.call(new VC(5)))
  }

  @Test def nonLFMCapableSAM(): Unit = {
    val fNonLMF: NonLMFCapableSAM = x => "a"
    assertEquals("a", fNonLMF(1))
    assertTrue(fNonLMF.isInstanceOf[C])
  }

}

object SAMTest {
  class VC(val x: Int) extends AnyVal

  trait SAMReturningChar {
    def call(): Char
  }

  trait SAMReturningVC {
    def call(): VC
  }

  trait SAMTakingChar {
    def call(c: Char): Int
  }

  trait SAMTakingVC {
    def call(vc: VC): Int
  }

  class C
  trait A extends C
  trait B extends A
  trait NonLMFCapableSAM extends B {
    def apply(x: Int): String
  }

  implicit class ComparatorCompat[A](val c: Comparator[A]) extends AnyVal {
    def reversed(): Comparator[A] = {
      assumeTrue("Comparator.reversed() is not available on this JDK", false)
      ???
    }
  }
}
