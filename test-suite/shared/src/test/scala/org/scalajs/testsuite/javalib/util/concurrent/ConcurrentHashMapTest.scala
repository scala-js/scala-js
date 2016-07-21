/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.util.concurrent

import java.{util => ju}

import org.junit.Assert._
import org.junit.Test
import org.scalajs.testsuite.javalib.util.MapTest
import org.scalajs.testsuite.utils.AssertThrows._

import scala.collection.JavaConversions._
import scala.language.implicitConversions
import scala.reflect.ClassTag

class ConcurrentHashMapTest extends MapTest {

  def factory: ConcurrentHashMapFactory = new ConcurrentHashMapFactory

  @Test def `should give proper Enumerator over elements`(): Unit = {
    val chm = factory.empty[String, String]

    chm.put("ONE", "one")
    val elements = chm.elements
    assertTrue(elements.hasNext)
    assertEquals("one", elements.nextElement)
    assertFalse(elements.hasNext)
  }

  @Test def `should replace contained items`(): Unit = {
    val chm = factory.empty[String, String]

    chm.put("ONE", "one")
    assertEquals("one", chm.replace("ONE", "two"))
    expectThrows(classOf[NullPointerException], chm.replace("ONE", null))
    expectThrows(classOf[NullPointerException], chm.replace(null, "one"))
    assertEquals("two", chm.get("ONE"))

    assertFalse(chm.replace("ONE", "one", "two"))
    expectThrows(classOf[NullPointerException], chm.replace(null, "two", "one"))
    expectThrows(classOf[NullPointerException], chm.replace("ONE", null, "one"))
    expectThrows(classOf[NullPointerException], chm.replace("ONE", "two", null))

    assertTrue(chm.replace("ONE", "two", "one"))
    assertEquals("one", chm.get("ONE"))
  }

  @Test def testPutIfAbsent_issue_2539(): Unit = {
    val chm = factory.empty[String, String]
    assertNull(chm.putIfAbsent("abc", "def"))
    assertEquals("def", chm.get("abc"))
    assertNull(chm.putIfAbsent("123", "456"))
    assertEquals("456", chm.get("123"))
    assertEquals("def", chm.putIfAbsent("abc", "def"))
    assertEquals("def", chm.putIfAbsent("abc", "ghi"))
    assertEquals("456", chm.putIfAbsent("123", "789"))
    assertEquals("def", chm.putIfAbsent("abc", "jkl"))
  }
}

object ConcurrentHashMapFactory extends ConcurrentHashMapFactory {
  def allFactories: Iterator[ConcurrentHashMapFactory] =
    Iterator(ConcurrentHashMapFactory)
}

class ConcurrentHashMapFactory extends ConcurrentMapFactory {
  def implementationName: String =
    "java.util.concurrent.ConcurrentHashMap"

  override def empty[K: ClassTag, V: ClassTag]: ju.concurrent.ConcurrentHashMap[K, V] =
    new ju.concurrent.ConcurrentHashMap[K, V]

  def allowsNullKeys: Boolean = false

  def allowsNullValues: Boolean = false
}
