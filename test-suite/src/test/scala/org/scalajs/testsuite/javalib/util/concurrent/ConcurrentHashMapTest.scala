/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.util.concurrent

import scala.language.implicitConversions

import scala.collection.JavaConversions._

import org.scalajs.testsuite.javalib.util.MapTest

import java.{util => ju}

object ConcurrentHashMapTest extends ConcurrentHashMapTest(new ConcurrentHashMapFactory)

class ConcurrentHashMapTest[F <: ConcurrentHashMapFactory](protected val mapFactory: F)
  extends MapTest {

  def testApi(): Unit = {
    testMapApi(mapFactory)

    it("should give proper Enumerator over elements") {
      val chm = mapFactory.empty[String, String]

      chm.put("ONE", "one")
      val elements = chm.elements
      expect(elements.hasNext).toBeTruthy
      expect(elements.nextElement).toEqual("one")
      expect(elements.hasNext).toBeFalsy
    }

    it("should replace contained items") {
      val chm = mapFactory.empty[String, String]

      chm.put("ONE", "one")
      expect(chm.replace("ONE", "two")).toEqual("one")
      expectThrows[NullPointerException](chm.replace("ONE", null))
      expectThrows[NullPointerException](chm.replace(null, "one"))
      expect(chm.get("ONE")).toEqual("two")

      expect(chm.replace("ONE", "one", "two")).toBeFalsy
      expectThrows[NullPointerException](chm.replace(null, "two", "one"))
      expectThrows[NullPointerException](chm.replace("ONE", null, "one"))
      expectThrows[NullPointerException](chm.replace("ONE", "two", null))

      expect(chm.replace("ONE", "two", "one")).toBeTruthy
      expect(chm.get("ONE")).toEqual("one")
    }

  }

}


object ConcurrentHashMapFactory extends ConcurrentHashMapFactory {
  def allFactories: Iterator[ConcurrentHashMapFactory] =
    Iterator(ConcurrentHashMapFactory)
}

class ConcurrentHashMapFactory extends ConcurrentMapFactory {
  def implementationName: String =
    "java.util.concurrent.ConcurrentHashMap"


  override def empty[K, V]: ju.concurrent.ConcurrentHashMap[K, V] =
    new ju.concurrent.ConcurrentHashMap[K, V]

  def allowsNullKeys: Boolean = false

  def allowsNullValues: Boolean = false
}
