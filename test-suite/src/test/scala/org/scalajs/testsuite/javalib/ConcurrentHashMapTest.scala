/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib

import scala.language.implicitConversions

import scala.collection.JavaConversions._

import java.util.{concurrent => juc}


object ConcurrentHashMapTest extends ConcurrentHashMapTest(new ConcurrentHashMapFactory)

class ConcurrentHashMapTest[F <: ConcurrentHashMapFactory](mapFactory: F)
  extends AbstractMapTest[F](mapFactory) {

  final protected def allowsNullKeys: Boolean = false
  final protected def allowsNullValues: Boolean = false

  override def testApi(): Unit = {

    super.testApi()

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
      expectNullPointerException(chm.replace("ONE", null))
      expectNullPointerException(chm.replace(null, "one"))
      expect(chm.get("ONE")).toEqual("two")

      expect(chm.replace("ONE", "one", "two")).toBeFalsy
      expectNullPointerException(chm.replace(null, "two", "one"))
      expectNullPointerException(chm.replace("ONE", null, "one"))
      expectNullPointerException(chm.replace("ONE", "two", null))

      expect(chm.replace("ONE", "two", "one")).toBeTruthy
      expect(chm.get("ONE")).toEqual("one")
    }

  }

}

class ConcurrentHashMapFactory extends AbstractMapFactory {

  override def implementationName: String =
    "java.util.concurrent.ConcurrentHashMap"

  override def empty[K, V]: juc.ConcurrentHashMap[K, V] =
    new juc.ConcurrentHashMap[K, V]

}
