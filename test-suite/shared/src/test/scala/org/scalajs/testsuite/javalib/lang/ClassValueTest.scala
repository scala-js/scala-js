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

package org.scalajs.testsuite.javalib.lang

import org.junit.Test
import org.junit.Assert._

class ClassValueTest {
  @Test def testClassValue(): Unit = {
    val classValue = new ClassValue[Any] {
      private var counter: Int = 0

      // () and null are corner-cases of our implementation
      protected def computeValue(cls: Class[_]): Any = {
        counter += 1
        cls match {
          case Integer.TYPE           => ()
          case _ if cls.isPrimitive() => null
          case _                      => cls.getName() + " " + counter
        }
      }
    }

    assertEquals("java.lang.String 1", classValue.get(classOf[String]))
    assertEquals("scala.Option 2", classValue.get(classOf[Option[_]]))
    assertEquals("java.lang.String 1", classValue.get(classOf[String]))
    assertEquals("scala.Option 2", classValue.get(classOf[Option[_]]))

    assertEquals(null, classValue.get(classOf[Boolean])) // insert
    assertEquals(null, classValue.get(classOf[Boolean])) // lookup, does not touch counter
    assertEquals((), classValue.get(classOf[Int])) // insert
    assertEquals((), classValue.get(classOf[Int])) // lookup, does not touch counter

    // the counter was incremented exactly twice for the primitives
    assertEquals("scala.collection.immutable.List 5", classValue.get(classOf[List[_]]))
    assertEquals("scala.collection.immutable.List 5", classValue.get(classOf[List[_]]))

    assertEquals("java.lang.String 1", classValue.get(classOf[String]))

    classValue.remove(classOf[String])
    assertEquals("scala.collection.immutable.List 5", classValue.get(classOf[List[_]]))
    assertEquals("java.lang.String 6", classValue.get(classOf[String]))
    assertEquals("java.lang.String 6", classValue.get(classOf[String]))

  }
}
