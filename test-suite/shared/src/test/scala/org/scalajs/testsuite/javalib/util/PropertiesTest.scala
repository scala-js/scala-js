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

package org.scalajs.testsuite.javalib.util

import java.{util => ju}
import java.util.Properties

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._

import org.scalajs.testsuite.utils.AssertThrows._
import org.scalajs.testsuite.utils.Platform._

import Utils._

class PropertiesTest {

  @Test def setProperty(): Unit = {
    val prop = new Properties()
    prop.setProperty("a", "A")
    assertEquals("A", prop.get("a"))
    prop.setProperty("a", "AA")
    prop.setProperty("b", "B")
    assertEquals("AA", prop.get("a"))
    assertEquals("B", prop.get("b"))

    val prop2 = new Properties(prop)
    prop2.setProperty("a", "AAA")
    assertEquals("AAA", prop2.get("a"))
  }

  @Test def getProperty(): Unit = {
    val prop = new Properties()

    assertNull(prop.getProperty("a"))
    prop.setProperty("a", "A")
    assertEquals("A", prop.getProperty("a"))
    assertNull(prop.getProperty("aa"))

    assertEquals("A", prop.getProperty("a", "B"))
    assertEquals("B", prop.getProperty("b", "B"))

    // Tests with default properties
    prop.setProperty("b", "B")

    val prop2 = new Properties(prop)
    prop2.setProperty("b", "BB")
    prop2.setProperty("c", "C")
    assertEquals("A", prop2.getProperty("a"))
    assertEquals("BB", prop2.getProperty("b"))
    assertEquals("C", prop2.getProperty("c"))
  }

  @Test def propertyNames(): Unit = {
    val prop = new Properties()
    assertTrue(enumerationIsEmpty(prop.propertyNames()))
    prop.setProperty("a", "A")
    prop.setProperty("b", "B")
    prop.setProperty("c", "C")
    assertEquals(3, enumerationSize(prop.propertyNames()))
    assertEnumSameElementsAsSet[Any]("a", "b", "c")(prop.propertyNames())

    val prop2 = new Properties(prop)
    prop.setProperty("c", "CC")
    prop.setProperty("d", "D")
    assertEquals(4, enumerationSize(prop2.propertyNames()))
    assertEnumSameElementsAsSet[Any]("a", "b", "c", "d")(prop2.propertyNames())
  }

  @Test def propertyNamesIsNotAffectedByOverriddenPropertyNamesInDefaults(): Unit = {
    val defaults = new java.util.Properties {
      override def propertyNames(): ju.Enumeration[_] =
        ju.Collections.emptyEnumeration[String]()
    }
    defaults.setProperty("foo", "bar")

    val props = new Properties(defaults)
    props.setProperty("foobar", "babar")
    assertEnumSameElementsAsSet[Any]("foo", "foobar")(props.propertyNames())
  }

  @Test def propertyNamesWithBadContents(): Unit = {
    assumeTrue("Assumed compliant asInstanceOf", hasCompliantAsInstanceOfs)

    val prop = new Properties()
    prop.setProperty("a", "A")
    prop.setProperty("b", "B")
    prop.setProperty("c", "C")

    prop.put(1.asInstanceOf[AnyRef], "2")
    assertThrows(classOf[Throwable], prop.propertyNames())
    prop.remove(1.asInstanceOf[AnyRef])

    prop.put("1", 1.asInstanceOf[AnyRef])
    assertEnumSameElementsAsSet[Any]("a", "b", "c", "1")(prop.propertyNames())
    prop.remove("1")

    val prop2 = new Properties(prop)
    prop.setProperty("c", "CC")
    prop.setProperty("d", "D")

    prop2.put(1.asInstanceOf[AnyRef], "2")
    assertThrows(classOf[Throwable], prop2.propertyNames())
    prop2.remove(1.asInstanceOf[AnyRef])

    prop2.put("1", 1.asInstanceOf[AnyRef])
    assertEnumSameElementsAsSet[Any]("a", "b", "c", "d", "1")(prop2.propertyNames())
  }

  @Test def stringPropertyNames(): Unit = {
    val prop = new Properties()
    assertEquals(0, prop.stringPropertyNames().size)
    prop.setProperty("a", "A")
    prop.setProperty("b", "B")
    prop.setProperty("c", "C")
    assertEquals(3, prop.stringPropertyNames().size)
    assertCollSameElementsAsSet("a", "b", "c")(prop.stringPropertyNames())

    val prop2 = new Properties(prop)
    prop.setProperty("c", "CC")
    prop.setProperty("d", "D")
    assertEquals(4, prop2.stringPropertyNames().size)
    assertCollSameElementsAsSet("a", "b", "c", "d")(prop2.stringPropertyNames())
  }

  @Test def stringPropertyNamesIsNotAffectedByOverriddenStringPropertyNamesInDefaults(): Unit = {
    val defaults = new java.util.Properties {
      override def stringPropertyNames(): ju.Set[String] =
        ju.Collections.emptySet[String]()
    }
    defaults.setProperty("foo", "bar")

    val props = new Properties(defaults)
    props.setProperty("foobar", "babar")
    assertCollSameElementsAsSet("foo", "foobar")(props.stringPropertyNames())
  }

  @Test def stringPropertyNamesWithBadContents(): Unit = {
    assumeTrue("Assumed compliant asInstanceOf", hasCompliantAsInstanceOfs)

    val prop = new Properties()
    prop.setProperty("a", "A")
    prop.setProperty("b", "B")
    prop.setProperty("c", "C")

    prop.put(1.asInstanceOf[AnyRef], "2")
    assertCollSameElementsAsSet("a", "b", "c")(prop.stringPropertyNames())
    prop.remove(1.asInstanceOf[AnyRef])

    prop.put("1", 1.asInstanceOf[AnyRef])
    assertCollSameElementsAsSet("a", "b", "c")(prop.stringPropertyNames())
    prop.remove("1")

    val prop2 = new Properties(prop)
    prop.setProperty("c", "CC")
    prop.setProperty("d", "D")

    prop2.put(1.asInstanceOf[AnyRef], "2")
    assertCollSameElementsAsSet("a", "b", "c", "d")(prop2.stringPropertyNames())
    prop2.remove(1.asInstanceOf[AnyRef])

    prop2.put("1", 1.asInstanceOf[AnyRef])
    assertCollSameElementsAsSet("a", "b", "c", "d")(prop2.stringPropertyNames())
  }
}
