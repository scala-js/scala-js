package org.scalajs.testsuite.javalib.util

import java.util.Properties

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._

import org.scalajs.testsuite.utils.AssertThrows._
import org.scalajs.testsuite.utils.Platform._

import scala.collection.JavaConversions._

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
    assertEquals(0, prop.propertyNames().size)
    prop.setProperty("a", "A")
    prop.setProperty("b", "B")
    prop.setProperty("c", "C")
    assertEquals(3, prop.propertyNames().size)
    assertEquals(Set("a", "b", "c"), prop.propertyNames().toSet)

    val prop2 = new Properties(prop)
    prop.setProperty("c", "CC")
    prop.setProperty("d", "D")
    assertEquals(4, prop2.propertyNames().size)
    assertEquals(Set("a", "b", "c", "d"), prop2.propertyNames().toSet)
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
    assertEquals(Set("a", "b", "c", "1"), prop.propertyNames().toSet)
    prop.remove("1")

    val prop2 = new Properties(prop)
    prop.setProperty("c", "CC")
    prop.setProperty("d", "D")

    prop2.put(1.asInstanceOf[AnyRef], "2")
    assertThrows(classOf[Throwable], prop2.propertyNames())
    prop2.remove(1.asInstanceOf[AnyRef])

    prop2.put("1", 1.asInstanceOf[AnyRef])
    assertEquals(Set("a", "b", "c", "d", "1"), prop2.propertyNames().toSet)
  }

  @Test def stringPropertyNames(): Unit = {
    val prop = new Properties()
    assertEquals(0, prop.stringPropertyNames().size)
    prop.setProperty("a", "A")
    prop.setProperty("b", "B")
    prop.setProperty("c", "C")
    assertEquals(3, prop.stringPropertyNames().size)
    assertEquals(Set("a", "b", "c"), prop.stringPropertyNames().toSet)

    val prop2 = new Properties(prop)
    prop.setProperty("c", "CC")
    prop.setProperty("d", "D")
    assertEquals(4, prop2.stringPropertyNames().size)
    assertEquals(Set("a", "b", "c", "d"), prop2.stringPropertyNames().toSet)
  }

  @Test def stringPropertyNamesWithBadContents(): Unit = {
    assumeTrue("Assumed compliant asInstanceOf", hasCompliantAsInstanceOfs)

    val prop = new Properties()
    prop.setProperty("a", "A")
    prop.setProperty("b", "B")
    prop.setProperty("c", "C")

    prop.put(1.asInstanceOf[AnyRef], "2")
    assertEquals(Set("a", "b", "c"), prop.stringPropertyNames().toSet)
    prop.remove(1.asInstanceOf[AnyRef])

    prop.put("1", 1.asInstanceOf[AnyRef])
    assertEquals(Set("a", "b", "c"), prop.stringPropertyNames().toSet)
    prop.remove("1")

    val prop2 = new Properties(prop)
    prop.setProperty("c", "CC")
    prop.setProperty("d", "D")

    prop2.put(1.asInstanceOf[AnyRef], "2")
    assertEquals(Set("a", "b", "c", "d"), prop2.stringPropertyNames().toSet)
    prop2.remove(1.asInstanceOf[AnyRef])

    prop2.put("1", 1.asInstanceOf[AnyRef])
    assertEquals(Set("a", "b", "c", "d"), prop2.stringPropertyNames().toSet)
  }
}
