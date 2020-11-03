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

import language.implicitConversions

import org.junit.{After, Test}
import org.junit.Assert._
import org.junit.Assume._

import org.scalajs.testsuite.utils.AssertThrows._
import org.scalajs.testsuite.utils.Platform._

class SystemPropertiesTest {

  private final val LineSeparatorPropName = "line.separator"
  private final val ExistingPropName = "org.scalajs.testsuite.existingprop"
  private final val TestPropName = "org.scalajs.testsuite.testprop"

  @After def resetSystemPropertiesAfterEachTest(): Unit = {
    System.setProperties(null)
  }

  /** Tests scenarios where only `getProperty`, `setProperty` and
   *  `clearProperty` are used.
   *
   *  In those scenarios, the inner `java.util.Properties` is not forced, and
   *  only the `dict` field of `SystemProperties` is manipulated.
   */
  @Test def testScenariosWithoutJavaUtilProperties(): Unit = {
    // Known property, always \n even on the JVM because our build ensures it
    assertEquals("\n", System.getProperty(LineSeparatorPropName))
    assertEquals("\n", System.getProperty(LineSeparatorPropName, "some default"))
    assertEquals(null, System.getProperty("this.property.does.not.exist"))
    assertEquals("some default", System.getProperty("this.property.does.not.exist", "some default"))

    assertEquals(null, System.getProperty(TestPropName))
    assertEquals(null, System.setProperty(TestPropName, "test value"))
    assertEquals("test value", System.getProperty(TestPropName))
    assertEquals("test value", System.getProperty(TestPropName, "some default"))
    assertEquals("test value", System.setProperty(TestPropName, "another value"))
    assertEquals("another value", System.getProperty(TestPropName))
    assertEquals("another value", System.clearProperty(TestPropName))
    assertEquals(null, System.getProperty(TestPropName))
    assertEquals(null, System.clearProperty(TestPropName))
  }

  /** Tests scenarios where we call `getProperties()`, forcing the inner
   *  `java.util.Properties` to be instantiated.
   *
   *  Also tests interactions between the existing values in `dict` at the time
   *  we force the `java.util.Properties`, and further interactions with
   *  `getProperty`, `setProperty` and `clearProperty`.
   */
  @Test def testScenariosWithGetProperties(): Unit = {
    System.setProperty(ExistingPropName, "existing value")

    val props = System.getProperties()
    assertNotNull(props)

    assertEquals("\n", props.getProperty(LineSeparatorPropName))
    assertEquals("\n", props.getProperty(LineSeparatorPropName, "some default"))
    assertEquals(null, props.getProperty("this.property.does.not.exist"))
    assertEquals("some default", props.getProperty("this.property.does.not.exist", "some default"))

    // Existing props prior to calling getProperties() are visible
    assertEquals("existing value", props.getProperty(ExistingPropName))

    // Manipulate props
    assertEquals(null, props.getProperty(TestPropName))
    assertEquals(null, props.setProperty(TestPropName, "test value"))
    assertEquals("test value", props.getProperty(TestPropName))
    assertEquals("test value", System.getProperty(TestPropName)) // reflects on System
    assertEquals("test value", props.getProperty(TestPropName, "some default"))
    assertEquals("test value", props.setProperty(TestPropName, "another value"))
    assertEquals("another value", props.getProperty(TestPropName))
    assertEquals("another value", System.setProperty(TestPropName, "third value"))
    assertEquals("third value", props.getProperty(TestPropName)) // System reflects on props
    assertEquals("third value", System.clearProperty(TestPropName))
    assertEquals(null, props.getProperty(TestPropName)) // System.clear also reflects on props
    assertEquals(null, System.clearProperty(TestPropName))

    // Kill everything
    props.clear()
    assertEquals(null, props.getProperty(LineSeparatorPropName))
    assertEquals(null, System.getProperty(LineSeparatorPropName)) // also kills it for System
  }

  /** Tests the effects of `setProperties()`, and its interactions with the
   *  other methods related to system properties.
   */
  @Test def testScenariosWithSetProperties(): Unit = {
    val props = new java.util.Properties()
    assertEquals(null, props.getProperty(LineSeparatorPropName))
    assertEquals(null, props.setProperty(TestPropName, "test value"))

    System.setProperties(props)
    assertSame(props, System.getProperties())
    assertEquals(null, System.getProperty(LineSeparatorPropName))
    assertEquals("test value", System.getProperty(TestPropName))
  }
}
