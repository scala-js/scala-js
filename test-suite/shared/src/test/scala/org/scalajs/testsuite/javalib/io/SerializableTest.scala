package org.scalajs.testsuite.javalib.io

import org.junit.Test
import org.junit.Assert._

import java.io.Serializable

class SerializableTest {
  @Test def boxedClassesAreSerializable(): Unit = {
    assertTrue("Unit", classOf[Serializable].isAssignableFrom(classOf[scala.runtime.BoxedUnit]))
    assertTrue("Boolean", classOf[Serializable].isAssignableFrom(classOf[java.lang.Boolean]))
    assertTrue("Character", classOf[Serializable].isAssignableFrom(classOf[java.lang.Character]))
    assertTrue("Byte", classOf[Serializable].isAssignableFrom(classOf[java.lang.Byte]))
    assertTrue("Short", classOf[Serializable].isAssignableFrom(classOf[java.lang.Short]))
    assertTrue("Integer", classOf[Serializable].isAssignableFrom(classOf[java.lang.Integer]))
    assertTrue("Long", classOf[Serializable].isAssignableFrom(classOf[java.lang.Long]))
    assertTrue("Float", classOf[Serializable].isAssignableFrom(classOf[java.lang.Float]))
    assertTrue("Double", classOf[Serializable].isAssignableFrom(classOf[java.lang.Double]))
    assertTrue("String", classOf[Serializable].isAssignableFrom(classOf[java.lang.String]))
  }

  @Test def boxedPrimitiveValuesAreSerializable(): Unit = {
    assertTrue("Unit", ((): Any).isInstanceOf[Serializable])
    assertTrue("Boolean", (true: Any).isInstanceOf[Serializable])
    assertTrue("Char", ('Z': Any).isInstanceOf[Serializable])
    assertTrue("Byte", (1.toByte: Any).isInstanceOf[Serializable])
    assertTrue("Short", (25000.toShort: Any).isInstanceOf[Serializable])
    assertTrue("Int", (100000: Any).isInstanceOf[Serializable])
    assertTrue("Long", (5L: Any).isInstanceOf[Serializable])
    assertTrue("Float", (1.5f: Any).isInstanceOf[Serializable])
    assertTrue("Double", (1.4: Any).isInstanceOf[Serializable])
    assertTrue("String", ("hello": Any).isInstanceOf[Serializable])
  }
}
