/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.typedarray

import org.junit.Assert._
import org.junit.Test

import org.scalajs.testsuite.utils.Requires

import scala.scalajs.js.typedarray._
import DataViewExt._

object DataViewTest extends Requires.TypedArray

class DataViewTest {

  @Test def arrayBuffer_only_constructor(): Unit = {
    val buf = new ArrayBuffer(10)
    val view = new DataView(buf)
    assertTrue(view.isInstanceOf[DataView])
    assertEquals(10, view.byteLength)
    assertEquals(0, view.byteOffset)
    assertEquals(0, view.getInt8(0))
  }

  @Test def arrayBuffer_and_offset_constructor(): Unit = {
    val buf = new ArrayBuffer(10)
    val view = new DataView(buf, 2)
    assertTrue(view.isInstanceOf[DataView])
    assertEquals(8, view.byteLength)
    assertEquals(2, view.byteOffset)
    assertEquals(0, view.getInt8(0))
  }

  @Test def arrayBuffer_offset_and_length_constructor(): Unit = {
    val buf = new ArrayBuffer(10)
    val view = new DataView(buf, 3, 2)
    assertTrue(view.isInstanceOf[DataView])
    assertEquals(2, view.byteLength)
    assertEquals(3, view.byteOffset)
    assertEquals(0, view.getInt8(0))
  }

  @Test def getInt8(): Unit = {
    val view = new DataView(new ArrayBuffer(4))

    view.setInt8(0, 1)
    view.setInt8(1, 127)
    view.setInt8(3, -50)

    assertEquals(1, view.getInt8(0))
    assertEquals(127, view.getInt8(1))
    assertEquals(0, view.getInt8(2))
    assertEquals(-50, view.getInt8(3))
  }

  @Test def getUint8(): Unit = {
    val view = new DataView(new ArrayBuffer(4))

    view.setInt8(0, 1)
    view.setInt8(1, -127)

    assertEquals(1, view.getUint8(0))
    assertEquals(129, view.getUint8(1))
    assertEquals(0, view.getUint8(2))
  }

  @Test def getInt16(): Unit = {
    val view = new DataView(new ArrayBuffer(4))

    view.setUint8(0, 0x01)
    view.setUint8(1, 0xFF)
    view.setUint8(2, 0xB3)
    view.setUint8(3, 0x30)

    assertEquals(0x01FF, view.getInt16(0))
    assertEquals(-255, view.getInt16(0, true))
    assertEquals(-77, view.getInt16(1))
    assertEquals(0x30B3, view.getInt16(2, true))
  }

  @Test def getUint16(): Unit = {
    val view = new DataView(new ArrayBuffer(4))

    view.setUint8(0, 0x01)
    view.setUint8(1, 0xFF)
    view.setUint8(2, 0xB3)
    view.setUint8(3, 0x30)

    assertEquals(0x01FF, view.getUint16(0))
    assertEquals(0xFF01, view.getUint16(0, true))
    assertEquals(0xFFB3, view.getUint16(1))
    assertEquals(0x30B3, view.getUint16(2, true))
  }

  @Test def getInt32(): Unit = {
    val view = new DataView(new ArrayBuffer(5))

    view.setUint8(0, 0x01)
    view.setUint8(1, 0xFF)
    view.setUint8(2, 0xB3)
    view.setUint8(3, 0x30)
    view.setUint8(4, 0x1A)

    assertEquals(0x01FFB330, view.getInt32(0))
    assertEquals(0x30B3FF01, view.getInt32(0, true))
    assertEquals(0xFFB3301A, view.getInt32(1)) // is negative since Int
    assertEquals(0x1A30B3FF, view.getInt32(1, true))
  }

  @Test def getInt64_through_DataViewExt(): Unit = {
    val view = new DataView(new ArrayBuffer(9))

    view.setUint8(0, 0x01)
    view.setUint8(1, 0xFF)
    view.setUint8(2, 0xB3)
    view.setUint8(3, 0x30)
    view.setUint8(4, 0x1A)
    view.setUint8(5, 0x74)
    view.setUint8(6, 0x5C)
    view.setUint8(7, 0xBD)
    view.setUint8(8, 0xAF)

    assertEquals(0x01FFB3301A745CBDL, view.getInt64(0))
    assertEquals(0xBD5C741A30B3FF01L, view.getInt64(0, true))
    assertEquals(0xFFB3301A745CBDAFL, view.getInt64(1))
    assertEquals(0xAFBD5C741A30B3FFL, view.getInt64(1, true))
  }

  @Test def getUint32(): Unit = {
    val view = new DataView(new ArrayBuffer(5))

    view.setUint8(0, 0x01)
    view.setUint8(1, 0xFF)
    view.setUint8(2, 0xB3)
    view.setUint8(3, 0x30)
    view.setUint8(4, 0x1A)

    assertEquals(0x01FFB330, view.getUint32(0))
    assertEquals(0x30B3FF01, view.getUint32(0, true))
    assertEquals(0xFFB3301AL.toDouble, view.getUint32(1))
    assertEquals(0x1A30B3FF, view.getUint32(1, true))
  }

  @Test def getFloat32(): Unit = {
    val view = new DataView(new ArrayBuffer(5))

    view.setFloat32(0, 0.1f)

    assertEquals(0.1, view.getFloat32(0), 1E-7)
    assertNotEquals(0.1, view.getFloat32(1), 1E-7)
    assertNotEquals(0.1, view.getFloat32(0, true), 1E-7)
    assertNotEquals(0.1, view.getFloat32(1, true), 1E-7)
  }

  @Test def getFloat64(): Unit = {
    val view = new DataView(new ArrayBuffer(9))

    view.setFloat64(0, 0.5)

    assertEquals(0.5, view.getFloat64(0))
    assertNotEquals(0.5, view.getFloat64(1), 0.0)
    assertNotEquals(0.5, view.getFloat64(0, true), 0.0)
    assertNotEquals(0.5, view.getFloat64(1, true), 0.0)
  }

  @Test def setInt8(): Unit = {
    val view = new DataView(new ArrayBuffer(4))

    view.setInt8(0, 1)
    view.setInt8(1, 127)
    view.setInt8(2, -20)
    view.setInt8(3, -50)

    assertEquals(0x01, view.getUint8(0))
    assertEquals(0x7F, view.getUint8(1))
    assertEquals(0xEC, view.getUint8(2))
    assertEquals(0xCE, view.getUint8(3))
  }

  @Test def setUint8(): Unit = {
    val view = new DataView(new ArrayBuffer(4))

    view.setUint8(0, 0x01)
    view.setUint8(1, 0xFC)
    view.setUint8(2, 0x4D)
    view.setUint8(3, 0x8C)

    assertEquals(1, view.getInt8(0))
    assertEquals(-4, view.getInt8(1))
    assertEquals(0x4D, view.getInt8(2))
    assertEquals(-116, view.getInt8(3))
  }

  @Test def setInt16(): Unit = {
    val view = new DataView(new ArrayBuffer(4))

    view.setInt16(0, 0x7F3B, true)
    view.setInt16(2, -1)

    assertEquals(0x3B, view.getUint8(0))
    assertEquals(0x7F, view.getUint8(1))
    assertEquals(0xFF, view.getUint8(2))
    assertEquals(0xFF, view.getUint8(3))
  }

  @Test def setUint16(): Unit = {
    val view = new DataView(new ArrayBuffer(4))

    view.setUint16(0, 0x7F3B, true)
    view.setUint16(2, 0xFCBA)

    assertEquals(0x3B, view.getUint8(0))
    assertEquals(0x7F, view.getUint8(1))
    assertEquals(0xFC, view.getUint8(2))
    assertEquals(0xBA, view.getUint8(3))

    view.setUint16(1, 0x03BC)

    assertEquals(0x3B, view.getUint8(0))
    assertEquals(0x03, view.getUint8(1))
    assertEquals(0xBC, view.getUint8(2))
    assertEquals(0xBA, view.getUint8(3))
  }

  @Test def setInt32(): Unit = {
    val view = new DataView(new ArrayBuffer(8))

    view.setInt32(0, 0x7F3B8342, true)
    view.setInt32(3, 0xFCBA1020)

    assertEquals(0x42, view.getUint8(0))
    assertEquals(0x83, view.getUint8(1))
    assertEquals(0x3B, view.getUint8(2))
    assertEquals(0xFC, view.getUint8(3))
    assertEquals(0xBA, view.getUint8(4))
    assertEquals(0x10, view.getUint8(5))
    assertEquals(0x20, view.getUint8(6))
    assertEquals(0x00, view.getUint8(7))
  }

  @Test def setUint32(): Unit = {
    val view = new DataView(new ArrayBuffer(8))

    view.setUint32(0, 0x7F3B8342, true)
    view.setUint32(3, 0xFCBA1020L.toDouble)

    assertEquals(0x42, view.getUint8(0))
    assertEquals(0x83, view.getUint8(1))
    assertEquals(0x3B, view.getUint8(2))
    assertEquals(0xFC, view.getUint8(3))
    assertEquals(0xBA, view.getUint8(4))
    assertEquals(0x10, view.getUint8(5))
    assertEquals(0x20, view.getUint8(6))
    assertEquals(0x00, view.getUint8(7))
  }

  @Test def setInt64_through_DataViewExt(): Unit = {
    val view = new DataView(new ArrayBuffer(16))

    view.setInt64(0, 0x01FFB3301A745CBDL, true)
    view.setInt64(8, 0xFCBA1020547DE1B5L)

    assertEquals(0xBD, view.getUint8(0))
    assertEquals(0x5C, view.getUint8(1))
    assertEquals(0x74, view.getUint8(2))
    assertEquals(0x1A, view.getUint8(3))
    assertEquals(0x30, view.getUint8(4))
    assertEquals(0xB3, view.getUint8(5))
    assertEquals(0xFF, view.getUint8(6))
    assertEquals(0x01, view.getUint8(7))

    assertEquals(0xFC, view.getUint8(8))
    assertEquals(0xBA, view.getUint8(9))
    assertEquals(0x10, view.getUint8(10))
    assertEquals(0x20, view.getUint8(11))
    assertEquals(0x54, view.getUint8(12))
    assertEquals(0x7D, view.getUint8(13))
    assertEquals(0xE1, view.getUint8(14))
    assertEquals(0xB5, view.getUint8(15))
  }

  @Test def setFloat32(): Unit = {
    val view = new DataView(new ArrayBuffer(5))

    view.setFloat32(0, 0.1f)
    view.setFloat32(1, 0.4f)

    assertEquals(0.4, view.getFloat32(1), 1E-7)
    assertNotEquals(0.1, view.getFloat32(0), 1E-7)

    view.setFloat32(0, 0.1f, true)
    view.setFloat32(1, 0.4f, true)

    assertNotEquals(0.1, view.getFloat32(0, true), 1E-7)
    assertEquals(0.4, view.getFloat32(1, true), 1E-7)
  }

  @Test def setFloat64(): Unit = {
    val view = new DataView(new ArrayBuffer(9))

    view.setFloat64(0, 0.5)
    view.setFloat64(1, 0.6)

    assertNotEquals(0.5, view.getFloat64(0), 0.0)
    assertEquals(0.6, view.getFloat64(1), 0.0)

    view.setFloat64(0, 0.5, true)
    view.setFloat64(1, 0.6, true)

    assertNotEquals(0.5, view.getFloat64(0, true), 0.0)
    assertEquals(0.6, view.getFloat64(1, true), 0.0)
  }
}
