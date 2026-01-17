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

package org.scalajs.testsuite.typedarray

import org.junit.Assert._
import org.junit.Test

import org.scalajs.testsuite.utils.Requires

import scala.scalajs.js.typedarray._
import DataViewExt._

object DataViewTest extends Requires.TypedArray

class DataViewTest {

  @Test def arrayBufferOnlyConstructor(): Unit = {
    val buf = new ArrayBuffer(10)
    val view = new DataView(buf)
    assertTrue(view.isInstanceOf[DataView])
    assertEquals(10, view.byteLength)
    assertEquals(0, view.byteOffset)
    assertEquals(0, view.getInt8(0))
  }

  @Test def arrayBufferAndOffsetConstructor(): Unit = {
    val buf = new ArrayBuffer(10)
    val view = new DataView(buf, 2)
    assertTrue(view.isInstanceOf[DataView])
    assertEquals(8, view.byteLength)
    assertEquals(2, view.byteOffset)
    assertEquals(0, view.getInt8(0))
  }

  @Test def arrayBufferOffsetAndLengthConstructor(): Unit = {
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
    view.setUint8(1, 0xff)
    view.setUint8(2, 0xb3)
    view.setUint8(3, 0x30)

    assertEquals(0x01ff, view.getInt16(0))
    assertEquals(-255, view.getInt16(0, true))
    assertEquals(-77, view.getInt16(1))
    assertEquals(0x30b3, view.getInt16(2, true))
  }

  @Test def getUint16(): Unit = {
    val view = new DataView(new ArrayBuffer(4))

    view.setUint8(0, 0x01)
    view.setUint8(1, 0xff)
    view.setUint8(2, 0xb3)
    view.setUint8(3, 0x30)

    assertEquals(0x01ff, view.getUint16(0))
    assertEquals(0xff01, view.getUint16(0, true))
    assertEquals(0xffb3, view.getUint16(1))
    assertEquals(0x30b3, view.getUint16(2, true))
  }

  @Test def getInt32(): Unit = {
    val view = new DataView(new ArrayBuffer(5))

    view.setUint8(0, 0x01)
    view.setUint8(1, 0xff)
    view.setUint8(2, 0xb3)
    view.setUint8(3, 0x30)
    view.setUint8(4, 0x1a)

    assertEquals(0x01ffb330, view.getInt32(0))
    assertEquals(0x30b3ff01, view.getInt32(0, true))
    assertEquals(0xffb3301a, view.getInt32(1)) // is negative since Int
    assertEquals(0x1a30b3ff, view.getInt32(1, true))
  }

  @Test def getInt64ThroughDataViewExt(): Unit = {
    val view = new DataView(new ArrayBuffer(9))

    view.setUint8(0, 0x01)
    view.setUint8(1, 0xff)
    view.setUint8(2, 0xb3)
    view.setUint8(3, 0x30)
    view.setUint8(4, 0x1a)
    view.setUint8(5, 0x74)
    view.setUint8(6, 0x5c)
    view.setUint8(7, 0xbd)
    view.setUint8(8, 0xaf)

    assertEquals(0x01ffb3301a745cbdL, view.getInt64(0))
    assertEquals(0xbd5c741a30b3ff01L, view.getInt64(0, true))
    assertEquals(0xffb3301a745cbdafL, view.getInt64(1))
    assertEquals(0xafbd5c741a30b3ffL, view.getInt64(1, true))
  }

  @Test def getUint32(): Unit = {
    val view = new DataView(new ArrayBuffer(5))

    view.setUint8(0, 0x01)
    view.setUint8(1, 0xff)
    view.setUint8(2, 0xb3)
    view.setUint8(3, 0x30)
    view.setUint8(4, 0x1a)

    assertEquals(0x01ffb330, view.getUint32(0), 0.0)
    assertEquals(0x30b3ff01, view.getUint32(0, true), 0.0)
    assertEquals(0xffb3301aL.toDouble, view.getUint32(1), 0.0)
    assertEquals(0x1a30b3ff, view.getUint32(1, true), 0.0)
  }

  @Test def getFloat32(): Unit = {
    val view = new DataView(new ArrayBuffer(5))

    view.setFloat32(0, 0.1f)

    assertEquals(0.1, view.getFloat32(0), 1e-7)
    assertNotEquals(0.1, view.getFloat32(1), 1e-7)
    assertNotEquals(0.1, view.getFloat32(0, true), 1e-7)
    assertNotEquals(0.1, view.getFloat32(1, true), 1e-7)
  }

  @Test def getFloat64(): Unit = {
    val view = new DataView(new ArrayBuffer(9))

    view.setFloat64(0, 0.5)

    assertEquals(0.5, view.getFloat64(0), 0.0)
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
    assertEquals(0x7f, view.getUint8(1))
    assertEquals(0xec, view.getUint8(2))
    assertEquals(0xce, view.getUint8(3))
  }

  @Test def setUint8(): Unit = {
    val view = new DataView(new ArrayBuffer(4))

    view.setUint8(0, 0x01)
    view.setUint8(1, 0xfc)
    view.setUint8(2, 0x4d)
    view.setUint8(3, 0x8c)

    assertEquals(1, view.getInt8(0))
    assertEquals(-4, view.getInt8(1))
    assertEquals(0x4d, view.getInt8(2))
    assertEquals(-116, view.getInt8(3))
  }

  @Test def setInt16(): Unit = {
    val view = new DataView(new ArrayBuffer(4))

    view.setInt16(0, 0x7f3b, true)
    view.setInt16(2, -1)

    assertEquals(0x3b, view.getUint8(0))
    assertEquals(0x7f, view.getUint8(1))
    assertEquals(0xff, view.getUint8(2))
    assertEquals(0xff, view.getUint8(3))
  }

  @Test def setUint16(): Unit = {
    val view = new DataView(new ArrayBuffer(4))

    view.setUint16(0, 0x7f3b, true)
    view.setUint16(2, 0xfcba)

    assertEquals(0x3b, view.getUint8(0))
    assertEquals(0x7f, view.getUint8(1))
    assertEquals(0xfc, view.getUint8(2))
    assertEquals(0xba, view.getUint8(3))

    view.setUint16(1, 0x03bc)

    assertEquals(0x3b, view.getUint8(0))
    assertEquals(0x03, view.getUint8(1))
    assertEquals(0xbc, view.getUint8(2))
    assertEquals(0xba, view.getUint8(3))
  }

  @Test def setInt32(): Unit = {
    val view = new DataView(new ArrayBuffer(8))

    view.setInt32(0, 0x7f3b8342, true)
    view.setInt32(3, 0xfcba1020)

    assertEquals(0x42, view.getUint8(0))
    assertEquals(0x83, view.getUint8(1))
    assertEquals(0x3b, view.getUint8(2))
    assertEquals(0xfc, view.getUint8(3))
    assertEquals(0xba, view.getUint8(4))
    assertEquals(0x10, view.getUint8(5))
    assertEquals(0x20, view.getUint8(6))
    assertEquals(0x00, view.getUint8(7))
  }

  @Test def setUint32(): Unit = {
    val view = new DataView(new ArrayBuffer(8))

    view.setUint32(0, 0x7f3b8342, true)
    view.setUint32(3, 0xfcba1020L.toDouble)

    assertEquals(0x42, view.getUint8(0))
    assertEquals(0x83, view.getUint8(1))
    assertEquals(0x3b, view.getUint8(2))
    assertEquals(0xfc, view.getUint8(3))
    assertEquals(0xba, view.getUint8(4))
    assertEquals(0x10, view.getUint8(5))
    assertEquals(0x20, view.getUint8(6))
    assertEquals(0x00, view.getUint8(7))
  }

  @Test def setInt64ThroughDataViewExt(): Unit = {
    val view = new DataView(new ArrayBuffer(16))

    view.setInt64(0, 0x01ffb3301a745cbdL, true)
    view.setInt64(8, 0xfcba1020547de1b5L)

    assertEquals(0xbd, view.getUint8(0))
    assertEquals(0x5c, view.getUint8(1))
    assertEquals(0x74, view.getUint8(2))
    assertEquals(0x1a, view.getUint8(3))
    assertEquals(0x30, view.getUint8(4))
    assertEquals(0xb3, view.getUint8(5))
    assertEquals(0xff, view.getUint8(6))
    assertEquals(0x01, view.getUint8(7))

    assertEquals(0xfc, view.getUint8(8))
    assertEquals(0xba, view.getUint8(9))
    assertEquals(0x10, view.getUint8(10))
    assertEquals(0x20, view.getUint8(11))
    assertEquals(0x54, view.getUint8(12))
    assertEquals(0x7d, view.getUint8(13))
    assertEquals(0xe1, view.getUint8(14))
    assertEquals(0xb5, view.getUint8(15))
  }

  @Test def setFloat32(): Unit = {
    val view = new DataView(new ArrayBuffer(5))

    view.setFloat32(0, 0.1f)
    view.setFloat32(1, 0.4f)

    assertEquals(0.4, view.getFloat32(1), 1e-7)
    assertNotEquals(0.1, view.getFloat32(0), 1e-7)

    view.setFloat32(0, 0.1f, true)
    view.setFloat32(1, 0.4f, true)

    assertNotEquals(0.1, view.getFloat32(0, true), 1e-7)
    assertEquals(0.4, view.getFloat32(1, true), 1e-7)
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
