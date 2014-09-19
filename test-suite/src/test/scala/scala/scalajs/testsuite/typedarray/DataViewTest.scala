/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package scala.scalajs.testsuite.typedarray

import org.scalajs.jasminetest.JasmineTest

import scala.scalajs.js.typedarray._

object DataViewTest extends JasmineTest {

  when("typedarray").
  describe("DataView") {

    it("should provide an ArrayBuffer only constructor") {
      val buf = new ArrayBuffer(10)
      val view = new DataView(buf)
      expect(view.isInstanceOf[DataView]).toBeTruthy
      expect(view.byteLength).toBe(10)
      expect(view.byteOffset).toBe(0)
      expect(view.getInt8(0)).toBe(0)
    }

    it("should provide an ArrayBuffer and offset constructor") {
      val buf = new ArrayBuffer(10)
      val view = new DataView(buf, 2)
      expect(view.isInstanceOf[DataView]).toBeTruthy
      expect(view.byteLength).toBe(8)
      expect(view.byteOffset).toBe(2)
      expect(view.getInt8(0)).toBe(0)
    }

    it("should provide an ArrayBuffer, offset and length constructor") {
      val buf = new ArrayBuffer(10)
      val view = new DataView(buf, 3, 2)
      expect(view.isInstanceOf[DataView]).toBeTruthy
      expect(view.byteLength).toBe(2)
      expect(view.byteOffset).toBe(3)
      expect(view.getInt8(0)).toBe(0)
    }

    it("should provide `getInt8`") {
      val view = new DataView(new ArrayBuffer(4))

      view.setInt8(0, 1)
      view.setInt8(1, 127)
      view.setInt8(3, -50)

      expect(view.getInt8(0)).toBe(1)
      expect(view.getInt8(1)).toBe(127)
      expect(view.getInt8(2)).toBe(0)
      expect(view.getInt8(3)).toBe(-50)
    }

    it("should provide `getUint8`") {
      val view = new DataView(new ArrayBuffer(4))

      view.setInt8(0, 1)
      view.setInt8(1, -127)

      expect(view.getUint8(0)).toBe(1)
      expect(view.getUint8(1)).toBe(129)
      expect(view.getUint8(2)).toBe(0)
    }

    it("should provide `getInt16`") {
      val view = new DataView(new ArrayBuffer(4))

      view.setUint8(0, 0x01)
      view.setUint8(1, 0xFF)
      view.setUint8(2, 0xB3)
      view.setUint8(3, 0x30)

      expect(view.getInt16(0)).toBe(0x01FF)
      expect(view.getInt16(0, true)).toBe(-255)
      expect(view.getInt16(1)).toBe(-77)
      expect(view.getInt16(2, true)).toBe(0x30B3)
    }

    it("should provide `getUint16`") {
      val view = new DataView(new ArrayBuffer(4))

      view.setUint8(0, 0x01)
      view.setUint8(1, 0xFF)
      view.setUint8(2, 0xB3)
      view.setUint8(3, 0x30)

      expect(view.getUint16(0)).toBe(0x01FF)
      expect(view.getUint16(0, true)).toBe(0xFF01)
      expect(view.getUint16(1)).toBe(0xFFB3)
      expect(view.getUint16(2, true)).toBe(0x30B3)
    }

    it("should provide `getInt32`") {
      val view = new DataView(new ArrayBuffer(5))

      view.setUint8(0, 0x01)
      view.setUint8(1, 0xFF)
      view.setUint8(2, 0xB3)
      view.setUint8(3, 0x30)
      view.setUint8(4, 0x1A)

      expect(view.getInt32(0)).toBe(0x01FFB330)
      expect(view.getInt32(0, true)).toBe(0x30B3FF01)
      expect(view.getInt32(1)).toBe(0xFFB3301A) // is negative since Int
      expect(view.getInt32(1, true)).toBe(0x1A30B3FF)
    }

    it("should provide `getUint32`") {
      val view = new DataView(new ArrayBuffer(5))

      view.setUint8(0, 0x01)
      view.setUint8(1, 0xFF)
      view.setUint8(2, 0xB3)
      view.setUint8(3, 0x30)
      view.setUint8(4, 0x1A)

      expect(view.getUint32(0)).toBe(0x01FFB330)
      expect(view.getUint32(0, true)).toBe(0x30B3FF01)
      expect(view.getUint32(1)).toBe(0xFFB3301AL.toDouble)
      expect(view.getUint32(1, true)).toBe(0x1A30B3FF)
    }

    it("should provide `getFloat32`") {
      val view = new DataView(new ArrayBuffer(5))

      view.setFloat32(0, 0.1f)

      expect(view.getFloat32(0)).toBeCloseTo(0.1, 7)
      expect(view.getFloat32(1)).not.toBeCloseTo(0.1, 7)
      expect(view.getFloat32(0, true)).not.toBeCloseTo(0.1, 7)
      expect(view.getFloat32(1, true)).not.toBeCloseTo(0.1, 7)
    }

    it("should provide `getFloat64`") {
      val view = new DataView(new ArrayBuffer(9))

      view.setFloat64(0, 0.5)

      expect(view.getFloat64(0)).toBe(0.5)
      expect(view.getFloat64(1)).not.toBe(0.5)
      expect(view.getFloat64(0, true)).not.toBe(0.5)
      expect(view.getFloat64(1, true)).not.toBe(0.5)
    }

    it("should provide `setInt8`") {
      val view = new DataView(new ArrayBuffer(4))

      view.setInt8(0, 1)
      view.setInt8(1, 127)
      view.setInt8(2, -20)
      view.setInt8(3, -50)

      expect(view.getUint8(0)).toBe(0x01)
      expect(view.getUint8(1)).toBe(0x7F)
      expect(view.getUint8(2)).toBe(0xEC)
      expect(view.getUint8(3)).toBe(0xCE)
    }

    it("should provide `setUint8`") {
      val view = new DataView(new ArrayBuffer(4))

      view.setUint8(0, 0x01)
      view.setUint8(1, 0xFC)
      view.setUint8(2, 0x4D)
      view.setUint8(3, 0x8C)

      expect(view.getInt8(0)).toBe(1)
      expect(view.getInt8(1)).toBe(-4)
      expect(view.getInt8(2)).toBe(0x4D)
      expect(view.getInt8(3)).toBe(-116)
    }

    it("should provide `setInt16`") {
      val view = new DataView(new ArrayBuffer(4))

      view.setInt16(0, 0x7F3B, true)
      view.setInt16(2, -1)

      expect(view.getUint8(0)).toBe(0x3B)
      expect(view.getUint8(1)).toBe(0x7F)
      expect(view.getUint8(2)).toBe(0xFF)
      expect(view.getUint8(3)).toBe(0xFF)
    }

    it("should provide `setUint16`") {
      val view = new DataView(new ArrayBuffer(4))

      view.setUint16(0, 0x7F3B, true)
      view.setUint16(2, 0xFCBA)

      expect(view.getUint8(0)).toBe(0x3B)
      expect(view.getUint8(1)).toBe(0x7F)
      expect(view.getUint8(2)).toBe(0xFC)
      expect(view.getUint8(3)).toBe(0xBA)

      view.setUint16(1, 0x03BC)

      expect(view.getUint8(0)).toBe(0x3B)
      expect(view.getUint8(1)).toBe(0x03)
      expect(view.getUint8(2)).toBe(0xBC)
      expect(view.getUint8(3)).toBe(0xBA)
    }

    it("should provide `setInt32`") {
      val view = new DataView(new ArrayBuffer(8))

      view.setInt32(0, 0x7F3B8342, true)
      view.setInt32(3, 0xFCBA1020)

      expect(view.getUint8(0)).toBe(0x42)
      expect(view.getUint8(1)).toBe(0x83)
      expect(view.getUint8(2)).toBe(0x3B)
      expect(view.getUint8(3)).toBe(0xFC)
      expect(view.getUint8(4)).toBe(0xBA)
      expect(view.getUint8(5)).toBe(0x10)
      expect(view.getUint8(6)).toBe(0x20)
      expect(view.getUint8(7)).toBe(0x00)
    }

    it("should provide `setUint32`") {
      val view = new DataView(new ArrayBuffer(8))

      view.setUint32(0, 0x7F3B8342, true)
      view.setUint32(3, 0xFCBA1020L.toDouble)

      expect(view.getUint8(0)).toBe(0x42)
      expect(view.getUint8(1)).toBe(0x83)
      expect(view.getUint8(2)).toBe(0x3B)
      expect(view.getUint8(3)).toBe(0xFC)
      expect(view.getUint8(4)).toBe(0xBA)
      expect(view.getUint8(5)).toBe(0x10)
      expect(view.getUint8(6)).toBe(0x20)
      expect(view.getUint8(7)).toBe(0x00)
    }

    it("should provide `setFloat32`") {
      val view = new DataView(new ArrayBuffer(5))

      view.setFloat32(0, 0.1f)
      view.setFloat32(1, 0.4f)

      expect(view.getFloat32(1)).toBeCloseTo(0.4, 7)
      expect(view.getFloat32(0)).not.toBeCloseTo(0.1, 7)

      view.setFloat32(0, 0.1f, true)
      view.setFloat32(1, 0.4f, true)

      expect(view.getFloat32(0, true)).not.toBeCloseTo(0.1, 7)
      expect(view.getFloat32(1, true)).toBeCloseTo(0.4, 7)
    }

    it("should provide `getFloat64`") {
      val view = new DataView(new ArrayBuffer(9))

      view.setFloat64(0, 0.5)
      view.setFloat64(1, 0.6)

      expect(view.getFloat64(0)).not.toBe(0.5)
      expect(view.getFloat64(1)).toBe(0.6)

      view.setFloat64(0, 0.5, true)
      view.setFloat64(1, 0.6, true)

      expect(view.getFloat64(0, true)).not.toBe(0.5)
      expect(view.getFloat64(1, true)).toBe(0.6)
    }

  }

}
