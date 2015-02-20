/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.niobuffer

import java.nio._

import scala.scalajs.js
import js.JSConverters._

object ByteBufferTest extends BaseBufferTest {
  type Factory = BufferFactory.ByteBufferFactory

  def defineTests(factory: Factory): Unit = {
    import factory._

    commonTests(factory)

    it("order()") {
      val buf = allocBuffer(10)
      expect(buf.order() == ByteOrder.BIG_ENDIAN).toBeTruthy
      buf.order(ByteOrder.LITTLE_ENDIAN)
      expect(buf.order() == ByteOrder.LITTLE_ENDIAN).toBeTruthy
      buf.order(ByteOrder.BIG_ENDIAN)
      expect(buf.order() == ByteOrder.BIG_ENDIAN).toBeTruthy
    }

    it("relative getChar()") {
      val buf = withContent(10, elemRange(0x7b, 0x85): _*)

      buf.order(ByteOrder.BIG_ENDIAN)
      expect(buf.getChar().toInt).toEqual(0x7b7c)
      expect(buf.position).toEqual(2)
      expect(buf.getChar().toInt).toEqual(0x7d7e)

      buf.order(ByteOrder.LITTLE_ENDIAN)
      buf.position(6)
      expect(buf.getChar().toInt).toEqual(0x8281)
      expect(buf.getChar().toInt).toEqual(0x8483)

      expect(() => buf.getChar()).toThrow
    }

    if (!createsReadOnly) {
      it("relative putChar()") {
        val buf = allocBuffer(10)
        buf.putChar(0x7b7c)
        expect(buf.position()).toEqual(2)
        expect(buf.get(0)).toEqual(0x7b)
        expect(buf.get(1)).toEqual(0x7c)
        buf.putChar(0x7d7e)
        expect(buf.get(2)).toEqual(0x7d)
        expect(buf.get(3)).toEqual(0x7e)
        expect(buf.get(4)).toEqual(0)

        buf.order(ByteOrder.LITTLE_ENDIAN)
        buf.position(7)
        buf.putChar(0x8182)
        expect(buf.get(6)).toEqual(0)
        expect(buf.get(7)).toEqual(0x82.toByte)
        expect(buf.get(8)).toEqual(0x81.toByte)

        expect(() => buf.putChar(0x8384)).toThrow
      }
    } else {
      it("relative putChar() - read-only") {
        val buf = allocBuffer(10)
        expect(() => buf.putChar(0x7576)).toThrow
        expect(buf.get(0)).toEqual(0)
        expect(buf.position()).toEqual(0)
      }
    }

    it("absolute getChar()") {
      val buf = withContent(10, elemRange(0x7b, 0x85): _*)

      buf.order(ByteOrder.BIG_ENDIAN)
      expect(buf.getChar(3).toInt).toEqual(0x7e7f)
      expect(buf.position).toEqual(0)
      expect(buf.getChar(4).toInt).toEqual(0x7f80)

      buf.order(ByteOrder.LITTLE_ENDIAN)
      buf.position(6)
      expect(buf.getChar(2).toInt).toEqual(0x7e7d)
      expect(buf.getChar(8).toInt).toEqual(0x8483)

      expect(() => buf.getChar(9)).toThrow
    }

    if (!createsReadOnly) {
      it("absolute putChar()") {
        val buf = allocBuffer(10)
        buf.putChar(2, 0x7b7c)
        expect(buf.position()).toEqual(0)
        expect(buf.get(0)).toEqual(0)
        expect(buf.get(2)).toEqual(0x7b)
        expect(buf.get(3)).toEqual(0x7c)
        buf.putChar(3, 0x7d7e)
        expect(buf.get(2)).toEqual(0x7b)
        expect(buf.get(3)).toEqual(0x7d)
        expect(buf.get(4)).toEqual(0x7e)
        expect(buf.get(5)).toEqual(0)

        buf.order(ByteOrder.LITTLE_ENDIAN)
        buf.position(7)
        buf.putChar(6, 0x8182)
        expect(buf.get(5)).toEqual(0)
        expect(buf.get(6)).toEqual(0x82.toByte)
        expect(buf.get(7)).toEqual(0x81.toByte)

        expect(() => buf.putChar(9, 0x8384)).toThrow
      }
    } else {
      it("absolute putChar() - read-only") {
        val buf = allocBuffer(10)
        expect(() => buf.putChar(3, 0x7576)).toThrow
        expect(buf.get(3)).toEqual(0)
        expect(buf.position()).toEqual(0)
      }
    }

    it("relative getShort()") {
      val buf = withContent(10, elemRange(0x7b, 0x85): _*)

      buf.order(ByteOrder.BIG_ENDIAN)
      expect(buf.getShort()).toEqual(0x7b7c)
      expect(buf.position).toEqual(2)
      expect(buf.getShort()).toEqual(0x7d7e)

      buf.order(ByteOrder.LITTLE_ENDIAN)
      buf.position(6)
      expect(buf.getShort()).toEqual(0xffff8281)
      expect(buf.getShort()).toEqual(0xffff8483)

      expect(() => buf.getShort()).toThrow
    }

    if (!createsReadOnly) {
      it("relative putShort()") {
        val buf = allocBuffer(10)
        buf.putShort(0x7b7c)
        expect(buf.position()).toEqual(2)
        expect(buf.get(0)).toEqual(0x7b)
        expect(buf.get(1)).toEqual(0x7c)
        buf.putShort(0x7d7e)
        expect(buf.get(2)).toEqual(0x7d)
        expect(buf.get(3)).toEqual(0x7e)
        expect(buf.get(4)).toEqual(0)

        buf.order(ByteOrder.LITTLE_ENDIAN)
        buf.position(7)
        buf.putShort(0xffff8182)
        expect(buf.get(6)).toEqual(0)
        expect(buf.get(7)).toEqual(0x82.toByte)
        expect(buf.get(8)).toEqual(0x81.toByte)

        expect(() => buf.putShort(0xffff8384)).toThrow
      }
    } else {
      it("relative putShort() - read-only") {
        val buf = allocBuffer(10)
        expect(() => buf.putShort(0x7576)).toThrow
        expect(buf.get(0)).toEqual(0)
        expect(buf.position()).toEqual(0)
      }
    }

    it("absolute getShort()") {
      val buf = withContent(10, elemRange(0x7b, 0x85): _*)

      buf.order(ByteOrder.BIG_ENDIAN)
      expect(buf.getShort(3)).toEqual(0x7e7f)
      expect(buf.position).toEqual(0)
      expect(buf.getShort(4)).toEqual(0x7f80)

      buf.order(ByteOrder.LITTLE_ENDIAN)
      buf.position(6)
      expect(buf.getShort(2)).toEqual(0x7e7d)
      expect(buf.getShort(8)).toEqual(0xffff8483)

      expect(() => buf.getShort(9)).toThrow
    }

    if (!createsReadOnly) {
      it("absolute putShort()") {
        val buf = allocBuffer(10)
        buf.putShort(2, 0x7b7c)
        expect(buf.position()).toEqual(0)
        expect(buf.get(0)).toEqual(0)
        expect(buf.get(2)).toEqual(0x7b)
        expect(buf.get(3)).toEqual(0x7c)
        buf.putShort(3, 0x7d7e)
        expect(buf.get(2)).toEqual(0x7b)
        expect(buf.get(3)).toEqual(0x7d)
        expect(buf.get(4)).toEqual(0x7e)
        expect(buf.get(5)).toEqual(0)

        buf.order(ByteOrder.LITTLE_ENDIAN)
        buf.position(7)
        buf.putShort(6, 0xffff8182)
        expect(buf.get(5)).toEqual(0)
        expect(buf.get(6)).toEqual(0x82.toByte)
        expect(buf.get(7)).toEqual(0x81.toByte)

        expect(() => buf.putShort(9, 0xffff8384)).toThrow
      }
    } else {
      it("absolute putShort() - read-only") {
        val buf = allocBuffer(10)
        expect(() => buf.putShort(3, 0x7576)).toThrow
        expect(buf.get(3)).toEqual(0)
        expect(buf.position()).toEqual(0)
      }
    }

    it("relative getInt()") {
      val buf = withContent(10, elemRange(0x7b, 0x85): _*)

      buf.order(ByteOrder.BIG_ENDIAN)
      expect(buf.getInt()).toEqual(0x7b7c7d7e)
      expect(buf.position).toEqual(4)
      expect(buf.getInt()).toEqual(0x7f808182)

      buf.order(ByteOrder.LITTLE_ENDIAN)
      buf.position(6)
      expect(buf.getInt()).toEqual(0x84838281)

      expect(() => buf.getInt()).toThrow
    }

    if (!createsReadOnly) {
      it("relative putInt()") {
        val buf = allocBuffer(10)
        buf.putInt(0x7b7c7d7e)
        expect(buf.position()).toEqual(4)
        expect(buf.get(0)).toEqual(0x7b)
        expect(buf.get(1)).toEqual(0x7c)
        expect(buf.get(2)).toEqual(0x7d)
        expect(buf.get(3)).toEqual(0x7e)
        buf.putInt(0x7f808182)
        expect(buf.get(4)).toEqual(0x7f)
        expect(buf.get(5)).toEqual(0x80.toByte)
        expect(buf.get(6)).toEqual(0x81.toByte)
        expect(buf.get(7)).toEqual(0x82.toByte)
        expect(buf.get(8)).toEqual(0)

        buf.order(ByteOrder.LITTLE_ENDIAN)
        buf.position(3)
        buf.putInt(0x81828384)
        expect(buf.get(2)).toEqual(0x7d)
        expect(buf.get(3)).toEqual(0x84.toByte)
        expect(buf.get(4)).toEqual(0x83.toByte)
        expect(buf.get(5)).toEqual(0x82.toByte)
        expect(buf.get(6)).toEqual(0x81.toByte)

        expect(() => buf.putInt(0xffff8384)).toThrow
      }
    } else {
      it("relative putInt() - read-only") {
        val buf = allocBuffer(10)
        expect(() => buf.putInt(0x75767778)).toThrow
        expect(buf.get(0)).toEqual(0)
        expect(buf.position()).toEqual(0)
      }
    }

    it("absolute getInt()") {
      val buf = withContent(10, elemRange(0x7b, 0x85): _*)

      buf.order(ByteOrder.BIG_ENDIAN)
      expect(buf.getInt(3)).toEqual(0x7e7f8081)
      expect(buf.position).toEqual(0)
      expect(buf.getInt(4)).toEqual(0x7f808182)

      buf.order(ByteOrder.LITTLE_ENDIAN)
      buf.position(6)
      expect(buf.getInt(2)).toEqual(0x807f7e7d)
      expect(buf.getInt(6)).toEqual(0x84838281)

      expect(() => buf.getInt(7)).toThrow
    }

    if (!createsReadOnly) {
      it("absolute putInt()") {
        val buf = allocBuffer(10)
        buf.putInt(2, 0x7b7c7d7e)
        expect(buf.position()).toEqual(0)
        expect(buf.get(0)).toEqual(0)
        expect(buf.get(2)).toEqual(0x7b)
        expect(buf.get(3)).toEqual(0x7c)
        expect(buf.get(4)).toEqual(0x7d)
        expect(buf.get(5)).toEqual(0x7e)
        buf.putInt(3, 0x7d7e7f80)
        expect(buf.get(2)).toEqual(0x7b)
        expect(buf.get(3)).toEqual(0x7d)
        expect(buf.get(4)).toEqual(0x7e)
        expect(buf.get(5)).toEqual(0x7f)
        expect(buf.get(6)).toEqual(0x80.toByte)
        expect(buf.get(7)).toEqual(0)

        buf.order(ByteOrder.LITTLE_ENDIAN)
        buf.position(7)
        buf.putInt(6, 0x81828384)
        expect(buf.get(5)).toEqual(0x7f)
        expect(buf.get(6)).toEqual(0x84.toByte)
        expect(buf.get(7)).toEqual(0x83.toByte)
        expect(buf.get(8)).toEqual(0x82.toByte)
        expect(buf.get(9)).toEqual(0x81.toByte)

        expect(() => buf.putInt(9, 0xffff8384)).toThrow
      }
    } else {
      it("absolute putInt() - read-only") {
        val buf = allocBuffer(10)
        expect(() => buf.putInt(3, 0x7576)).toThrow
        expect(buf.get(3)).toEqual(0)
        expect(buf.position()).toEqual(0)
      }
    }

    it("relative getLong()") {
      val buf = withContent(20, elemRange(0x76, 0x8a): _*)

      buf.order(ByteOrder.BIG_ENDIAN)
      expect(buf.getLong() == 0x767778797a7b7c7dL).toBeTruthy
      expect(buf.position).toEqual(8)
      expect(buf.getLong() == 0x7e7f808182838485L).toBeTruthy

      buf.order(ByteOrder.LITTLE_ENDIAN)
      buf.position(6)
      expect(buf.getLong() == 0x838281807f7e7d7cL).toBeTruthy

      expect(() => buf.getLong()).toThrow
    }

    if (!createsReadOnly) {
      it("relative putLong()") {
        val buf = allocBuffer(20)
        buf.putLong(0x767778797a7b7c7dL)
        expect(buf.position()).toEqual(8)
        expect(buf.get(0)).toEqual(0x76)
        expect(buf.get(1)).toEqual(0x77)
        expect(buf.get(2)).toEqual(0x78)
        expect(buf.get(3)).toEqual(0x79)
        expect(buf.get(4)).toEqual(0x7a)
        expect(buf.get(5)).toEqual(0x7b)
        expect(buf.get(6)).toEqual(0x7c)
        expect(buf.get(7)).toEqual(0x7d)
        buf.putLong(0x7e7f808182838485L)
        expect(buf.get(8)).toEqual(0x7e)
        expect(buf.get(9)).toEqual(0x7f)
        expect(buf.get(10)).toEqual(0x80.toByte)
        expect(buf.get(11)).toEqual(0x81.toByte)
        expect(buf.get(12)).toEqual(0x82.toByte)
        expect(buf.get(13)).toEqual(0x83.toByte)
        expect(buf.get(14)).toEqual(0x84.toByte)
        expect(buf.get(15)).toEqual(0x85.toByte)
        expect(buf.get(16)).toEqual(0)

        buf.order(ByteOrder.LITTLE_ENDIAN)
        buf.position(7)
        buf.putLong(0x8182838485868788L)
        expect(buf.get(6)).toEqual(0x7c)
        expect(buf.get(7)).toEqual(0x88.toByte)
        expect(buf.get(8)).toEqual(0x87.toByte)
        expect(buf.get(9)).toEqual(0x86.toByte)
        expect(buf.get(10)).toEqual(0x85.toByte)
        expect(buf.get(11)).toEqual(0x84.toByte)
        expect(buf.get(12)).toEqual(0x83.toByte)
        expect(buf.get(13)).toEqual(0x82.toByte)
        expect(buf.get(14)).toEqual(0x81.toByte)

        expect(() => buf.putLong(0xffff8384)).toThrow
      }
    } else {
      it("relative putLong() - read-only") {
        val buf = allocBuffer(20)
        expect(() => buf.putLong(0x75767778)).toThrow
        expect(buf.get(0)).toEqual(0)
        expect(buf.position()).toEqual(0)
      }
    }

    it("absolute getLong()") {
      val buf = withContent(20, elemRange(0x76, 0x8a): _*)

      buf.order(ByteOrder.BIG_ENDIAN)
      expect(buf.getLong(3) == 0x797a7b7c7d7e7f80L).toBeTruthy
      expect(buf.position).toEqual(0)
      expect(buf.getLong(6) == 0x7c7d7e7f80818283L).toBeTruthy

      buf.order(ByteOrder.LITTLE_ENDIAN)
      buf.position(6)
      expect(buf.getLong(8) == 0x8584838281807f7eL).toBeTruthy
      expect(buf.getLong(12) == 0x8988878685848382L).toBeTruthy

      expect(() => buf.getLong(15)).toThrow
    }

    if (!createsReadOnly) {
      it("absolute putLong()") {
        val buf = allocBuffer(20)
        buf.putLong(2, 0x7b7c7d7e7f808182L)
        expect(buf.position()).toEqual(0)
        expect(buf.get(0)).toEqual(0)
        expect(buf.get(2)).toEqual(0x7b)
        expect(buf.get(3)).toEqual(0x7c)
        expect(buf.get(4)).toEqual(0x7d)
        expect(buf.get(5)).toEqual(0x7e)
        expect(buf.get(6)).toEqual(0x7f)
        expect(buf.get(7)).toEqual(0x80.toByte)
        expect(buf.get(8)).toEqual(0x81.toByte)
        expect(buf.get(9)).toEqual(0x82.toByte)
        buf.putLong(7, 0x7d7e7f8081828384L)
        expect(buf.get(6)).toEqual(0x7f)
        expect(buf.get(7)).toEqual(0x7d)
        expect(buf.get(8)).toEqual(0x7e)
        expect(buf.get(9)).toEqual(0x7f)
        expect(buf.get(10)).toEqual(0x80.toByte)
        expect(buf.get(11)).toEqual(0x81.toByte)
        expect(buf.get(12)).toEqual(0x82.toByte)
        expect(buf.get(13)).toEqual(0x83.toByte)
        expect(buf.get(14)).toEqual(0x84.toByte)
        expect(buf.get(15)).toEqual(0)

        buf.order(ByteOrder.LITTLE_ENDIAN)
        buf.position(11)
        buf.putLong(9, 0x8182838485868788L)
        expect(buf.get(8)).toEqual(0x7e)
        expect(buf.get(9)).toEqual(0x88.toByte)
        expect(buf.get(10)).toEqual(0x87.toByte)
        expect(buf.get(11)).toEqual(0x86.toByte)
        expect(buf.get(12)).toEqual(0x85.toByte)
        expect(buf.get(13)).toEqual(0x84.toByte)
        expect(buf.get(14)).toEqual(0x83.toByte)
        expect(buf.get(15)).toEqual(0x82.toByte)
        expect(buf.get(16)).toEqual(0x81.toByte)

        expect(() => buf.putLong(16, 0xffff8384)).toThrow
      }
    } else {
      it("absolute putLong() - read-only") {
        val buf = allocBuffer(20)
        expect(() => buf.putLong(3, 0x7576)).toThrow
        expect(buf.get(3)).toEqual(0)
        expect(buf.position()).toEqual(0)
      }
    }

    it("relative getFloat()") {
      val buf = withContent(pos = 0, limit = 10, capacity = 10,
          0x40, 0x49, 0x0f, 0xd8.toByte, 0x43, 0x17, 0x30, 0x62, 0x4d, 0xab.toByte)

      buf.order(ByteOrder.BIG_ENDIAN)
      expect(buf.getFloat()).toEqual(3.141592f)
      expect(buf.position).toEqual(4)
      expect(buf.getFloat()).toEqual(151.189f)

      buf.order(ByteOrder.LITTLE_ENDIAN)
      buf.position(6)
      expect(buf.getFloat()).toEqual(-7.2966893e-13f)

      expect(() => buf.getFloat()).toThrow
    }

    if (!createsReadOnly) {
      it("relative putFloat()") {
        val buf = allocBuffer(10)
        buf.putFloat(3.141592f)
        expect(buf.position()).toEqual(4)
        expect(buf.get(0)).toEqual(0x40)
        expect(buf.get(1)).toEqual(0x49)
        expect(buf.get(2)).toEqual(0x0f)
        expect(buf.get(3)).toEqual(0xd8.toByte)
        buf.putFloat(151.189f)
        expect(buf.get(4)).toEqual(0x43)
        expect(buf.get(5)).toEqual(0x17)
        expect(buf.get(6)).toEqual(0x30)
        expect(buf.get(7)).toEqual(0x62)
        expect(buf.get(8)).toEqual(0)

        buf.order(ByteOrder.LITTLE_ENDIAN)
        buf.position(3)
        buf.putFloat(-7.2966893e-13f)
        expect(buf.get(2)).toEqual(0x0f)
        expect(buf.get(3)).toEqual(0x30)
        expect(buf.get(4)).toEqual(0x62)
        expect(buf.get(5)).toEqual(0x4d)
        expect(buf.get(6)).toEqual(0xab.toByte)

        expect(() => buf.putFloat(654.4f)).toThrow
      }
    } else {
      it("relative putFloat() - read-only") {
        val buf = allocBuffer(10)
        expect(() => buf.putFloat(151.189f)).toThrow
        expect(buf.get(0)).toEqual(0)
        expect(buf.position()).toEqual(0)
      }
    }

    it("absolute getFloat()") {
      val buf = withContent(pos = 0, limit = 10, capacity = 10,
          0x40, 0x49, 0x0f, 0xd8.toByte, 0x43, 0x17, 0x30, 0x62, 0x4d, 0xab.toByte)

      buf.order(ByteOrder.BIG_ENDIAN)
      expect(buf.getFloat(0)).toEqual(3.141592f)
      expect(buf.position).toEqual(0)
      expect(buf.getFloat(4)).toEqual(151.189f)

      buf.order(ByteOrder.LITTLE_ENDIAN)
      buf.position(8)
      expect(buf.getFloat(6)).toEqual(-7.2966893e-13f)

      expect(() => buf.getFloat(7)).toThrow
    }

    if (!createsReadOnly) {
      it("absolute putFloat()") {
        val buf = allocBuffer(10)
        buf.putFloat(2, 3.141592f)
        expect(buf.position()).toEqual(0)
        expect(buf.get(0)).toEqual(0)
        expect(buf.get(2)).toEqual(0x40)
        expect(buf.get(3)).toEqual(0x49)
        expect(buf.get(4)).toEqual(0x0f)
        expect(buf.get(5)).toEqual(0xd8.toByte)
        buf.putFloat(5, 151.189f)
        expect(buf.get(4)).toEqual(0x0f)
        expect(buf.get(5)).toEqual(0x43)
        expect(buf.get(6)).toEqual(0x17)
        expect(buf.get(7)).toEqual(0x30)
        expect(buf.get(8)).toEqual(0x62)
        expect(buf.get(9)).toEqual(0)

        buf.order(ByteOrder.LITTLE_ENDIAN)
        buf.position(7)
        buf.putFloat(5, -7.2966893e-13f)
        expect(buf.get(4)).toEqual(0x0f)
        expect(buf.get(5)).toEqual(0x30)
        expect(buf.get(6)).toEqual(0x62)
        expect(buf.get(7)).toEqual(0x4d)
        expect(buf.get(8)).toEqual(0xab.toByte)

        expect(() => buf.putFloat(9, 3.141592f)).toThrow
      }
    } else {
      it("absolute putFloat() - read-only") {
        val buf = allocBuffer(10)
        expect(() => buf.putFloat(3, 151.189f)).toThrow
        expect(buf.get(3)).toEqual(0)
        expect(buf.position()).toEqual(0)
      }
    }

    it("relative getDouble()") {
      val buf = withContent(pos = 0, limit = 20, capacity = 20,
          0x40, 0x09, 0x21, 0xfb.toByte, 0x54, 0x44, 0x2d, 0x18,
          0x40, 0x97.toByte, 0x9c.toByte, 0xcb.toByte, 0xac.toByte, 0x71, 0x0c, 0xb3.toByte,
          0x20, 0xe8.toByte, 0x74, 0xb5.toByte)

      buf.order(ByteOrder.BIG_ENDIAN)
      expect(buf.getDouble()).toEqual(Math.PI)
      expect(buf.position).toEqual(8)
      expect(buf.getDouble()).toEqual(1511.1989)

      buf.order(ByteOrder.LITTLE_ENDIAN)
      buf.position(12)
      expect(buf.getDouble()).toEqual(-3.492426300334232e-51)

      expect(() => buf.getDouble()).toThrow
    }

    if (!createsReadOnly) {
      it("relative putDouble()") {
        val buf = allocBuffer(20)
        buf.putDouble(Math.PI)
        expect(buf.position()).toEqual(8)
        expect(buf.get(0)).toEqual(0x40)
        expect(buf.get(1)).toEqual(0x09)
        expect(buf.get(2)).toEqual(0x21)
        expect(buf.get(3)).toEqual(0xfb.toByte)
        expect(buf.get(4)).toEqual(0x54)
        expect(buf.get(5)).toEqual(0x44)
        expect(buf.get(6)).toEqual(0x2d)
        expect(buf.get(7)).toEqual(0x18)
        buf.putDouble(1511.1989)
        expect(buf.get(8)).toEqual(0x40)
        expect(buf.get(9)).toEqual(0x97.toByte)
        expect(buf.get(10)).toEqual(0x9c.toByte)
        expect(buf.get(11)).toEqual(0xcb.toByte)
        expect(buf.get(12)).toEqual(0xac.toByte)
        expect(buf.get(13)).toEqual(0x71)
        expect(buf.get(14)).toEqual(0x0c)
        expect(buf.get(15)).toEqual(0xb3.toByte)
        expect(buf.get(16)).toEqual(0)

        buf.order(ByteOrder.LITTLE_ENDIAN)
        buf.position(7)
        buf.putDouble(-3.492426300334232e-51)
        expect(buf.get(6)).toEqual(0x2d)
        expect(buf.get(7)).toEqual(0xac.toByte)
        expect(buf.get(8)).toEqual(0x71)
        expect(buf.get(9)).toEqual(0x0c)
        expect(buf.get(10)).toEqual(0xb3.toByte)
        expect(buf.get(11)).toEqual(0x20)
        expect(buf.get(12)).toEqual(0xe8.toByte)
        expect(buf.get(13)).toEqual(0x74)
        expect(buf.get(14)).toEqual(0xb5.toByte)

        expect(() => buf.putDouble(1511.1989)).toThrow
      }
    } else {
      it("relative putDouble() - read-only") {
        val buf = allocBuffer(20)
        expect(() => buf.putDouble(1511.1989)).toThrow
        expect(buf.get(0)).toEqual(0)
        expect(buf.position()).toEqual(0)
      }
    }

    it("absolute getDouble()") {
      val buf = withContent(pos = 0, limit = 20, capacity = 20,
          0x40, 0x09, 0x21, 0xfb.toByte, 0x54, 0x44, 0x2d, 0x18,
          0x40, 0x97.toByte, 0x9c.toByte, 0xcb.toByte, 0xac.toByte, 0x71, 0x0c, 0xb3.toByte,
          0x20, 0xe8.toByte, 0x74, 0xb5.toByte)

      buf.order(ByteOrder.BIG_ENDIAN)
      expect(buf.getDouble(0)).toEqual(Math.PI)
      expect(buf.position).toEqual(0)
      expect(buf.getDouble(8)).toEqual(1511.1989)

      buf.order(ByteOrder.LITTLE_ENDIAN)
      buf.position(8)
      expect(buf.getDouble(12)).toEqual(-3.492426300334232e-51)

      expect(() => buf.getDouble(15)).toThrow
    }

    if (!createsReadOnly) {
      it("absolute putDouble()") {
        val buf = allocBuffer(20)
        buf.putDouble(2, Math.PI)
        expect(buf.position()).toEqual(0)
        expect(buf.get(0)).toEqual(0)
        expect(buf.get(2)).toEqual(0x40)
        expect(buf.get(3)).toEqual(0x09)
        expect(buf.get(4)).toEqual(0x21)
        expect(buf.get(5)).toEqual(0xfb.toByte)
        expect(buf.get(6)).toEqual(0x54)
        expect(buf.get(7)).toEqual(0x44)
        expect(buf.get(8)).toEqual(0x2d)
        expect(buf.get(9)).toEqual(0x18)
        buf.putDouble(5, 1511.1989)
        expect(buf.get(4)).toEqual(0x21)
        expect(buf.get(5)).toEqual(0x40)
        expect(buf.get(6)).toEqual(0x97.toByte)
        expect(buf.get(7)).toEqual(0x9c.toByte)
        expect(buf.get(8)).toEqual(0xcb.toByte)
        expect(buf.get(9)).toEqual(0xac.toByte)
        expect(buf.get(10)).toEqual(0x71)
        expect(buf.get(11)).toEqual(0x0c)
        expect(buf.get(12)).toEqual(0xb3.toByte)
        expect(buf.get(13)).toEqual(0)

        buf.order(ByteOrder.LITTLE_ENDIAN)
        buf.position(7)
        buf.putDouble(9, -3.492426300334232e-51)
        expect(buf.get(8)).toEqual(0xcb.toByte)
        expect(buf.get(9)).toEqual(0xac.toByte)
        expect(buf.get(10)).toEqual(0x71)
        expect(buf.get(11)).toEqual(0x0c)
        expect(buf.get(12)).toEqual(0xb3.toByte)
        expect(buf.get(13)).toEqual(0x20)
        expect(buf.get(14)).toEqual(0xe8.toByte)
        expect(buf.get(15)).toEqual(0x74)
        expect(buf.get(16)).toEqual(0xb5.toByte)

        expect(() => buf.putDouble(17, 1511.1989)).toThrow
      }
    } else {
      it("absolute putDouble() - read-only") {
        val buf = allocBuffer(20)
        expect(() => buf.putDouble(3, 1511.1989)).toThrow
        expect(buf.get(3)).toEqual(0)
        expect(buf.position()).toEqual(0)
      }
    }
  }

  class AllocByteBufferFactory extends Factory {
    def allocBuffer(capacity: Int): ByteBuffer =
      ByteBuffer.allocate(capacity)
  }

  class WrappedByteBufferFactory extends Factory with BufferFactory.WrappedBufferFactory {
    def baseWrap(array: Array[Byte]): ByteBuffer =
      ByteBuffer.wrap(array)

    def baseWrap(array: Array[Byte], offset: Int, length: Int): ByteBuffer =
      ByteBuffer.wrap(array, offset, length)
  }

  describe("Allocated ByteBuffer") {
    defineTests(new AllocByteBufferFactory)
  }

  describe("Wrapped ByteBuffer") {
    defineTests(new WrappedByteBufferFactory)
  }

  describe("Read-only wrapped ByteBuffer") {
    defineTests(new WrappedByteBufferFactory with BufferFactory.ReadOnlyBufferFactory)
  }

  describe("Sliced ByteBuffer") {
    defineTests(new AllocByteBufferFactory with BufferFactory.SlicedBufferFactory)
  }
}
