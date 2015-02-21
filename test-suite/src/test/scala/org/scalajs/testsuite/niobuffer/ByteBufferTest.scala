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

    it("asCharBuffer() - Bytes to Chars") {
      val buf = withContent(10, elemRange(0x7b, 0x85): _*)
      buf.limit(8).position(1)

      buf.order(ByteOrder.BIG_ENDIAN)
      val charBuf1 = buf.asCharBuffer()
      expect(charBuf1.isReadOnly).toEqual(createsReadOnly)
      expect(charBuf1.capacity).toEqual(3)
      expect(charBuf1.position).toEqual(0)
      expect(charBuf1.limit).toEqual(3)
      expect(charBuf1.order == ByteOrder.BIG_ENDIAN).toBeTruthy
      expect(charBuf1.get(1).toInt).toEqual(0x7e7f)
      expect(charBuf1.position).toEqual(0)
      expect(charBuf1.get().toInt).toEqual(0x7c7d)
      expect(charBuf1.position).toEqual(1)

      buf.order(ByteOrder.LITTLE_ENDIAN)
      val charBuf2 = buf.asCharBuffer()
      expect(charBuf2.isReadOnly).toEqual(createsReadOnly)
      expect(charBuf2.capacity).toEqual(3)
      expect(charBuf2.position).toEqual(0)
      expect(charBuf2.limit).toEqual(3)
      expect(charBuf2.order == ByteOrder.LITTLE_ENDIAN).toBeTruthy
      expect(charBuf2.get(1).toInt).toEqual(0x7f7e)
      expect(charBuf2.position).toEqual(0)
      expect(charBuf2.get().toInt).toEqual(0x7d7c)
      expect(charBuf2.position).toEqual(1)
    }

    if (!createsReadOnly) {
      it("asCharBuffer() - Chars to Bytes") {
        val buf = allocBuffer(10)
        buf.limit(8).position(1)

        buf.order(ByteOrder.BIG_ENDIAN)
        val charBuf1 = buf.asCharBuffer()
        charBuf1.put(1, 0x7e7f)
        expect(buf.get(3)).toEqual(0x7e)
        expect(buf.get(4)).toEqual(0x7f)
        expect(charBuf1.position).toEqual(0)
        charBuf1.put(0x7c7d.toChar)
        expect(buf.get(1)).toEqual(0x7c)
        expect(buf.get(2)).toEqual(0x7d)
        expect(charBuf1.position).toEqual(1)

        buf.order(ByteOrder.LITTLE_ENDIAN)
        val charBuf2 = buf.asCharBuffer()
        charBuf2.put(1, 0x7e7f)
        expect(buf.get(3)).toEqual(0x7f)
        expect(buf.get(4)).toEqual(0x7e)
        expect(charBuf2.position).toEqual(0)
        charBuf2.put(0x7c7d.toChar)
        expect(buf.get(1)).toEqual(0x7d)
        expect(buf.get(2)).toEqual(0x7c)
        expect(charBuf2.position).toEqual(1)
      }
    } else {
      it("asCharBuffer() - Chars to Bytes - read-only") {
        val buf = allocBuffer(10)
        buf.limit(8).position(1)

        val charBuf1 = buf.asReadOnlyBuffer().asCharBuffer()
        expect(() => charBuf1.put(1, 0x7e7f)).toThrow
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

    it("asShortBuffer() - Bytes to Shorts") {
      val buf = withContent(10, elemRange(0x7b, 0x85): _*)
      buf.limit(8).position(1)

      buf.order(ByteOrder.BIG_ENDIAN)
      val shortBuf1 = buf.asShortBuffer()
      expect(shortBuf1.isReadOnly).toEqual(createsReadOnly)
      expect(shortBuf1.capacity).toEqual(3)
      expect(shortBuf1.position).toEqual(0)
      expect(shortBuf1.limit).toEqual(3)
      expect(shortBuf1.order == ByteOrder.BIG_ENDIAN).toBeTruthy
      expect(shortBuf1.get(1)).toEqual(0x7e7f)
      expect(shortBuf1.position).toEqual(0)
      expect(shortBuf1.get()).toEqual(0x7c7d)
      expect(shortBuf1.position).toEqual(1)

      buf.order(ByteOrder.LITTLE_ENDIAN)
      val shortBuf2 = buf.asShortBuffer()
      expect(shortBuf2.isReadOnly).toEqual(createsReadOnly)
      expect(shortBuf2.capacity).toEqual(3)
      expect(shortBuf2.position).toEqual(0)
      expect(shortBuf2.limit).toEqual(3)
      expect(shortBuf2.order == ByteOrder.LITTLE_ENDIAN).toBeTruthy
      expect(shortBuf2.get(1)).toEqual(0x7f7e)
      expect(shortBuf2.position).toEqual(0)
      expect(shortBuf2.get()).toEqual(0x7d7c)
      expect(shortBuf2.position).toEqual(1)
    }

    if (!createsReadOnly) {
      it("asShortBuffer() - Shorts to Bytes") {
        val buf = allocBuffer(10)
        buf.limit(8).position(1)

        buf.order(ByteOrder.BIG_ENDIAN)
        val shortBuf1 = buf.asShortBuffer()
        shortBuf1.put(1, 0x7e7f)
        expect(buf.get(3)).toEqual(0x7e)
        expect(buf.get(4)).toEqual(0x7f)
        expect(shortBuf1.position).toEqual(0)
        shortBuf1.put(0x7c7d.toShort)
        expect(buf.get(1)).toEqual(0x7c)
        expect(buf.get(2)).toEqual(0x7d)
        expect(shortBuf1.position).toEqual(1)

        buf.order(ByteOrder.LITTLE_ENDIAN)
        val shortBuf2 = buf.asShortBuffer()
        shortBuf2.put(1, 0x7e7f)
        expect(buf.get(3)).toEqual(0x7f)
        expect(buf.get(4)).toEqual(0x7e)
        expect(shortBuf2.position).toEqual(0)
        shortBuf2.put(0x7c7d.toShort)
        expect(buf.get(1)).toEqual(0x7d)
        expect(buf.get(2)).toEqual(0x7c)
        expect(shortBuf2.position).toEqual(1)
      }
    } else {
      it("asShortBuffer() - Shorts to Bytes - read-only") {
        val buf = allocBuffer(10)
        buf.limit(8).position(1)

        val shortBuf1 = buf.asReadOnlyBuffer().asShortBuffer()
        expect(() => shortBuf1.put(1, 0x7e7f)).toThrow
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

    it("asIntBuffer() - Bytes to Ints") {
      val buf = withContent(14, elemRange(0x7b, 0x89): _*)
      buf.limit(10).position(1)

      buf.order(ByteOrder.BIG_ENDIAN)
      val intBuf1 = buf.asIntBuffer()
      expect(intBuf1.isReadOnly).toEqual(createsReadOnly)
      expect(intBuf1.capacity).toEqual(2)
      expect(intBuf1.position).toEqual(0)
      expect(intBuf1.limit).toEqual(2)
      expect(intBuf1.order == ByteOrder.BIG_ENDIAN).toBeTruthy
      expect(intBuf1.get(1)).toEqual(0x80818283)
      expect(intBuf1.position).toEqual(0)
      expect(intBuf1.get()).toEqual(0x7c7d7e7f)
      expect(intBuf1.position).toEqual(1)

      buf.order(ByteOrder.LITTLE_ENDIAN)
      val intBuf2 = buf.asIntBuffer()
      expect(intBuf2.isReadOnly).toEqual(createsReadOnly)
      expect(intBuf2.capacity).toEqual(2)
      expect(intBuf2.position).toEqual(0)
      expect(intBuf2.limit).toEqual(2)
      expect(intBuf2.order == ByteOrder.LITTLE_ENDIAN).toBeTruthy
      expect(intBuf2.get(1)).toEqual(0x83828180)
      expect(intBuf2.position).toEqual(0)
      expect(intBuf2.get()).toEqual(0x7f7e7d7c)
      expect(intBuf2.position).toEqual(1)
    }

    if (!createsReadOnly) {
      it("asIntBuffer() - Ints to Bytes") {
        val buf = allocBuffer(14)
        buf.limit(10).position(1)

        buf.order(ByteOrder.BIG_ENDIAN)
        val intBuf1 = buf.asIntBuffer()
        intBuf1.put(1, 0x81828384)
        expect(buf.get(5)).toEqual(0x81.toByte)
        expect(buf.get(6)).toEqual(0x82.toByte)
        expect(buf.get(7)).toEqual(0x83.toByte)
        expect(buf.get(8)).toEqual(0x84.toByte)
        expect(intBuf1.position).toEqual(0)
        intBuf1.put(0x7c7d7e7f)
        expect(buf.get(1)).toEqual(0x7c)
        expect(buf.get(2)).toEqual(0x7d)
        expect(buf.get(3)).toEqual(0x7e)
        expect(buf.get(4)).toEqual(0x7f)
        expect(intBuf1.position).toEqual(1)

        buf.order(ByteOrder.LITTLE_ENDIAN)
        val intBuf2 = buf.asIntBuffer()
        intBuf2.put(1, 0x81828384)
        expect(buf.get(5)).toEqual(0x84.toByte)
        expect(buf.get(6)).toEqual(0x83.toByte)
        expect(buf.get(7)).toEqual(0x82.toByte)
        expect(buf.get(8)).toEqual(0x81.toByte)
        expect(intBuf2.position).toEqual(0)
        intBuf2.put(0x7c7d7e7f)
        expect(buf.get(1)).toEqual(0x7f)
        expect(buf.get(2)).toEqual(0x7e)
        expect(buf.get(3)).toEqual(0x7d)
        expect(buf.get(4)).toEqual(0x7c)
        expect(intBuf2.position).toEqual(1)
      }
    } else {
      it("asIntBuffer() - Ints to Bytes - read-only") {
        val buf = allocBuffer(14)
        buf.limit(10).position(1)

        val intBuf1 = buf.asReadOnlyBuffer().asIntBuffer()
        expect(() => intBuf1.put(1, 0x7e7f8081)).toThrow
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

    it("asLongBuffer() - Bytes to Longs") {
      val buf = withContent(20, elemRange(0x76, 0x8a): _*)
      buf.limit(19).position(3)

      buf.order(ByteOrder.BIG_ENDIAN)
      val longBuf1 = buf.asLongBuffer()
      expect(longBuf1.isReadOnly).toEqual(createsReadOnly)
      expect(longBuf1.capacity).toEqual(2)
      expect(longBuf1.position).toEqual(0)
      expect(longBuf1.limit).toEqual(2)
      expect(longBuf1.order == ByteOrder.BIG_ENDIAN).toBeTruthy
      expect(longBuf1.get(1) == 0x8182838485868788L).toBeTruthy
      expect(longBuf1.position).toEqual(0)
      expect(longBuf1.get() == 0x797a7b7c7d7e7f80L).toBeTruthy
      expect(longBuf1.position).toEqual(1)

      buf.order(ByteOrder.LITTLE_ENDIAN)
      val longBuf2 = buf.asLongBuffer()
      expect(longBuf2.isReadOnly).toEqual(createsReadOnly)
      expect(longBuf2.capacity).toEqual(2)
      expect(longBuf2.position).toEqual(0)
      expect(longBuf2.limit).toEqual(2)
      expect(longBuf2.order == ByteOrder.LITTLE_ENDIAN).toBeTruthy
      expect(longBuf2.get(1) == 0x8887868584838281L).toBeTruthy
      expect(longBuf2.position).toEqual(0)
      expect(longBuf2.get() == 0x807f7e7d7c7b7a79L).toBeTruthy
      expect(longBuf2.position).toEqual(1)
    }

    if (!createsReadOnly) {
      it("asLongBuffer() - Longs to Bytes") {
        val buf = allocBuffer(20)
        buf.limit(19).position(3)

        buf.order(ByteOrder.BIG_ENDIAN)
        val longBuf1 = buf.asLongBuffer()
        longBuf1.put(1, 0x8182838485868788L)
        expect(buf.get(11)).toEqual(0x81.toByte)
        expect(buf.get(12)).toEqual(0x82.toByte)
        expect(buf.get(13)).toEqual(0x83.toByte)
        expect(buf.get(14)).toEqual(0x84.toByte)
        expect(buf.get(15)).toEqual(0x85.toByte)
        expect(buf.get(16)).toEqual(0x86.toByte)
        expect(buf.get(17)).toEqual(0x87.toByte)
        expect(buf.get(18)).toEqual(0x88.toByte)
        expect(longBuf1.position).toEqual(0)
        longBuf1.put(0x797a7b7c7d7e7f80L)
        expect(buf.get(3)).toEqual(0x79)
        expect(buf.get(4)).toEqual(0x7a)
        expect(buf.get(5)).toEqual(0x7b)
        expect(buf.get(6)).toEqual(0x7c)
        expect(buf.get(7)).toEqual(0x7d)
        expect(buf.get(8)).toEqual(0x7e)
        expect(buf.get(9)).toEqual(0x7f)
        expect(buf.get(10)).toEqual(0x80.toByte)
        expect(longBuf1.position).toEqual(1)

        buf.order(ByteOrder.LITTLE_ENDIAN)
        val longBuf2 = buf.asLongBuffer()
        longBuf2.put(1, 0x8182838485868788L)
        expect(buf.get(11)).toEqual(0x88.toByte)
        expect(buf.get(12)).toEqual(0x87.toByte)
        expect(buf.get(13)).toEqual(0x86.toByte)
        expect(buf.get(14)).toEqual(0x85.toByte)
        expect(buf.get(15)).toEqual(0x84.toByte)
        expect(buf.get(16)).toEqual(0x83.toByte)
        expect(buf.get(17)).toEqual(0x82.toByte)
        expect(buf.get(18)).toEqual(0x81.toByte)
        expect(longBuf2.position).toEqual(0)
        longBuf2.put(0x797a7b7c7d7e7f80L)
        expect(buf.get(3)).toEqual(0x80.toByte)
        expect(buf.get(4)).toEqual(0x7f)
        expect(buf.get(5)).toEqual(0x7e)
        expect(buf.get(6)).toEqual(0x7d)
        expect(buf.get(7)).toEqual(0x7c)
        expect(buf.get(8)).toEqual(0x7b)
        expect(buf.get(9)).toEqual(0x7a)
        expect(buf.get(10)).toEqual(0x79)
        expect(longBuf2.position).toEqual(1)
      }
    } else {
      it("asLongBuffer() - Longs to Bytes - read-only") {
        val buf = allocBuffer(20)
        buf.limit(19).position(3)

        val longBuf1 = buf.asReadOnlyBuffer().asLongBuffer()
        expect(() => longBuf1.put(1, 0x8182838485868788L)).toThrow
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

    it("asFloatBuffer() - Bytes to Floats") {
      val buf = withContent(pos = 0, limit = 12, capacity = 12,
          0x10, 0x23,
          0x40, 0x49, 0x0f, 0xd8.toByte, 0x62, 0x30, 0x17, 0x43,
          0x4d, 0xab.toByte)
      buf.limit(11).position(2)

      buf.order(ByteOrder.BIG_ENDIAN)
      val floatBuf1 = buf.asFloatBuffer()
      expect(floatBuf1.isReadOnly).toEqual(createsReadOnly)
      expect(floatBuf1.capacity).toEqual(2)
      expect(floatBuf1.position).toEqual(0)
      expect(floatBuf1.limit).toEqual(2)
      expect(floatBuf1.order == ByteOrder.BIG_ENDIAN).toBeTruthy
      expect(floatBuf1.get(1)).toEqual(8.120758e20f)
      expect(floatBuf1.position).toEqual(0)
      expect(floatBuf1.get()).toEqual(3.141592f)
      expect(floatBuf1.position).toEqual(1)

      buf.order(ByteOrder.LITTLE_ENDIAN)
      val floatBuf2 = buf.asFloatBuffer()
      expect(floatBuf2.isReadOnly).toEqual(createsReadOnly)
      expect(floatBuf2.capacity).toEqual(2)
      expect(floatBuf2.position).toEqual(0)
      expect(floatBuf2.limit).toEqual(2)
      expect(floatBuf2.order == ByteOrder.LITTLE_ENDIAN).toBeTruthy
      expect(floatBuf2.get(1)).toEqual(151.189f)
      expect(floatBuf2.position).toEqual(0)
      expect(floatBuf2.get()).toEqual(-6.3017908e14f)
      expect(floatBuf2.position).toEqual(1)
    }

    if (!createsReadOnly) {
      it("asFloatBuffer() - Floats to Bytes") {
        val buf = allocBuffer(14)
        buf.limit(10).position(1)

        buf.order(ByteOrder.BIG_ENDIAN)
        val floatBuf1 = buf.asFloatBuffer()
        floatBuf1.put(1, 3.141592f)
        expect(buf.get(5)).toEqual(0x40)
        expect(buf.get(6)).toEqual(0x49)
        expect(buf.get(7)).toEqual(0x0f)
        expect(buf.get(8)).toEqual(0xd8.toByte)
        expect(floatBuf1.position).toEqual(0)
        floatBuf1.put(151.189f)
        expect(buf.get(1)).toEqual(0x43)
        expect(buf.get(2)).toEqual(0x17)
        expect(buf.get(3)).toEqual(0x30)
        expect(buf.get(4)).toEqual(0x62)
        expect(floatBuf1.position).toEqual(1)

        buf.order(ByteOrder.LITTLE_ENDIAN)
        val floatBuf2 = buf.asFloatBuffer()
        floatBuf2.put(1, 3.141592f)
        expect(buf.get(5)).toEqual(0xd8.toByte)
        expect(buf.get(6)).toEqual(0x0f)
        expect(buf.get(7)).toEqual(0x49)
        expect(buf.get(8)).toEqual(0x40)
        expect(floatBuf2.position).toEqual(0)
        floatBuf2.put(151.189f)
        expect(buf.get(1)).toEqual(0x62)
        expect(buf.get(2)).toEqual(0x30)
        expect(buf.get(3)).toEqual(0x17)
        expect(buf.get(4)).toEqual(0x43)
        expect(floatBuf2.position).toEqual(1)
      }
    } else {
      it("asFloatBuffer() - Floats to Bytes - read-only") {
        val buf = allocBuffer(14)
        buf.limit(10).position(1)

        val floatBuf1 = buf.asReadOnlyBuffer().asFloatBuffer()
        expect(() => floatBuf1.put(1, 3.141592f)).toThrow
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

    it("asDoubleBuffer() - Bytes to Doubles") {
      val buf = withContent(pos = 0, limit = 20, capacity = 20,
          0x20, 0xe8.toByte,
          0x40, 0x09, 0x21, 0xfb.toByte, 0x54, 0x44, 0x2d, 0x18,
          0xb3.toByte, 0x0c, 0x71, 0xac.toByte, 0xcb.toByte, 0x9c.toByte, 0x97.toByte, 0x40,
          0x74, 0xb5.toByte)
      buf.limit(19).position(2)

      buf.order(ByteOrder.BIG_ENDIAN)
      val doubleBuf1 = buf.asDoubleBuffer()
      expect(doubleBuf1.isReadOnly).toEqual(createsReadOnly)
      expect(doubleBuf1.capacity).toEqual(2)
      expect(doubleBuf1.position).toEqual(0)
      expect(doubleBuf1.limit).toEqual(2)
      expect(doubleBuf1.order == ByteOrder.BIG_ENDIAN).toBeTruthy
      expect(doubleBuf1.get(1)).toEqual(-8.642954761616149e-63)
      expect(doubleBuf1.position).toEqual(0)
      expect(doubleBuf1.get()).toEqual(Math.PI)
      expect(doubleBuf1.position).toEqual(1)

      buf.order(ByteOrder.LITTLE_ENDIAN)
      val doubleBuf2 = buf.asDoubleBuffer()
      expect(doubleBuf2.isReadOnly).toEqual(createsReadOnly)
      expect(doubleBuf2.capacity).toEqual(2)
      expect(doubleBuf2.position).toEqual(0)
      expect(doubleBuf2.limit).toEqual(2)
      expect(doubleBuf2.order == ByteOrder.LITTLE_ENDIAN).toBeTruthy
      expect(doubleBuf2.get(1)).toEqual(1511.1989)
      expect(doubleBuf2.position).toEqual(0)
      expect(doubleBuf2.get()).toEqual(3.207375630676366e-192)
      expect(doubleBuf2.position).toEqual(1)
    }

    if (!createsReadOnly) {
      it("asDoubleBuffer() - Doubles to Bytes") {
        val buf = allocBuffer(20)
        buf.limit(19).position(3)

        buf.order(ByteOrder.BIG_ENDIAN)
        val doubleBuf1 = buf.asDoubleBuffer()
        doubleBuf1.put(1, Math.PI)
        expect(buf.get(11)).toEqual(0x40)
        expect(buf.get(12)).toEqual(0x09)
        expect(buf.get(13)).toEqual(0x21)
        expect(buf.get(14)).toEqual(0xfb.toByte)
        expect(buf.get(15)).toEqual(0x54)
        expect(buf.get(16)).toEqual(0x44)
        expect(buf.get(17)).toEqual(0x2d)
        expect(buf.get(18)).toEqual(0x18)
        expect(doubleBuf1.position).toEqual(0)
        doubleBuf1.put(1511.1989)
        expect(buf.get(3)).toEqual(0x40)
        expect(buf.get(4)).toEqual(0x97.toByte)
        expect(buf.get(5)).toEqual(0x9c.toByte)
        expect(buf.get(6)).toEqual(0xcb.toByte)
        expect(buf.get(7)).toEqual(0xac.toByte)
        expect(buf.get(8)).toEqual(0x71)
        expect(buf.get(9)).toEqual(0x0c)
        expect(buf.get(10)).toEqual(0xb3.toByte)
        expect(doubleBuf1.position).toEqual(1)

        buf.order(ByteOrder.LITTLE_ENDIAN)
        val doubleBuf2 = buf.asDoubleBuffer()
        doubleBuf2.put(1, Math.PI)
        expect(buf.get(11)).toEqual(0x18)
        expect(buf.get(12)).toEqual(0x2d)
        expect(buf.get(13)).toEqual(0x44)
        expect(buf.get(14)).toEqual(0x54)
        expect(buf.get(15)).toEqual(0xfb.toByte)
        expect(buf.get(16)).toEqual(0x21)
        expect(buf.get(17)).toEqual(0x09)
        expect(buf.get(18)).toEqual(0x40)
        expect(doubleBuf2.position).toEqual(0)
        doubleBuf2.put(1511.1989)
        expect(buf.get(3)).toEqual(0xb3.toByte)
        expect(buf.get(4)).toEqual(0x0c)
        expect(buf.get(5)).toEqual(0x71)
        expect(buf.get(6)).toEqual(0xac.toByte)
        expect(buf.get(7)).toEqual(0xcb.toByte)
        expect(buf.get(8)).toEqual(0x9c.toByte)
        expect(buf.get(9)).toEqual(0x97.toByte)
        expect(buf.get(10)).toEqual(0x40)
        expect(doubleBuf2.position).toEqual(1)
      }
    } else {
      it("asDoubleBuffer() - Doubles to Bytes - read-only") {
        val buf = allocBuffer(20)
        buf.limit(19).position(3)

        val doubleBuf1 = buf.asReadOnlyBuffer().asDoubleBuffer()
        expect(() => doubleBuf1.put(1, Math.PI)).toThrow
      }
    }
  }

  for ((description, factory) <- ByteBufferFactories.AllByteBufferFactories) {
    describe(description) {
      defineTests(factory)
    }
  }
}
