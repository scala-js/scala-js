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

package org.scalajs.testsuite.niobuffer

import java.nio._

import org.junit.Test
import org.junit.Assert._
import org.scalajs.testsuite.niobuffer.BufferFactory.ByteBufferFactory

import org.scalajs.testsuite.utils.AssertThrows.assertThrows

abstract class ByteBufferTest extends BaseBufferTest {
  type Factory = BufferFactory.ByteBufferFactory

  import factory._

  @Test def order(): Unit = {
    val buf = allocBuffer(10)
    assertEquals(ByteOrder.BIG_ENDIAN, buf.order())
    buf.order(ByteOrder.LITTLE_ENDIAN)
    assertEquals(ByteOrder.LITTLE_ENDIAN, buf.order())
    buf.order(ByteOrder.BIG_ENDIAN)
    assertEquals(ByteOrder.BIG_ENDIAN, buf.order())
  }

  @Test def relativeGetChar(): Unit = {
    val buf = withContent(10, elemRange(0x7b, 0x85): _*)

    buf.order(ByteOrder.BIG_ENDIAN)
    assertEquals(0x7b7c, buf.getChar().toInt)
    assertEquals(2, buf.position())
    assertEquals(0x7d7e, buf.getChar().toInt)

    buf.order(ByteOrder.LITTLE_ENDIAN)
    buf.position(6)
    assertEquals(0x8281, buf.getChar().toInt)
    assertEquals(0x8483, buf.getChar().toInt)

    assertThrows(classOf[BufferUnderflowException], buf.getChar())
  }

  @Test def relativePutChar(): Unit = {
    val buf = allocBuffer(10)
    if (!createsReadOnly) {
      buf.putChar(0x7b7c)
      assertEquals(2, buf.position())
      assertEquals(0x7b, buf.get(0))
      assertEquals(0x7c, buf.get(1))
      buf.putChar(0x7d7e)
      assertEquals(0x7d, buf.get(2))
      assertEquals(0x7e, buf.get(3))
      assertEquals(0, buf.get(4))

      buf.order(ByteOrder.LITTLE_ENDIAN)
      buf.position(7)
      buf.putChar(0x8182)
      assertEquals(0, buf.get(6))
      assertEquals(0x82.toByte, buf.get(7))
      assertEquals(0x81.toByte, buf.get(8))

      assertThrows(classOf[BufferOverflowException], buf.putChar(0x8384))
    } else {
      assertThrows(classOf[ReadOnlyBufferException], buf.putChar(0x7576))
      assertEquals(0, buf.get(0))
      assertEquals(0, buf.position())
    }
  }

  @Test def absoluteGetChar(): Unit = {
    val buf = withContent(10, elemRange(0x7b, 0x85): _*)

    buf.order(ByteOrder.BIG_ENDIAN)
    assertEquals(0x7e7f, buf.getChar(3).toInt)
    assertEquals(0, buf.position())
    assertEquals(0x7f80, buf.getChar(4).toInt)

    buf.order(ByteOrder.LITTLE_ENDIAN)
    buf.position(6)
    assertEquals(0x7e7d, buf.getChar(2).toInt)
    assertEquals(0x8483, buf.getChar(8).toInt)

    assertThrows(classOf[IndexOutOfBoundsException], buf.getChar(9))
  }

  @Test def absolutePutChar(): Unit = {
    if (!createsReadOnly) {
      val buf = allocBuffer(10)
      buf.putChar(2, 0x7b7c)
      assertEquals(0, buf.position())
      assertEquals(0, buf.get(0))
      assertEquals(0x7b, buf.get(2))
      assertEquals(0x7c, buf.get(3))
      buf.putChar(3, 0x7d7e)
      assertEquals(0x7b, buf.get(2))
      assertEquals(0x7d, buf.get(3))
      assertEquals(0x7e, buf.get(4))
      assertEquals(0, buf.get(5))

      buf.order(ByteOrder.LITTLE_ENDIAN)
      buf.position(7)
      buf.putChar(6, 0x8182)
      assertEquals(0, buf.get(5))
      assertEquals(0x82.toByte, buf.get(6))
      assertEquals(0x81.toByte, buf.get(7))

      assertThrows(classOf[IndexOutOfBoundsException], buf.putChar(9, 0x8384))
    } else {
      val buf = allocBuffer(10)
      assertThrows(classOf[ReadOnlyBufferException], buf.putChar(3, 0x7576))
      assertEquals(0, buf.get(3))
      assertEquals(0, buf.position())
    }
  }

  @Test def asCharBufferBytesToChars(): Unit = {
    val buf = withContent(10, elemRange(0x7b, 0x85): _*)
    buf.limit(8).position(1)

    buf.order(ByteOrder.BIG_ENDIAN)
    val charBuf1 = buf.asCharBuffer()
    assertEquals(createsReadOnly, charBuf1.isReadOnly)
    assertEquals(3, charBuf1.capacity)
    assertEquals(0, charBuf1.position())
    assertEquals(3, charBuf1.limit())
    assertEquals(ByteOrder.BIG_ENDIAN, charBuf1.order)
    assertEquals(0x7e7f, charBuf1.get(1).toInt)
    assertEquals(0, charBuf1.position())
    assertEquals(0x7c7d, charBuf1.get().toInt)
    assertEquals(1, charBuf1.position())

    buf.order(ByteOrder.LITTLE_ENDIAN)
    val charBuf2 = buf.asCharBuffer()
    assertEquals(createsReadOnly, charBuf2.isReadOnly)
    assertEquals(3, charBuf2.capacity)
    assertEquals(0, charBuf2.position())
    assertEquals(3, charBuf2.limit())
    assertEquals(ByteOrder.LITTLE_ENDIAN, charBuf2.order)
    assertEquals(0x7f7e, charBuf2.get(1).toInt)
    assertEquals(0, charBuf2.position())
    assertEquals(0x7d7c, charBuf2.get().toInt)
    assertEquals(1, charBuf2.position())
  }

  @Test def asCharBufferCharsToBytes(): Unit = {
    if (!createsReadOnly) {
      val buf = allocBuffer(10)
      buf.limit(8).position(1)

      buf.order(ByteOrder.BIG_ENDIAN)
      val charBuf1 = buf.asCharBuffer()
      charBuf1.put(1, 0x7e7f.toChar)
      assertEquals(0x7e, buf.get(3))
      assertEquals(0x7f, buf.get(4))
      assertEquals(0, charBuf1.position())
      charBuf1.put(0x7c7d.toChar)
      assertEquals(0x7c, buf.get(1))
      assertEquals(0x7d, buf.get(2))
      assertEquals(1, charBuf1.position())

      buf.order(ByteOrder.LITTLE_ENDIAN)
      val charBuf2 = buf.asCharBuffer()
      charBuf2.put(1, 0x7e7f.toChar)
      assertEquals(0x7f, buf.get(3))
      assertEquals(0x7e, buf.get(4))
      assertEquals(0, charBuf2.position())
      charBuf2.put(0x7c7d.toChar)
      assertEquals(0x7d, buf.get(1))
      assertEquals(0x7c, buf.get(2))
      assertEquals(1, charBuf2.position())
    } else {
      val buf = allocBuffer(10)
      buf.limit(8).position(1)

      val charBuf1 = buf.asReadOnlyBuffer().asCharBuffer()
      assertThrows(classOf[ReadOnlyBufferException], charBuf1.put(1, 0x7e7f.toChar))
    }
  }

  @Test def relativeGetShort(): Unit = {
    val buf = withContent(10, elemRange(0x7b, 0x85): _*)

    buf.order(ByteOrder.BIG_ENDIAN)
    assertEquals(0x7b7c, buf.getShort())
    assertEquals(2, buf.position())
    assertEquals(0x7d7e, buf.getShort())

    buf.order(ByteOrder.LITTLE_ENDIAN)
    buf.position(6)
    assertEquals(0xffff8281, buf.getShort())
    assertEquals(0xffff8483, buf.getShort())

    assertThrows(classOf[BufferUnderflowException], buf.getShort())
  }

  @Test def relativePutShort(): Unit = {
    if (!createsReadOnly) {
      val buf = allocBuffer(10)
      buf.putShort(0x7b7c)
      assertEquals(2, buf.position())
      assertEquals(0x7b, buf.get(0))
      assertEquals(0x7c, buf.get(1))
      buf.putShort(0x7d7e)
      assertEquals(0x7d, buf.get(2))
      assertEquals(0x7e, buf.get(3))
      assertEquals(0, buf.get(4))

      buf.order(ByteOrder.LITTLE_ENDIAN)
      buf.position(7)
      buf.putShort(0xffff8182)
      assertEquals(0, buf.get(6))
      assertEquals(0x82.toByte, buf.get(7))
      assertEquals(0x81.toByte, buf.get(8))

      assertThrows(classOf[BufferOverflowException], buf.putShort(0xffff8384))
    } else {
      val buf = allocBuffer(10)
      assertThrows(classOf[ReadOnlyBufferException], buf.putShort(0x7576))
      assertEquals(0, buf.get(0))
      assertEquals(0, buf.position())
    }
  }

  @Test def absoluteGetShort(): Unit = {
    val buf = withContent(10, elemRange(0x7b, 0x85): _*)

    buf.order(ByteOrder.BIG_ENDIAN)
    assertEquals(0x7e7f, buf.getShort(3))
    assertEquals(0, buf.position())
    assertEquals(0x7f80, buf.getShort(4))

    buf.order(ByteOrder.LITTLE_ENDIAN)
    buf.position(6)
    assertEquals(0x7e7d, buf.getShort(2))
    assertEquals(0xffff8483, buf.getShort(8))

    assertThrows(classOf[IndexOutOfBoundsException], buf.getShort(9))
  }

  @Test def absolutePutShort(): Unit = {
    if (!createsReadOnly) {
      val buf = allocBuffer(10)
      buf.putShort(2, 0x7b7c)
      assertEquals(0, buf.position())
      assertEquals(0, buf.get(0))
      assertEquals(0x7b, buf.get(2))
      assertEquals(0x7c, buf.get(3))
      buf.putShort(3, 0x7d7e)
      assertEquals(0x7b, buf.get(2))
      assertEquals(0x7d, buf.get(3))
      assertEquals(0x7e, buf.get(4))
      assertEquals(0, buf.get(5))

      buf.order(ByteOrder.LITTLE_ENDIAN)
      buf.position(7)
      buf.putShort(6, 0xffff8182)
      assertEquals(0, buf.get(5))
      assertEquals(0x82.toByte, buf.get(6))
      assertEquals(0x81.toByte, buf.get(7))

      assertThrows(classOf[IndexOutOfBoundsException], buf.putShort(9, 0xffff8384))
    } else {
      val buf = allocBuffer(10)
      assertThrows(classOf[ReadOnlyBufferException], buf.putShort(3, 0x7576))
      assertEquals(0, buf.get(3))
      assertEquals(0, buf.position())
    }
  }

  @Test def asShortBufferBytesToShorts(): Unit = {
    val buf = withContent(10, elemRange(0x7b, 0x85): _*)
    buf.limit(8).position(1)

    buf.order(ByteOrder.BIG_ENDIAN)
    val shortBuf1 = buf.asShortBuffer()
    assertEquals(createsReadOnly, shortBuf1.isReadOnly)
    assertEquals(3, shortBuf1.capacity)
    assertEquals(0, shortBuf1.position())
    assertEquals(3, shortBuf1.limit())
    assertEquals(ByteOrder.BIG_ENDIAN, shortBuf1.order)
    assertEquals(0x7e7f, shortBuf1.get(1))
    assertEquals(0, shortBuf1.position())
    assertEquals(0x7c7d, shortBuf1.get())
    assertEquals(1, shortBuf1.position())

    buf.order(ByteOrder.LITTLE_ENDIAN)
    val shortBuf2 = buf.asShortBuffer()
    assertEquals(createsReadOnly, shortBuf2.isReadOnly)
    assertEquals(3, shortBuf2.capacity)
    assertEquals(0, shortBuf2.position())
    assertEquals(3, shortBuf2.limit())
    assertEquals(ByteOrder.LITTLE_ENDIAN, shortBuf2.order)
    assertEquals(0x7f7e, shortBuf2.get(1))
    assertEquals(0, shortBuf2.position())
    assertEquals(0x7d7c, shortBuf2.get())
    assertEquals(1, shortBuf2.position())
  }

  @Test def asShortBufferShortsToBytes(): Unit = {
    if (!createsReadOnly) {
      val buf = allocBuffer(10)
      buf.limit(8).position(1)

      buf.order(ByteOrder.BIG_ENDIAN)
      val shortBuf1 = buf.asShortBuffer()
      shortBuf1.put(1, 0x7e7f.toShort)
      assertEquals(0x7e, buf.get(3))
      assertEquals(0x7f, buf.get(4))
      assertEquals(0, shortBuf1.position())
      shortBuf1.put(0x7c7d.toShort)
      assertEquals(0x7c, buf.get(1))
      assertEquals(0x7d, buf.get(2))
      assertEquals(1, shortBuf1.position())

      buf.order(ByteOrder.LITTLE_ENDIAN)
      val shortBuf2 = buf.asShortBuffer()
      shortBuf2.put(1, 0x7e7f.toShort)
      assertEquals(0x7f, buf.get(3))
      assertEquals(0x7e, buf.get(4))
      assertEquals(0, shortBuf2.position())
      shortBuf2.put(0x7c7d.toShort)
      assertEquals(0x7d, buf.get(1))
      assertEquals(0x7c, buf.get(2))
      assertEquals(1, shortBuf2.position())
    } else {
      val buf = allocBuffer(10)
      buf.limit(8).position(1)

      val shortBuf1 = buf.asReadOnlyBuffer().asShortBuffer()
      assertThrows(classOf[ReadOnlyBufferException], shortBuf1.put(1, 0x7e7f.toShort))
    }
  }

  @Test def relativeGetInt(): Unit = {
    val buf = withContent(10, elemRange(0x7b, 0x85): _*)

    buf.order(ByteOrder.BIG_ENDIAN)
    assertEquals(0x7b7c7d7e, buf.getInt())
    assertEquals(4, buf.position())
    assertEquals(0x7f808182, buf.getInt())

    buf.order(ByteOrder.LITTLE_ENDIAN)
    buf.position(6)
    assertEquals(0x84838281, buf.getInt())

    assertThrows(classOf[BufferUnderflowException], buf.getInt())
  }

  @Test def relativePutInt(): Unit = {
    if (!createsReadOnly) {
      val buf = allocBuffer(10)
      buf.putInt(0x7b7c7d7e)
      assertEquals(4, buf.position())
      assertEquals(0x7b, buf.get(0))
      assertEquals(0x7c, buf.get(1))
      assertEquals(0x7d, buf.get(2))
      assertEquals(0x7e, buf.get(3))
      buf.putInt(0x7f808182)
      assertEquals(0x7f, buf.get(4))
      assertEquals(0x80.toByte, buf.get(5))
      assertEquals(0x81.toByte, buf.get(6))
      assertEquals(0x82.toByte, buf.get(7))
      assertEquals(0, buf.get(8))

      buf.order(ByteOrder.LITTLE_ENDIAN)
      buf.position(3)
      buf.putInt(0x81828384)
      assertEquals(0x7d, buf.get(2))
      assertEquals(0x84.toByte, buf.get(3))
      assertEquals(0x83.toByte, buf.get(4))
      assertEquals(0x82.toByte, buf.get(5))
      assertEquals(0x81.toByte, buf.get(6))

      assertThrows(classOf[BufferOverflowException], buf.putInt(0xffff8384))
    } else {
      val buf = allocBuffer(10)
      assertThrows(classOf[ReadOnlyBufferException], buf.putInt(0x75767778))
      assertEquals(0, buf.get(0))
      assertEquals(0, buf.position())
    }
  }

  @Test def absoluteGetInt(): Unit = {
    val buf = withContent(10, elemRange(0x7b, 0x85): _*)

    buf.order(ByteOrder.BIG_ENDIAN)
    assertEquals(0x7e7f8081, buf.getInt(3))
    assertEquals(0, buf.position())
    assertEquals(0x7f808182, buf.getInt(4))

    buf.order(ByteOrder.LITTLE_ENDIAN)
    buf.position(6)
    assertEquals(0x807f7e7d, buf.getInt(2))
    assertEquals(0x84838281, buf.getInt(6))

    assertThrows(classOf[IndexOutOfBoundsException], buf.getInt(7))
  }

  @Test def absolutePutInt(): Unit = {
    if (!createsReadOnly) {
      val buf = allocBuffer(10)
      buf.putInt(2, 0x7b7c7d7e)
      assertEquals(0, buf.position())
      assertEquals(0, buf.get(0))
      assertEquals(0x7b, buf.get(2))
      assertEquals(0x7c, buf.get(3))
      assertEquals(0x7d, buf.get(4))
      assertEquals(0x7e, buf.get(5))
      buf.putInt(3, 0x7d7e7f80)
      assertEquals(0x7b, buf.get(2))
      assertEquals(0x7d, buf.get(3))
      assertEquals(0x7e, buf.get(4))
      assertEquals(0x7f, buf.get(5))
      assertEquals(0x80.toByte, buf.get(6))
      assertEquals(0, buf.get(7))

      buf.order(ByteOrder.LITTLE_ENDIAN)
      buf.position(7)
      buf.putInt(6, 0x81828384)
      assertEquals(0x7f, buf.get(5))
      assertEquals(0x84.toByte, buf.get(6))
      assertEquals(0x83.toByte, buf.get(7))
      assertEquals(0x82.toByte, buf.get(8))
      assertEquals(0x81.toByte, buf.get(9))

      assertThrows(classOf[IndexOutOfBoundsException], buf.putInt(9, 0xffff8384))
    } else {
      val buf = allocBuffer(10)
      assertThrows(classOf[ReadOnlyBufferException], buf.putInt(3, 0x7576))
      assertEquals(0, buf.get(3))
      assertEquals(0, buf.position())
    }
  }

  @Test def asIntBufferBytesToInts(): Unit = {
    val buf = withContent(14, elemRange(0x7b, 0x89): _*)
    buf.limit(10).position(1)

    buf.order(ByteOrder.BIG_ENDIAN)
    val intBuf1 = buf.asIntBuffer()
    assertEquals(createsReadOnly, intBuf1.isReadOnly)
    assertEquals(2, intBuf1.capacity)
    assertEquals(0, intBuf1.position())
    assertEquals(2, intBuf1.limit())
    assertEquals(ByteOrder.BIG_ENDIAN, intBuf1.order)
    assertEquals(0x80818283, intBuf1.get(1))
    assertEquals(0, intBuf1.position())
    assertEquals(0x7c7d7e7f, intBuf1.get())
    assertEquals(1, intBuf1.position())

    buf.order(ByteOrder.LITTLE_ENDIAN)
    val intBuf2 = buf.asIntBuffer()
    assertEquals(createsReadOnly, intBuf2.isReadOnly)
    assertEquals(2, intBuf2.capacity)
    assertEquals(0, intBuf2.position())
    assertEquals(2, intBuf2.limit())
    assertEquals(ByteOrder.LITTLE_ENDIAN, intBuf2.order)
    assertEquals(0x83828180, intBuf2.get(1))
    assertEquals(0, intBuf2.position())
    assertEquals(0x7f7e7d7c, intBuf2.get())
    assertEquals(1, intBuf2.position())
  }

  @Test def asIntBufferIntsToBytes(): Unit = {
    if (!createsReadOnly) {
      val buf = allocBuffer(14)
      buf.limit(10).position(1)

      buf.order(ByteOrder.BIG_ENDIAN)
      val intBuf1 = buf.asIntBuffer()
      intBuf1.put(1, 0x81828384)
      assertEquals(0x81.toByte, buf.get(5))
      assertEquals(0x82.toByte, buf.get(6))
      assertEquals(0x83.toByte, buf.get(7))
      assertEquals(0x84.toByte, buf.get(8))
      assertEquals(0, intBuf1.position())
      intBuf1.put(0x7c7d7e7f)
      assertEquals(0x7c, buf.get(1))
      assertEquals(0x7d, buf.get(2))
      assertEquals(0x7e, buf.get(3))
      assertEquals(0x7f, buf.get(4))
      assertEquals(1, intBuf1.position())

      buf.order(ByteOrder.LITTLE_ENDIAN)
      val intBuf2 = buf.asIntBuffer()
      intBuf2.put(1, 0x81828384)
      assertEquals(0x84.toByte, buf.get(5))
      assertEquals(0x83.toByte, buf.get(6))
      assertEquals(0x82.toByte, buf.get(7))
      assertEquals(0x81.toByte, buf.get(8))
      assertEquals(0, intBuf2.position())
      intBuf2.put(0x7c7d7e7f)
      assertEquals(0x7f, buf.get(1))
      assertEquals(0x7e, buf.get(2))
      assertEquals(0x7d, buf.get(3))
      assertEquals(0x7c, buf.get(4))
      assertEquals(1, intBuf2.position())
    } else {
      val buf = allocBuffer(14)
      buf.limit(10).position(1)

      val intBuf1 = buf.asReadOnlyBuffer().asIntBuffer()
      assertThrows(classOf[ReadOnlyBufferException], intBuf1.put(1, 0x7e7f8081))
    }
  }

  @Test def relativeGetLong(): Unit = {
    val buf = withContent(20, elemRange(0x76, 0x8a): _*)

    buf.order(ByteOrder.BIG_ENDIAN)
    assertEquals(0x767778797a7b7c7dL, buf.getLong())
    assertEquals(8, buf.position())
    assertEquals(0x7e7f808182838485L, buf.getLong())

    buf.order(ByteOrder.LITTLE_ENDIAN)
    buf.position(6)
    assertEquals(0x838281807f7e7d7cL, buf.getLong())

    assertThrows(classOf[BufferUnderflowException], buf.getLong())
  }

  @Test def relativePutLong(): Unit = {
    if (!createsReadOnly) {
      val buf = allocBuffer(20)
      buf.putLong(0x767778797a7b7c7dL)
      assertEquals(8, buf.position())
      assertEquals(0x76, buf.get(0))
      assertEquals(0x77, buf.get(1))
      assertEquals(0x78, buf.get(2))
      assertEquals(0x79, buf.get(3))
      assertEquals(0x7a, buf.get(4))
      assertEquals(0x7b, buf.get(5))
      assertEquals(0x7c, buf.get(6))
      assertEquals(0x7d, buf.get(7))
      buf.putLong(0x7e7f808182838485L)
      assertEquals(0x7e, buf.get(8))
      assertEquals(0x7f, buf.get(9))
      assertEquals(0x80.toByte, buf.get(10))
      assertEquals(0x81.toByte, buf.get(11))
      assertEquals(0x82.toByte, buf.get(12))
      assertEquals(0x83.toByte, buf.get(13))
      assertEquals(0x84.toByte, buf.get(14))
      assertEquals(0x85.toByte, buf.get(15))
      assertEquals(0, buf.get(16))

      buf.order(ByteOrder.LITTLE_ENDIAN)
      buf.position(7)
      buf.putLong(0x8182838485868788L)
      assertEquals(0x7c, buf.get(6))
      assertEquals(0x88.toByte, buf.get(7))
      assertEquals(0x87.toByte, buf.get(8))
      assertEquals(0x86.toByte, buf.get(9))
      assertEquals(0x85.toByte, buf.get(10))
      assertEquals(0x84.toByte, buf.get(11))
      assertEquals(0x83.toByte, buf.get(12))
      assertEquals(0x82.toByte, buf.get(13))
      assertEquals(0x81.toByte, buf.get(14))

      assertThrows(classOf[BufferOverflowException], buf.putLong(0xffff8384))
    } else {
      val buf = allocBuffer(20)
      assertThrows(classOf[ReadOnlyBufferException], buf.putLong(0x75767778))
      assertEquals(0, buf.get(0))
      assertEquals(0, buf.position())
    }
  }

  @Test def absoluteGetLong(): Unit = {
    val buf = withContent(20, elemRange(0x76, 0x8a): _*)

    buf.order(ByteOrder.BIG_ENDIAN)
    assertEquals(0x797a7b7c7d7e7f80L, buf.getLong(3))
    assertEquals(0, buf.position())
    assertEquals(0x7c7d7e7f80818283L, buf.getLong(6))

    buf.order(ByteOrder.LITTLE_ENDIAN)
    buf.position(6)
    assertEquals(0x8584838281807f7eL, buf.getLong(8))
    assertEquals(0x8988878685848382L, buf.getLong(12))

    assertThrows(classOf[IndexOutOfBoundsException], buf.getLong(15))
  }

  @Test def absolutePutLong(): Unit = {
    if (!createsReadOnly) {
      val buf = allocBuffer(20)
      buf.putLong(2, 0x7b7c7d7e7f808182L)
      assertEquals(0, buf.position())
      assertEquals(0, buf.get(0))
      assertEquals(0x7b, buf.get(2))
      assertEquals(0x7c, buf.get(3))
      assertEquals(0x7d, buf.get(4))
      assertEquals(0x7e, buf.get(5))
      assertEquals(0x7f, buf.get(6))
      assertEquals(0x80.toByte, buf.get(7))
      assertEquals(0x81.toByte, buf.get(8))
      assertEquals(0x82.toByte, buf.get(9))
      buf.putLong(7, 0x7d7e7f8081828384L)
      assertEquals(0x7f, buf.get(6))
      assertEquals(0x7d, buf.get(7))
      assertEquals(0x7e, buf.get(8))
      assertEquals(0x7f, buf.get(9))
      assertEquals(0x80.toByte, buf.get(10))
      assertEquals(0x81.toByte, buf.get(11))
      assertEquals(0x82.toByte, buf.get(12))
      assertEquals(0x83.toByte, buf.get(13))
      assertEquals(0x84.toByte, buf.get(14))
      assertEquals(0, buf.get(15))

      buf.order(ByteOrder.LITTLE_ENDIAN)
      buf.position(11)
      buf.putLong(9, 0x8182838485868788L)
      assertEquals(0x7e, buf.get(8))
      assertEquals(0x88.toByte, buf.get(9))
      assertEquals(0x87.toByte, buf.get(10))
      assertEquals(0x86.toByte, buf.get(11))
      assertEquals(0x85.toByte, buf.get(12))
      assertEquals(0x84.toByte, buf.get(13))
      assertEquals(0x83.toByte, buf.get(14))
      assertEquals(0x82.toByte, buf.get(15))
      assertEquals(0x81.toByte, buf.get(16))

      assertThrows(classOf[IndexOutOfBoundsException], buf.putLong(16, 0xffff8384))
    } else {
      val buf = allocBuffer(20)
      assertThrows(classOf[ReadOnlyBufferException], buf.putLong(3, 0x7576))
      assertEquals(0, buf.get(3))
      assertEquals(0, buf.position())
    }
  }

  @Test def asLongBufferBytesToLongs(): Unit = {
    val buf = withContent(20, elemRange(0x76, 0x8a): _*)
    buf.limit(19).position(3)

    buf.order(ByteOrder.BIG_ENDIAN)
    val longBuf1 = buf.asLongBuffer()
    assertEquals(createsReadOnly, longBuf1.isReadOnly)
    assertEquals(2, longBuf1.capacity)
    assertEquals(0, longBuf1.position())
    assertEquals(2, longBuf1.limit())
    assertEquals(ByteOrder.BIG_ENDIAN, longBuf1.order)
    assertEquals(0x8182838485868788L, longBuf1.get(1))
    assertEquals(0, longBuf1.position())
    assertEquals(0x797a7b7c7d7e7f80L, longBuf1.get())
    assertEquals(1, longBuf1.position())

    buf.order(ByteOrder.LITTLE_ENDIAN)
    val longBuf2 = buf.asLongBuffer()
    assertEquals(createsReadOnly, longBuf2.isReadOnly)
    assertEquals(2, longBuf2.capacity)
    assertEquals(0, longBuf2.position())
    assertEquals(2, longBuf2.limit())
    assertEquals(ByteOrder.LITTLE_ENDIAN, longBuf2.order)
    assertEquals(0x8887868584838281L, longBuf2.get(1))
    assertEquals(0, longBuf2.position())
    assertEquals(0x807f7e7d7c7b7a79L, longBuf2.get())
    assertEquals(1, longBuf2.position())
  }

  @Test def asLongBufferLongsToBytes(): Unit = {
    if (!createsReadOnly) {
      val buf = allocBuffer(20)
      buf.limit(19).position(3)

      buf.order(ByteOrder.BIG_ENDIAN)
      val longBuf1 = buf.asLongBuffer()
      longBuf1.put(1, 0x8182838485868788L)
      assertEquals(0x81.toByte, buf.get(11))
      assertEquals(0x82.toByte, buf.get(12))
      assertEquals(0x83.toByte, buf.get(13))
      assertEquals(0x84.toByte, buf.get(14))
      assertEquals(0x85.toByte, buf.get(15))
      assertEquals(0x86.toByte, buf.get(16))
      assertEquals(0x87.toByte, buf.get(17))
      assertEquals(0x88.toByte, buf.get(18))
      assertEquals(0, longBuf1.position())
      longBuf1.put(0x797a7b7c7d7e7f80L)
      assertEquals(0x79, buf.get(3))
      assertEquals(0x7a, buf.get(4))
      assertEquals(0x7b, buf.get(5))
      assertEquals(0x7c, buf.get(6))
      assertEquals(0x7d, buf.get(7))
      assertEquals(0x7e, buf.get(8))
      assertEquals(0x7f, buf.get(9))
      assertEquals(0x80.toByte, buf.get(10))
      assertEquals(1, longBuf1.position())

      buf.order(ByteOrder.LITTLE_ENDIAN)
      val longBuf2 = buf.asLongBuffer()
      longBuf2.put(1, 0x8182838485868788L)
      assertEquals(0x88.toByte, buf.get(11))
      assertEquals(0x87.toByte, buf.get(12))
      assertEquals(0x86.toByte, buf.get(13))
      assertEquals(0x85.toByte, buf.get(14))
      assertEquals(0x84.toByte, buf.get(15))
      assertEquals(0x83.toByte, buf.get(16))
      assertEquals(0x82.toByte, buf.get(17))
      assertEquals(0x81.toByte, buf.get(18))
      assertEquals(0, longBuf2.position())
      longBuf2.put(0x797a7b7c7d7e7f80L)
      assertEquals(0x80.toByte, buf.get(3))
      assertEquals(0x7f, buf.get(4))
      assertEquals(0x7e, buf.get(5))
      assertEquals(0x7d, buf.get(6))
      assertEquals(0x7c, buf.get(7))
      assertEquals(0x7b, buf.get(8))
      assertEquals(0x7a, buf.get(9))
      assertEquals(0x79, buf.get(10))
      assertEquals(1, longBuf2.position())
    } else {
      val buf = allocBuffer(20)
      buf.limit(19).position(3)

      val longBuf1 = buf.asReadOnlyBuffer().asLongBuffer()
      assertThrows(classOf[ReadOnlyBufferException], longBuf1.put(1, 0x8182838485868788L))
    }
  }

  @Test def relativeGetFloat(): Unit = {
    val buf = withContent(pos = 0, limit = 10, capacity = 10,
        0x40, 0x49, 0x0f, 0xd8.toByte, 0x43, 0x17, 0x30, 0x62, 0x4d, 0xab.toByte)

    buf.order(ByteOrder.BIG_ENDIAN)
    assertEquals(3.141592f, buf.getFloat(), 0.0f)
    assertEquals(4, buf.position())
    assertEquals(151.189f, buf.getFloat(), 0.0f)

    buf.order(ByteOrder.LITTLE_ENDIAN)
    buf.position(6)
    assertEquals(-7.2966893e-13f, buf.getFloat(), 0.0f)

    assertThrows(classOf[BufferUnderflowException], buf.getFloat())
  }

  @Test def relativePutFloat(): Unit = {
    if (!createsReadOnly) {
      val buf = allocBuffer(10)
      buf.putFloat(3.141592f)
      assertEquals(4, buf.position())
      assertEquals(0x40, buf.get(0))
      assertEquals(0x49, buf.get(1))
      assertEquals(0x0f, buf.get(2))
      assertEquals(0xd8.toByte, buf.get(3))
      buf.putFloat(151.189f)
      assertEquals(0x43, buf.get(4))
      assertEquals(0x17, buf.get(5))
      assertEquals(0x30, buf.get(6))
      assertEquals(0x62, buf.get(7))
      assertEquals(0, buf.get(8))

      buf.order(ByteOrder.LITTLE_ENDIAN)
      buf.position(3)
      buf.putFloat(-7.2966893e-13f)
      assertEquals(0x0f, buf.get(2))
      assertEquals(0x30, buf.get(3))
      assertEquals(0x62, buf.get(4))
      assertEquals(0x4d, buf.get(5))
      assertEquals(0xab.toByte, buf.get(6))

      assertThrows(classOf[BufferOverflowException], buf.putFloat(654.4f))
    } else {
      val buf = allocBuffer(10)
      assertThrows(classOf[ReadOnlyBufferException], buf.putFloat(151.189f))
      assertEquals(0, buf.get(0))
      assertEquals(0, buf.position())
    }
  }

  @Test def absoluteGetFloat(): Unit = {
    val buf = withContent(pos = 0, limit = 10, capacity = 10,
        0x40, 0x49, 0x0f, 0xd8.toByte, 0x43, 0x17, 0x30, 0x62, 0x4d, 0xab.toByte)

    buf.order(ByteOrder.BIG_ENDIAN)
    assertEquals(3.141592f, buf.getFloat(0), 0.0f)
    assertEquals(0, buf.position())
    assertEquals(151.189f, buf.getFloat(4), 0.0f)

    buf.order(ByteOrder.LITTLE_ENDIAN)
    buf.position(8)
    assertEquals(-7.2966893e-13f, buf.getFloat(6), 0.0f)

    assertThrows(classOf[IndexOutOfBoundsException], buf.getFloat(7))
  }

  @Test def absolutePutFloat(): Unit = {
    if (!createsReadOnly) {
      val buf = allocBuffer(10)
      buf.putFloat(2, 3.141592f)
      assertEquals(0, buf.position())
      assertEquals(0, buf.get(0))
      assertEquals(0x40, buf.get(2))
      assertEquals(0x49, buf.get(3))
      assertEquals(0x0f, buf.get(4))
      assertEquals(0xd8.toByte, buf.get(5))
      buf.putFloat(5, 151.189f)
      assertEquals(0x0f, buf.get(4))
      assertEquals(0x43, buf.get(5))
      assertEquals(0x17, buf.get(6))
      assertEquals(0x30, buf.get(7))
      assertEquals(0x62, buf.get(8))
      assertEquals(0, buf.get(9))

      buf.order(ByteOrder.LITTLE_ENDIAN)
      buf.position(7)
      buf.putFloat(5, -7.2966893e-13f)
      assertEquals(0x0f, buf.get(4))
      assertEquals(0x30, buf.get(5))
      assertEquals(0x62, buf.get(6))
      assertEquals(0x4d, buf.get(7))
      assertEquals(0xab.toByte, buf.get(8))

      assertThrows(classOf[IndexOutOfBoundsException], buf.putFloat(9, 3.141592f))
    } else {
      val buf = allocBuffer(10)
      assertThrows(classOf[ReadOnlyBufferException], buf.putFloat(3, 151.189f))
      assertEquals(0, buf.get(3))
      assertEquals(0, buf.position())
    }
  }

  @Test def asFloatBufferBytesToFloats(): Unit = {
    val buf = withContent(pos = 0, limit = 12, capacity = 12,
        0x10, 0x23,
        0x40, 0x49, 0x0f, 0xd8.toByte, 0x62, 0x30, 0x17, 0x43,
        0x4d, 0xab.toByte)
    buf.limit(11).position(2)

    buf.order(ByteOrder.BIG_ENDIAN)
    val floatBuf1 = buf.asFloatBuffer()
    assertEquals(createsReadOnly, floatBuf1.isReadOnly)
    assertEquals(2, floatBuf1.capacity)
    assertEquals(0, floatBuf1.position())
    assertEquals(2, floatBuf1.limit())
    assertEquals(ByteOrder.BIG_ENDIAN, floatBuf1.order)
    assertEquals(8.120758e20f, floatBuf1.get(1), 0.0f)
    assertEquals(0, floatBuf1.position())
    assertEquals(3.141592f, floatBuf1.get(), 0.0f)
    assertEquals(1, floatBuf1.position())

    buf.order(ByteOrder.LITTLE_ENDIAN)
    val floatBuf2 = buf.asFloatBuffer()
    assertEquals(createsReadOnly, floatBuf2.isReadOnly)
    assertEquals(2, floatBuf2.capacity)
    assertEquals(0, floatBuf2.position())
    assertEquals(2, floatBuf2.limit())
    assertEquals(ByteOrder.LITTLE_ENDIAN, floatBuf2.order)
    assertEquals(151.189f, floatBuf2.get(1), 0.0f)
    assertEquals(0, floatBuf2.position())
    assertEquals(-6.3017908e14f, floatBuf2.get(), 0.0f)
    assertEquals(1, floatBuf2.position())
  }

  @Test def asFloatBufferFloatsToBytes(): Unit = {
    if (!createsReadOnly) {
      val buf = allocBuffer(14)
      buf.limit(10).position(1)

      buf.order(ByteOrder.BIG_ENDIAN)
      val floatBuf1 = buf.asFloatBuffer()
      floatBuf1.put(1, 3.141592f)
      assertEquals(0x40, buf.get(5))
      assertEquals(0x49, buf.get(6))
      assertEquals(0x0f, buf.get(7))
      assertEquals(0xd8.toByte, buf.get(8))
      assertEquals(0, floatBuf1.position())
      floatBuf1.put(151.189f)
      assertEquals(0x43, buf.get(1))
      assertEquals(0x17, buf.get(2))
      assertEquals(0x30, buf.get(3))
      assertEquals(0x62, buf.get(4))
      assertEquals(1, floatBuf1.position())

      buf.order(ByteOrder.LITTLE_ENDIAN)
      val floatBuf2 = buf.asFloatBuffer()
      floatBuf2.put(1, 3.141592f)
      assertEquals(0xd8.toByte, buf.get(5))
      assertEquals(0x0f, buf.get(6))
      assertEquals(0x49, buf.get(7))
      assertEquals(0x40, buf.get(8))
      assertEquals(0, floatBuf2.position())
      floatBuf2.put(151.189f)
      assertEquals(0x62, buf.get(1))
      assertEquals(0x30, buf.get(2))
      assertEquals(0x17, buf.get(3))
      assertEquals(0x43, buf.get(4))
      assertEquals(1, floatBuf2.position())
    } else {
      val buf = allocBuffer(14)
      buf.limit(10).position(1)

      val floatBuf1 = buf.asReadOnlyBuffer().asFloatBuffer()
      assertThrows(classOf[ReadOnlyBufferException], floatBuf1.put(1, 3.141592f))
    }
  }

  @Test def relativeGetDouble(): Unit = {
    val buf = withContent(pos = 0, limit = 20, capacity = 20,
        0x40, 0x09, 0x21, 0xfb.toByte, 0x54, 0x44, 0x2d, 0x18,
        0x40, 0x97.toByte, 0x9c.toByte, 0xcb.toByte, 0xac.toByte, 0x71, 0x0c, 0xb3.toByte,
        0x20, 0xe8.toByte, 0x74, 0xb5.toByte)

    buf.order(ByteOrder.BIG_ENDIAN)
    assertEquals(Math.PI, buf.getDouble(), 0.0)
    assertEquals(8, buf.position())
    assertEquals(1511.1989, buf.getDouble(), 0.0)

    buf.order(ByteOrder.LITTLE_ENDIAN)
    buf.position(12)
    assertEquals(-3.492426300334232e-51, buf.getDouble(), 0.0)

    assertThrows(classOf[BufferUnderflowException], buf.getDouble())
  }

  @Test def relativePutDouble(): Unit = {
    if (!createsReadOnly) {
      val buf = allocBuffer(20)
      buf.putDouble(Math.PI)
      assertEquals(8, buf.position())
      assertEquals(0x40, buf.get(0))
      assertEquals(0x09, buf.get(1))
      assertEquals(0x21, buf.get(2))
      assertEquals(0xfb.toByte, buf.get(3))
      assertEquals(0x54, buf.get(4))
      assertEquals(0x44, buf.get(5))
      assertEquals(0x2d, buf.get(6))
      assertEquals(0x18, buf.get(7))
      buf.putDouble(1511.1989)
      assertEquals(0x40, buf.get(8))
      assertEquals(0x97.toByte, buf.get(9))
      assertEquals(0x9c.toByte, buf.get(10))
      assertEquals(0xcb.toByte, buf.get(11))
      assertEquals(0xac.toByte, buf.get(12))
      assertEquals(0x71, buf.get(13))
      assertEquals(0x0c, buf.get(14))
      assertEquals(0xb3.toByte, buf.get(15))
      assertEquals(0, buf.get(16))

      buf.order(ByteOrder.LITTLE_ENDIAN)
      buf.position(7)
      buf.putDouble(-3.492426300334232e-51)
      assertEquals(0x2d, buf.get(6))
      assertEquals(0xac.toByte, buf.get(7))
      assertEquals(0x71, buf.get(8))
      assertEquals(0x0c, buf.get(9))
      assertEquals(0xb3.toByte, buf.get(10))
      assertEquals(0x20, buf.get(11))
      assertEquals(0xe8.toByte, buf.get(12))
      assertEquals(0x74, buf.get(13))
      assertEquals(0xb5.toByte, buf.get(14))

      assertThrows(classOf[BufferOverflowException], buf.putDouble(1511.1989))
    } else {
      val buf = allocBuffer(20)
      assertThrows(classOf[ReadOnlyBufferException], buf.putDouble(1511.1989))
      assertEquals(0, buf.get(0))
      assertEquals(0, buf.position())
    }
  }

  @Test def absoluteGetDouble(): Unit = {
    val buf = withContent(pos = 0, limit = 20, capacity = 20,
        0x40, 0x09, 0x21, 0xfb.toByte, 0x54, 0x44, 0x2d, 0x18,
        0x40, 0x97.toByte, 0x9c.toByte, 0xcb.toByte, 0xac.toByte, 0x71, 0x0c, 0xb3.toByte,
        0x20, 0xe8.toByte, 0x74, 0xb5.toByte)

    buf.order(ByteOrder.BIG_ENDIAN)
    assertEquals(Math.PI, buf.getDouble(0), 0.0)
    assertEquals(0, buf.position())
    assertEquals(1511.1989, buf.getDouble(8), 0.0)

    buf.order(ByteOrder.LITTLE_ENDIAN)
    buf.position(8)
    assertEquals(-3.492426300334232e-51, buf.getDouble(12), 0.0)

    assertThrows(classOf[IndexOutOfBoundsException], buf.getDouble(15))
  }

  @Test def absolutePutDouble(): Unit = {
    if (!createsReadOnly) {
      val buf = allocBuffer(20)
      buf.putDouble(2, Math.PI)
      assertEquals(0, buf.position())
      assertEquals(0, buf.get(0))
      assertEquals(0x40, buf.get(2))
      assertEquals(0x09, buf.get(3))
      assertEquals(0x21, buf.get(4))
      assertEquals(0xfb.toByte, buf.get(5))
      assertEquals(0x54, buf.get(6))
      assertEquals(0x44, buf.get(7))
      assertEquals(0x2d, buf.get(8))
      assertEquals(0x18, buf.get(9))
      buf.putDouble(5, 1511.1989)
      assertEquals(0x21, buf.get(4))
      assertEquals(0x40, buf.get(5))
      assertEquals(0x97.toByte, buf.get(6))
      assertEquals(0x9c.toByte, buf.get(7))
      assertEquals(0xcb.toByte, buf.get(8))
      assertEquals(0xac.toByte, buf.get(9))
      assertEquals(0x71, buf.get(10))
      assertEquals(0x0c, buf.get(11))
      assertEquals(0xb3.toByte, buf.get(12))
      assertEquals(0, buf.get(13))

      buf.order(ByteOrder.LITTLE_ENDIAN)
      buf.position(7)
      buf.putDouble(9, -3.492426300334232e-51)
      assertEquals(0xcb.toByte, buf.get(8))
      assertEquals(0xac.toByte, buf.get(9))
      assertEquals(0x71, buf.get(10))
      assertEquals(0x0c, buf.get(11))
      assertEquals(0xb3.toByte, buf.get(12))
      assertEquals(0x20, buf.get(13))
      assertEquals(0xe8.toByte, buf.get(14))
      assertEquals(0x74, buf.get(15))
      assertEquals(0xb5.toByte, buf.get(16))

      assertThrows(classOf[IndexOutOfBoundsException], buf.putDouble(17, 1511.1989))
    } else {
      val buf = allocBuffer(20)
      assertThrows(classOf[ReadOnlyBufferException], buf.putDouble(3, 1511.1989))
      assertEquals(0, buf.get(3))
      assertEquals(0, buf.position())
    }
  }

  @Test def asDoubleBufferBytesToDoubles(): Unit = {
    val buf = withContent(pos = 0, limit = 20, capacity = 20,
        0x20, 0xe8.toByte,
        0x40, 0x09, 0x21, 0xfb.toByte, 0x54, 0x44, 0x2d, 0x18,
        0xb3.toByte, 0x0c, 0x71, 0xac.toByte, 0xcb.toByte, 0x9c.toByte, 0x97.toByte, 0x40,
        0x74, 0xb5.toByte)
    buf.limit(19).position(2)

    buf.order(ByteOrder.BIG_ENDIAN)
    val doubleBuf1 = buf.asDoubleBuffer()
    assertEquals(createsReadOnly, doubleBuf1.isReadOnly)
    assertEquals(2, doubleBuf1.capacity)
    assertEquals(0, doubleBuf1.position())
    assertEquals(2, doubleBuf1.limit())
    assertEquals(ByteOrder.BIG_ENDIAN, doubleBuf1.order)
    assertEquals(-8.642954761616149e-63, doubleBuf1.get(1), 0.0)
    assertEquals(0, doubleBuf1.position())
    assertEquals(Math.PI, doubleBuf1.get(), 0.0)
    assertEquals(1, doubleBuf1.position())

    buf.order(ByteOrder.LITTLE_ENDIAN)
    val doubleBuf2 = buf.asDoubleBuffer()
    assertEquals(createsReadOnly, doubleBuf2.isReadOnly)
    assertEquals(2, doubleBuf2.capacity)
    assertEquals(0, doubleBuf2.position())
    assertEquals(2, doubleBuf2.limit())
    assertEquals(ByteOrder.LITTLE_ENDIAN, doubleBuf2.order)
    assertEquals(1511.1989, doubleBuf2.get(1), 0.0)
    assertEquals(0, doubleBuf2.position())
    assertEquals(3.207375630676366e-192, doubleBuf2.get(), 0.0)
    assertEquals(1, doubleBuf2.position())
  }

  @Test def asDoubleBufferDoublesToBytes(): Unit = {
    if (!createsReadOnly) {
      val buf = allocBuffer(20)
      buf.limit(19).position(3)

      buf.order(ByteOrder.BIG_ENDIAN)
      val doubleBuf1 = buf.asDoubleBuffer()
      doubleBuf1.put(1, Math.PI)
      assertEquals(0x40, buf.get(11))
      assertEquals(0x09, buf.get(12))
      assertEquals(0x21, buf.get(13))
      assertEquals(0xfb.toByte, buf.get(14))
      assertEquals(0x54, buf.get(15))
      assertEquals(0x44, buf.get(16))
      assertEquals(0x2d, buf.get(17))
      assertEquals(0x18, buf.get(18))
      assertEquals(0, doubleBuf1.position())
      doubleBuf1.put(1511.1989)
      assertEquals(0x40, buf.get(3))
      assertEquals(0x97.toByte, buf.get(4))
      assertEquals(0x9c.toByte, buf.get(5))
      assertEquals(0xcb.toByte, buf.get(6))
      assertEquals(0xac.toByte, buf.get(7))
      assertEquals(0x71, buf.get(8))
      assertEquals(0x0c, buf.get(9))
      assertEquals(0xb3.toByte, buf.get(10))
      assertEquals(1, doubleBuf1.position())

      buf.order(ByteOrder.LITTLE_ENDIAN)
      val doubleBuf2 = buf.asDoubleBuffer()
      doubleBuf2.put(1, Math.PI)
      assertEquals(0x18, buf.get(11))
      assertEquals(0x2d, buf.get(12))
      assertEquals(0x44, buf.get(13))
      assertEquals(0x54, buf.get(14))
      assertEquals(0xfb.toByte, buf.get(15))
      assertEquals(0x21, buf.get(16))
      assertEquals(0x09, buf.get(17))
      assertEquals(0x40, buf.get(18))
      assertEquals(0, doubleBuf2.position())
      doubleBuf2.put(1511.1989)
      assertEquals(0xb3.toByte, buf.get(3))
      assertEquals(0x0c, buf.get(4))
      assertEquals(0x71, buf.get(5))
      assertEquals(0xac.toByte, buf.get(6))
      assertEquals(0xcb.toByte, buf.get(7))
      assertEquals(0x9c.toByte, buf.get(8))
      assertEquals(0x97.toByte, buf.get(9))
      assertEquals(0x40, buf.get(10))
      assertEquals(1, doubleBuf2.position())
    } else {
      val buf = allocBuffer(20)
      buf.limit(19).position(3)

      val doubleBuf1 = buf.asReadOnlyBuffer().asDoubleBuffer()
      assertThrows(classOf[ReadOnlyBufferException], doubleBuf1.put(1, Math.PI))
    }
  }
}

class AllocByteBufferTest extends ByteBufferTest {
  val factory: ByteBufferFactory =
    new ByteBufferFactories.AllocByteBufferFactory
}

class WrappedByteBufferTest extends ByteBufferTest {
  val factory: ByteBufferFactory =
    new ByteBufferFactories.WrappedByteBufferFactory
}

class ReadOnlyWrappedByteBufferTest extends ByteBufferTest {
  val factory: ByteBufferFactory =
    new ByteBufferFactories.ReadOnlyWrappedByteBufferFactory
}

class SlicedAllocByteBufferTest extends ByteBufferTest {
  val factory: ByteBufferFactory =
    new ByteBufferFactories.SlicedAllocByteBufferFactory
}
