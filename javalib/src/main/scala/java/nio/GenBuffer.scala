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

package java.nio

private[nio] object GenBuffer {
  def apply[B <: Buffer](self: B): GenBuffer[B] =
    new GenBuffer(self)
}

/* The underlying `val self` is intentionally public because
 * `self.ElementType` and `self.BufferType` appear in signatures.
 * It's tolerable because the class is `private[nio]` anyway.
 */
private[nio] final class GenBuffer[B <: Buffer] private (val self: B)
    extends AnyVal {

  import self._

  @inline
  def generic_get(): ElementType =
    load(getPosAndAdvanceRead())

  @inline
  def generic_put(elem: ElementType): B = {
    ensureNotReadOnly()
    store(getPosAndAdvanceWrite(), elem)
    self
  }

  @inline
  def generic_get(index: Int): ElementType =
    load(validateIndex(index))

  @inline
  def generic_put(index: Int, elem: ElementType): BufferType = {
    ensureNotReadOnly()
    store(validateIndex(index), elem)
    self
  }

  @inline
  def generic_get(dst: Array[ElementType],
      offset: Int, length: Int): BufferType = {
    validateArrayIndexRange(dst, offset, length)
    load(getPosAndAdvanceRead(length), dst, offset, length)
    self
  }

  @inline
  def generic_put(src: BufferType): BufferType = {
    if (src eq self)
      throw new IllegalArgumentException
    ensureNotReadOnly()
    val srcLimit = src.limit()
    var srcPos = src.position()
    val length = srcLimit - srcPos
    var selfPos = getPosAndAdvanceWrite(length)
    src.position(srcLimit)

    val srcArray = src._array // even if read-only
    if (srcArray != null) {
      store(selfPos, srcArray, src._arrayOffset + srcPos, length)
    } else {
      while (srcPos != srcLimit) {
        store(selfPos, src.load(srcPos))
        srcPos += 1
        selfPos += 1
      }
    }

    self
  }

  @inline
  def generic_put(src: Array[ElementType],
      offset: Int, length: Int): BufferType = {
    ensureNotReadOnly()
    validateArrayIndexRange(src, offset, length)
    store(getPosAndAdvanceWrite(length), src, offset, length)
    self
  }

  @inline
  def generic_hasArray(): Boolean =
    _array != null && !isReadOnly()

  @inline
  def generic_array(): Array[ElementType] = {
    val a = _array
    if (a == null)
      throw new UnsupportedOperationException
    if (isReadOnly())
      throw new ReadOnlyBufferException
    a
  }

  @inline
  def generic_arrayOffset(): Int = {
    val o = _arrayOffset
    if (o == -1)
      throw new UnsupportedOperationException
    if (isReadOnly())
      throw new ReadOnlyBufferException
    o
  }

  @inline
  def generic_hashCode(hashSeed: Int): Int = {
    import scala.util.hashing.MurmurHash3._
    val start = position()
    val end = limit()
    var h = hashSeed
    var i = start
    while (i != end) {
      h = mix(h, load(i).##)
      i += 1
    }
    finalizeHash(h, end-start)
  }

  @inline
  def generic_compareTo(that: BufferType)(
      compare: (ElementType, ElementType) => Int): Int = {
    // scalastyle:off return
    if (self eq that) {
      0
    } else {
      val thisStart = self.position()
      val thisRemaining = self.limit() - thisStart
      val thatStart = that.position()
      val thatRemaining = that.limit() - thatStart
      val shortestLength = Math.min(thisRemaining, thatRemaining)

      var i = 0
      while (i != shortestLength) {
        val cmp = compare(self.load(thisStart + i), that.load(thatStart + i))
        if (cmp != 0)
          return cmp
        i += 1
      }

      Integer.compare(thisRemaining, thatRemaining)
    }
    // scalastyle:on return
  }

  @inline
  def generic_load(startIndex: Int,
      dst: Array[ElementType], offset: Int, length: Int): Unit = {
    var selfPos = startIndex
    val endPos = selfPos + length
    var arrayIndex = offset
    while (selfPos != endPos) {
      dst(arrayIndex) = load(selfPos)
      selfPos += 1
      arrayIndex += 1
    }
  }

  @inline
  def generic_store(startIndex: Int,
      src: Array[ElementType], offset: Int, length: Int): Unit = {
    var selfPos = startIndex
    val endPos = selfPos + length
    var arrayIndex = offset
    while (selfPos != endPos) {
      store(selfPos, src(arrayIndex))
      selfPos += 1
      arrayIndex += 1
    }
  }

}
