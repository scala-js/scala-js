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

package org.scalajs.linker.backend.wasmemitter

import java.nio.{ByteBuffer, ByteOrder}

import scala.collection.mutable

import org.scalajs.ir.OriginalName

import org.scalajs.linker.backend.wasmemitter.VarGen.genDataID

import org.scalajs.linker.backend.webassembly.Identitities._
import org.scalajs.linker.backend.webassembly.Modules._

/** Pool of constant arrays that we store in data segments. */
final class ConstantArrayPool {
  /* We use 4 data segments; one for each byte size: 1, 2, 4 and 8.
   * This way, every sub-segment containing the contents of an array is aligned
   * to the byte size of elements of that array.
   */

  // Indexed by log2ByteSize
  private val constantArrays = Array.fill(4)(mutable.ListBuffer.empty[Array[Byte]])
  private val currentSizes = new Array[Int](4)

  def addArray8[T](elems: List[T])(putElem: (ByteBuffer, T) => Unit): (DataID, Int) =
    addArrayInternal(log2ByteSize = 0, elems)(putElem)

  def addArray16[T](elems: List[T])(putElem: (ByteBuffer, T) => Unit): (DataID, Int) =
    addArrayInternal(log2ByteSize = 1, elems)(putElem)

  def addArray32[T](elems: List[T])(putElem: (ByteBuffer, T) => Unit): (DataID, Int) =
    addArrayInternal(log2ByteSize = 2, elems)(putElem)

  def addArray64[T](elems: List[T])(putElem: (ByteBuffer, T) => Unit): (DataID, Int) =
    addArrayInternal(log2ByteSize = 3, elems)(putElem)

  private def addArrayInternal[T](log2ByteSize: Int, elems: List[T])(
      putElem: (ByteBuffer, T) => Unit): (DataID, Int) = {

    val length = elems.size
    val size = length << log2ByteSize // length * byteSize
    val array = new Array[Byte](size)
    val offset = currentSizes(log2ByteSize)

    val buffer = ByteBuffer.wrap(array).order(ByteOrder.LITTLE_ENDIAN)
    elems.foreach(putElem(buffer, _))

    constantArrays(log2ByteSize) += array
    currentSizes(log2ByteSize) += size

    (genDataID.constantArrays(log2ByteSize), offset)
  }

  def genPool(): List[Data] = {
    for {
      log2ByteSize <- constantArrays.indices.toList
      if constantArrays(log2ByteSize).nonEmpty
    } yield {
      val bytes = new Array[Byte](currentSizes(log2ByteSize))
      var offset = 0
      for (array <- constantArrays(log2ByteSize)) {
        System.arraycopy(array, 0, bytes, offset, array.length)
        offset += array.length
      }
      Data(genDataID.constantArrays(log2ByteSize),
          OriginalName(s"constantArrays${1 << log2ByteSize}"),
          bytes, Data.Mode.Passive)
    }
  }
}
