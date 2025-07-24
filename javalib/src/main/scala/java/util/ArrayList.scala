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

package java.util

import java.lang.Cloneable
import java.lang.Utils._
import java.util.ScalaOps._

import scala.scalajs._
import scala.scalajs.LinkingInfo.isWebAssembly

class ArrayList[E] private (innerInit: AnyRef, private var _size: Int)
    extends AbstractList[E] with RandomAccess with Cloneable with Serializable {
  self =>

  /* This class has two different implementations for handling the
   * internal data storage, depending on whether we are on Wasm or JS.
   * On JS, we utilize `js.Array`. On Wasm, for performance reasons,
   * we avoid JS interop and use a scala.Array.
   * The `_size` field (unused in JS) keeps track of the effective size
   * of the underlying Array for the Wasm implementation.
   */

  private val innerJS: js.Array[E] =
    if (isWebAssembly) null
    else innerInit.asInstanceOf[js.Array[E]]

  private var innerWasm: Array[AnyRef] =
    if (!isWebAssembly) null
    else innerInit.asInstanceOf[Array[AnyRef]]

  def this(initialCapacity: Int) = {
    this(
      {
        if (initialCapacity < 0)
          throw new IllegalArgumentException
        if (isWebAssembly) new Array[AnyRef](initialCapacity)
        else new js.Array[E]
      },
      0
    )
  }

  def this() = this(16)

  def this(c: Collection[_ <: E]) = {
    this(c.size())
    addAll(c)
  }

  def trimToSize(): Unit = {
    if (isWebAssembly)
      resizeTo(size())
    // We ignore this in JS as js.Array doesn't support explicit pre-allocation
  }

  def ensureCapacity(minCapacity: Int): Unit = {
    if (isWebAssembly) {
      if (innerWasm.length < minCapacity)
        resizeTo(roundUpToPowerOfTwo(minCapacity))
    }
    // We ignore this in JS as js.Array doesn't support explicit pre-allocation
  }

  def size(): Int =
    if (isWebAssembly) _size
    else innerJS.length

  override def clone(): AnyRef = {
    if (isWebAssembly)
      new ArrayList(innerWasm.clone(), size())
    else
      new ArrayList(innerJS.jsSlice(0), 0)
  }

  def get(index: Int): E = {
    checkIndexInBounds(index)
    if (isWebAssembly)
      innerWasm(index).asInstanceOf[E]
    else
      innerJS(index)
  }

  override def set(index: Int, element: E): E = {
    val e = get(index)
    if (isWebAssembly)
      innerWasm(index) = element.asInstanceOf[AnyRef]
    else
      innerJS(index) = element
    e
  }

  override def add(e: E): Boolean = {
    if (isWebAssembly) {
      if (size() >= innerWasm.length)
        expand()
      innerWasm(size()) = e.asInstanceOf[AnyRef]
      _size += 1
    } else {
      innerJS.push(e)
    }
    true
  }

  override def add(index: Int, element: E): Unit = {
    checkIndexOnBounds(index)
    if (isWebAssembly) {
      if (size() >= innerWasm.length)
        expand()
      System.arraycopy(innerWasm, index, innerWasm, index + 1, size() - index)
      innerWasm(index) = element.asInstanceOf[AnyRef]
      _size += 1
    } else {
      innerJS.splice(index, 0, element)
    }
  }

  override def remove(index: Int): E = {
    checkIndexInBounds(index)
    if (isWebAssembly) {
      val removed = innerWasm(index).asInstanceOf[E]
      System.arraycopy(innerWasm, index + 1, innerWasm, index, size() - index - 1)
      innerWasm(size - 1) = null // free reference for GC
      _size -= 1
      removed
    } else {
      arrayRemoveAndGet(innerJS, index)
    }
  }

  override def clear(): Unit =
    if (isWebAssembly) {
      Arrays.fill(innerWasm, null) // free references for GC
      _size = 0
    } else {
      innerJS.length = 0
    }

  override def addAll(index: Int, c: Collection[_ <: E]): Boolean = {
    c match {
      case other: ArrayList[_] =>
        checkIndexOnBounds(index)
        if (isWebAssembly) {
          ensureCapacity(size() + other.size())
          System.arraycopy(innerWasm, index, innerWasm, index + other.size(), size() - index)
          System.arraycopy(other.innerWasm, 0, innerWasm, index, other.size())
          _size += c.size()
        } else {
          innerJS.splice(index, 0, other.innerJS.toSeq: _*)
        }
        other.size() > 0
      case _ => super.addAll(index, c)
    }
  }

  override protected def removeRange(fromIndex: Int, toIndex: Int): Unit = {
    if (fromIndex < 0 || toIndex > size() || toIndex < fromIndex)
      throw new IndexOutOfBoundsException()
    if (isWebAssembly) {
      if (fromIndex != toIndex) {
        System.arraycopy(innerWasm, toIndex, innerWasm, fromIndex, size() - toIndex)
        val newSize = size() - toIndex + fromIndex
        Arrays.fill(innerWasm, newSize, size(), null) // free references for GC
        _size = newSize
      }
    } else {
      innerJS.splice(fromIndex, toIndex - fromIndex)
    }
  }

  // Wasm only
  private def expand(): Unit = {
    resizeTo(Math.max(innerWasm.length * 2, 16))
  }

  // Wasm only
  private def resizeTo(newCapacity: Int): Unit = {
    innerWasm = Arrays.copyOf(innerWasm, newCapacity)
  }
}
