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

import scala.scalajs.js
import scala.scalajs.LinkingInfo.{isWebAssembly, linkTimeIf}

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

  private val innerJS: js.Array[E] = {
    linkTimeIf(!isWebAssembly) {
      innerInit.asInstanceOf[js.Array[E]]
    } {
      null
    }
  }

  private var innerWasm: Array[AnyRef] = {
    linkTimeIf(isWebAssembly) {
      innerInit.asInstanceOf[Array[AnyRef]]
    } {
      null
    }
  }

  def this(initialCapacity: Int) = {
    this(
      {
        BoundsChecks.checkCapacity(initialCapacity)
        linkTimeIf(isWebAssembly) {
          (new Array[AnyRef](initialCapacity)).asInstanceOf[AnyRef]
        } {
          new js.Array[E]
        }
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
    linkTimeIf(isWebAssembly) {
      resizeTo(size())
    } {}
    // We ignore this in JS as js.Array doesn't support explicit pre-allocation
  }

  def ensureCapacity(minCapacity: Int): Unit = {
    linkTimeIf(isWebAssembly) {
      if (innerWasm.length < minCapacity)
        resizeTo(roundUpToPowerOfTwo(minCapacity))
    } {}
    // We ignore this in JS as js.Array doesn't support explicit pre-allocation
  }

  def size(): Int = {
    linkTimeIf(isWebAssembly) {
      _size
    } {
      innerJS.length
    }
  }

  override def clone(): AnyRef = {
    linkTimeIf(isWebAssembly) {
      new ArrayList(innerWasm.clone(), size())
    } {
      new ArrayList(innerJS.jsSlice(0), 0)
    }
  }

  def get(index: Int): E = {
    checkIndexInBounds(index)
    linkTimeIf(isWebAssembly) {
      innerWasm(index).asInstanceOf[E]
    } {
      innerJS(index)
    }
  }

  override def set(index: Int, element: E): E = {
    val e = get(index)
    linkTimeIf(isWebAssembly) {
      innerWasm(index) = element.asInstanceOf[AnyRef]
    } {
      innerJS(index) = element
    }
    e
  }

  override def add(e: E): Boolean = {
    linkTimeIf(isWebAssembly) {
      if (size() >= innerWasm.length)
        expand()
      innerWasm(size()) = e.asInstanceOf[AnyRef]
      _size += 1
    } {
      innerJS.push(e)
    }
    true
  }

  override def add(index: Int, element: E): Unit = {
    checkIndexOnBounds(index)
    linkTimeIf(isWebAssembly) {
      if (size() >= innerWasm.length)
        expand()
      System.arraycopy(innerWasm, index, innerWasm, index + 1, size() - index)
      innerWasm(index) = element.asInstanceOf[AnyRef]
      _size += 1
    } {
      innerJS.splice(index, 0, element)
    }
  }

  override def remove(index: Int): E = {
    checkIndexInBounds(index)
    linkTimeIf(isWebAssembly) {
      val removed = innerWasm(index).asInstanceOf[E]
      System.arraycopy(innerWasm, index + 1, innerWasm, index, size() - index - 1)
      innerWasm(size - 1) = null // free reference for GC
      _size -= 1
      removed
    } {
      arrayRemoveAndGet(innerJS, index)
    }
  }

  override def clear(): Unit = {
    linkTimeIf(isWebAssembly) {
      Arrays.fill(innerWasm, null) // free references for GC
      _size = 0
    } {
      innerJS.length = 0
    }
  }

  override def addAll(index: Int, c: Collection[_ <: E]): Boolean = {
    c match {
      case other: ArrayList[_] =>
        checkIndexOnBounds(index)
        linkTimeIf(isWebAssembly) {
          ensureCapacity(size() + other.size())
          System.arraycopy(innerWasm, index, innerWasm, index + other.size(), size() - index)
          System.arraycopy(other.innerWasm, 0, innerWasm, index, other.size())
          _size += c.size()
        } {
          innerJS.splice(index, 0, other.innerJS.toSeq: _*)
        }
        other.size() > 0
      case _ => super.addAll(index, c)
    }
  }

  override protected def removeRange(fromIndex: Int, toIndex: Int): Unit = {
    val count = BoundsChecks.checkStartEnd(fromIndex, toIndex, size())
    linkTimeIf(isWebAssembly) {
      if (count != 0) {
        System.arraycopy(innerWasm, toIndex, innerWasm, fromIndex, size() - toIndex)
        val newSize = size() - count
        Arrays.fill(innerWasm, newSize, size(), null) // free references for GC
        _size = newSize
      }
    } {
      innerJS.splice(fromIndex, count)
    }
  }

  // Wasm only
  private def expand(): Unit =
    resizeTo(Math.max(innerWasm.length * 2, 16))

  // Wasm only
  private def resizeTo(newCapacity: Int): Unit =
    innerWasm = Arrays.copyOf(innerWasm, newCapacity)
}
