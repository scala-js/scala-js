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

package scala.scalajs.js

import scala.language.implicitConversions

import scala.collection.mutable
import scala.collection.{SeqFactory, StrictOptimizedSeqFactory, StrictOptimizedSeqOps}

import scala.scalajs.js
import scala.scalajs.LinkingInfo.{linkTimeIf, moduleKind}
import scala.scalajs.LinkingInfo.ModuleKind.MinimalWasmModule

/** Equivalent of scm.WrappedArray for js.Array */
@inline
final class WrappedArray[A](private val array: js.Array[A])
    extends mutable.AbstractBuffer[A]
    with StrictOptimizedSeqOps[A, js.WrappedArray, js.WrappedArray[A]] with mutable.IndexedSeq[A]
    with mutable.IndexedSeqOps[A, js.WrappedArray, js.WrappedArray[A]] with mutable.IndexedBuffer[A]
    with mutable.Builder[A, js.WrappedArray[A]]
    with scala.collection.IterableFactoryDefaults[A, js.WrappedArray] with Serializable {

  private val wasmArray: mutable.ArrayBuffer[A] = {
    linkTimeIf(moduleKind == MinimalWasmModule) {
      mutable.ArrayBuffer.empty[A]
    } {
      null
    }
  }

  /** Creates a new empty [[WrappedArray]]. */
  def this() = this {
    linkTimeIf[js.Array[A]](moduleKind == MinimalWasmModule) {
      null
    } {
      js.Array()
    }
  }

  override def iterableFactory: SeqFactory[js.WrappedArray] = js.WrappedArray

  // IndexedSeq interface

  @inline def update(index: Int, elem: A): Unit = {
    linkTimeIf(moduleKind == MinimalWasmModule) {
      wasmArray(index) = elem
    } {
      array(index) = elem
    }
  }

  @inline def apply(index: Int): A = {
    linkTimeIf(moduleKind == MinimalWasmModule) {
      wasmArray(index)
    } {
      array(index)
    }
  }

  @inline def length: Int = {
    linkTimeIf(moduleKind == MinimalWasmModule) {
      wasmArray.length
    } {
      array.length
    }
  }

  @inline override def knownSize: Int = length

  // Builder interface

  @inline def addOne(elem: A): this.type = {
    linkTimeIf(moduleKind == MinimalWasmModule) {
      wasmArray += elem
      ()
    } {
      array.push(elem)
      ()
    }
    this
  }

  @inline def clear(): Unit = {
    linkTimeIf(moduleKind == MinimalWasmModule) {
      wasmArray.clear()
    } {
      array.length = 0
    }
  }

  @inline def result(): js.WrappedArray[A] = this

  // Rest of Buffer interface

  @inline def prepend(elem: A): this.type = {
    linkTimeIf(moduleKind == MinimalWasmModule) {
      wasmArray.prepend(elem)
      ()
    } {
      array.unshift(elem)
      ()
    }
    this
  }

  @inline override def prependAll(xs: IterableOnce[A]): this.type = {
    linkTimeIf(moduleKind == MinimalWasmModule) {
      wasmArray.prependAll(xs)
      ()
    } {
      array.unshift(xs.iterator.toSeq: _*)
      ()
    }
    this
  }

  @inline def insert(idx: Int, elem: A): Unit = {
    if (idx < 0 || idx > length)
      throw new IndexOutOfBoundsException
    linkTimeIf(moduleKind == MinimalWasmModule) {
      wasmArray.insert(idx, elem)
    } {
      array.splice(idx, 0, elem)
      ()
    }
  }

  @inline
  def insertAll(n: Int, elems: scala.collection.IterableOnce[A]): Unit = {
    if (n < 0 || n > length)
      throw new IndexOutOfBoundsException
    linkTimeIf(moduleKind == MinimalWasmModule) {
      wasmArray.insertAll(n, elems)
    } {
      array.splice(n, 0, elems.iterator.toSeq: _*)
      ()
    }
  }

  def remove(n: Int): A = {
    if (n < 0 || n >= length)
      throw new IndexOutOfBoundsException
    linkTimeIf(moduleKind == MinimalWasmModule) {
      wasmArray.remove(n)
    } {
      array.splice(n, 1)(0)
    }
  }

  override def remove(n: Int, count: Int): Unit = {
    if (count < 0)
      throw new IllegalArgumentException
    if (n < 0 || (count > 0 && n + count > length))
      throw new IndexOutOfBoundsException
    linkTimeIf(moduleKind == MinimalWasmModule) {
      wasmArray.remove(n, count)
    } {
      array.splice(n, count)
      ()
    }
  }

  @inline override def className: String = "WrappedArray"

}

/** Factory for [[WrappedArray]]. Provides implicit conversion to [[Array]]. */
object WrappedArray extends StrictOptimizedSeqFactory[js.WrappedArray] {

  def empty[A]: js.WrappedArray[A] = new js.WrappedArray[A]()

  def newBuilder[A]: mutable.Builder[A, js.WrappedArray[A]] =
    new js.WrappedArray[A]

  def from[A](source: IterableOnce[A]): js.WrappedArray[A] =
    (newBuilder[A] ++= source).result()

  implicit def toJSArray[A](wrappedArray: js.WrappedArray[A]): js.Array[A] =
    wrappedArray.array

}
