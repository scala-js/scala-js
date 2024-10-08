/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection
package mutable

import scala.reflect.ClassTag
import scala.runtime.BoxedUnit

import scala.scalajs.js
import scala.scalajs.LinkingInfo

/** A builder class for arrays.
 *
 *  @since 2.8
 *
 *  @tparam T    the type of the elements for the builder.
 */
abstract class ArrayBuilder[T] extends ReusableBuilder[T, Array[T]] with Serializable

/** A companion object for array builders.
 *
 *  @since 2.8
 */
object ArrayBuilder {

  /** Creates a new arraybuilder of type `T`.
   *
   *  @tparam T     type of the elements for the array builder, with a `ClassTag` context bound.
   *  @return       a new empty array builder.
   */
  @inline
  def make[T: ClassTag](): ArrayBuilder[T] =
    if (LinkingInfo.isWebAssembly) makeForWasm()
    else makeForJS()

  /** Implementation of `make` for JS. */
  @inline
  private def makeForJS[T: ClassTag](): ArrayBuilder[T] =
    new ArrayBuilder.generic[T](implicitly[ClassTag[T]].runtimeClass)

  /** Implementation of `make` for Wasm.
   *
   *  This is the original upstream implementation from 2.13.x. It is better
   *  than the one for 2.12.x because it does not use `isPrimitive` as "fast"
   *  dispatch, which we cannot constant-fold away.
   */
  @inline
  private def makeForWasm[T: ClassTag](): ArrayBuilder[T] = {
    val tag = implicitly[ClassTag[T]]
    tag.runtimeClass match {
      case java.lang.Byte.TYPE      => new ArrayBuilder.ofByte().asInstanceOf[ArrayBuilder[T]]
      case java.lang.Short.TYPE     => new ArrayBuilder.ofShort().asInstanceOf[ArrayBuilder[T]]
      case java.lang.Character.TYPE => new ArrayBuilder.ofChar().asInstanceOf[ArrayBuilder[T]]
      case java.lang.Integer.TYPE   => new ArrayBuilder.ofInt().asInstanceOf[ArrayBuilder[T]]
      case java.lang.Long.TYPE      => new ArrayBuilder.ofLong().asInstanceOf[ArrayBuilder[T]]
      case java.lang.Float.TYPE     => new ArrayBuilder.ofFloat().asInstanceOf[ArrayBuilder[T]]
      case java.lang.Double.TYPE    => new ArrayBuilder.ofDouble().asInstanceOf[ArrayBuilder[T]]
      case java.lang.Boolean.TYPE   => new ArrayBuilder.ofBoolean().asInstanceOf[ArrayBuilder[T]]
      case java.lang.Void.TYPE      => new ArrayBuilder.ofUnit().asInstanceOf[ArrayBuilder[T]]
      case _                        => new ArrayBuilder.ofRef[T with AnyRef]()(tag.asInstanceOf[ClassTag[T with AnyRef]]).asInstanceOf[ArrayBuilder[T]]
    }
  }

  /** A generic ArrayBuilder optimized for Scala.js.
   *
   *  @tparam T              type of elements for the array builder.
   *  @param  elementClass   runtime class of the elements in the array.
   */
  @inline
  private final class generic[T](elementClass: Class[_]) extends ArrayBuilder[T] {

    private val isCharArrayBuilder = classOf[Char] == elementClass
    private var elems: js.Array[Any] = js.Array()

    def +=(elem: T): this.type = {
      val unboxedElem =
        if (isCharArrayBuilder) elem.asInstanceOf[Char].toInt
        else if (elem == null) zeroOf(elementClass)
        else elem
      elems.push(unboxedElem)
      this
    }

    def clear(): Unit =
      elems = js.Array()

    def result(): Array[T] = {
      val elemRuntimeClass =
        if (classOf[Unit] == elementClass) classOf[BoxedUnit]
        else if (classOf[Null] == elementClass || classOf[Nothing] == elementClass) classOf[Object]
        else elementClass
      genericArrayBuilderResult(elemRuntimeClass, elems)
    }

    override def toString(): String = "ArrayBuilder.generic"
  }

  // Intrinsic
  private def zeroOf(runtimeClass: Class[_]): Any = runtimeClass match {
    case java.lang.Byte.TYPE      => 0.toByte
    case java.lang.Short.TYPE     => 0.toShort
    case java.lang.Character.TYPE => 0 // yes, as an Int
    case java.lang.Integer.TYPE   => 0
    case java.lang.Long.TYPE      => 0L
    case java.lang.Float.TYPE     => 0.0f
    case java.lang.Double.TYPE    => 0.0
    case java.lang.Boolean.TYPE   => false
    case java.lang.Void.TYPE      => ()
    case _                        => null
  }

  // Intrinsic
  private def genericArrayBuilderResult[T](runtimeClass: Class[_],
      a: js.Array[Any]): Array[T] = {
    val len = a.length

    if (classOf[Char] == runtimeClass) {
      val result = new Array[Char](len)
      var i = 0
      while (i != len) {
        result(i) = a(i).asInstanceOf[Int].toChar
        i += 1
      }
      result.asInstanceOf[Array[T]]
    } else {
      val result: Array[T] = java.lang.reflect.Array.newInstance(
          runtimeClass, len).asInstanceOf[Array[T]]
      var i = 0
      while (i != len) {
        result(i) = a(i).asInstanceOf[T]
        i += 1
      }
      result
    }
  }

  /** A class for array builders for arrays of reference types.
   *
   *  This builder can be reused.
   *
   *  @tparam T     type of elements for the array builder, subtype of `AnyRef` with a `ClassTag` context bound.
   */
  final class ofRef[T <: AnyRef : ClassTag] extends ArrayBuilder[T] {

    private var elems: Array[T] = _
    private var capacity: Int = 0
    private var size: Int = 0

    private def mkArray(size: Int): Array[T] = {
      val newelems = new Array[T](size)
      if (this.size > 0) Array.copy(elems, 0, newelems, 0, this.size)
      newelems
    }

    private def resize(size: Int) {
      elems = mkArray(size)
      capacity = size
    }

    override def sizeHint(size: Int) {
      if (capacity < size) resize(size)
    }

    private def ensureSize(size: Int) {
      if (capacity < size || capacity == 0) {
        var newsize = if (capacity == 0) 16 else capacity * 2
        while (newsize < size) newsize *= 2
        resize(newsize)
      }
    }

    def +=(elem: T): this.type = {
      ensureSize(size + 1)
      elems(size) = elem
      size += 1
      this
    }

    override def ++=(xs: TraversableOnce[T]): this.type = (xs.asInstanceOf[AnyRef]) match {
      case xs: WrappedArray.ofRef[_] =>
        ensureSize(this.size + xs.length)
        Array.copy(xs.array, 0, elems, this.size, xs.length)
        size += xs.length
        this
      case _ =>
        super.++=(xs)
    }

    def clear() { size = 0 }

    def result() = {
      if (capacity != 0 && capacity == size) {
        capacity = 0
        elems
      }
      else mkArray(size)
    }

    override def equals(other: Any): Boolean = other match {
      case x: ofRef[_] => (size == x.size) && (elems == x.elems)
      case _ => false
    }

    override def toString = "ArrayBuilder.ofRef"
  }

  /** A class for array builders for arrays of `byte`s. It can be reused. */
  final class ofByte extends ArrayBuilder[Byte] {

    private var elems: Array[Byte] = _
    private var capacity: Int = 0
    private var size: Int = 0

    private def mkArray(size: Int): Array[Byte] = {
      val newelems = new Array[Byte](size)
      if (this.size > 0) Array.copy(elems, 0, newelems, 0, this.size)
      newelems
    }

    private def resize(size: Int) {
      elems = mkArray(size)
      capacity = size
    }

    override def sizeHint(size: Int) {
      if (capacity < size) resize(size)
    }

    private def ensureSize(size: Int) {
      if (capacity < size || capacity == 0) {
        var newsize = if (capacity == 0) 16 else capacity * 2
        while (newsize < size) newsize *= 2
        resize(newsize)
      }
    }

    def +=(elem: Byte): this.type = {
      ensureSize(size + 1)
      elems(size) = elem
      size += 1
      this
    }

    override def ++=(xs: TraversableOnce[Byte]): this.type = xs match {
      case xs: WrappedArray.ofByte =>
        ensureSize(this.size + xs.length)
        Array.copy(xs.array, 0, elems, this.size, xs.length)
        size += xs.length
        this
      case _ =>
        super.++=(xs)
    }

    def clear() { size = 0 }

    def result() = {
      if (capacity != 0 && capacity == size) {
        capacity = 0
        elems
      }
      else mkArray(size)
    }

    override def equals(other: Any): Boolean = other match {
      case x: ofByte => (size == x.size) && (elems == x.elems)
      case _ => false
    }

    override def toString = "ArrayBuilder.ofByte"
  }

  /** A class for array builders for arrays of `short`s. It can be reused. */
  final class ofShort extends ArrayBuilder[Short] {

    private var elems: Array[Short] = _
    private var capacity: Int = 0
    private var size: Int = 0

    private def mkArray(size: Int): Array[Short] = {
      val newelems = new Array[Short](size)
      if (this.size > 0) Array.copy(elems, 0, newelems, 0, this.size)
      newelems
    }

    private def resize(size: Int) {
      elems = mkArray(size)
      capacity = size
    }

    override def sizeHint(size: Int) {
      if (capacity < size) resize(size)
    }

    private def ensureSize(size: Int) {
      if (capacity < size || capacity == 0) {
        var newsize = if (capacity == 0) 16 else capacity * 2
        while (newsize < size) newsize *= 2
        resize(newsize)
      }
    }

    def +=(elem: Short): this.type = {
      ensureSize(size + 1)
      elems(size) = elem
      size += 1
      this
    }

    override def ++=(xs: TraversableOnce[Short]): this.type = xs match {
      case xs: WrappedArray.ofShort =>
        ensureSize(this.size + xs.length)
        Array.copy(xs.array, 0, elems, this.size, xs.length)
        size += xs.length
        this
      case _ =>
        super.++=(xs)
    }

    def clear() { size = 0 }

    def result() = {
      if (capacity != 0 && capacity == size) {
        capacity = 0
        elems
      }
      else mkArray(size)
    }

    override def equals(other: Any): Boolean = other match {
      case x: ofShort => (size == x.size) && (elems == x.elems)
      case _ => false
    }

    override def toString = "ArrayBuilder.ofShort"
  }

  /** A class for array builders for arrays of `char`s. It can be reused. */
  final class ofChar extends ArrayBuilder[Char] {

    private var elems: Array[Char] = _
    private var capacity: Int = 0
    private var size: Int = 0

    private def mkArray(size: Int): Array[Char] = {
      val newelems = new Array[Char](size)
      if (this.size > 0) Array.copy(elems, 0, newelems, 0, this.size)
      newelems
    }

    private def resize(size: Int) {
      elems = mkArray(size)
      capacity = size
    }

    override def sizeHint(size: Int) {
      if (capacity < size) resize(size)
    }

    private def ensureSize(size: Int) {
      if (capacity < size || capacity == 0) {
        var newsize = if (capacity == 0) 16 else capacity * 2
        while (newsize < size) newsize *= 2
        resize(newsize)
      }
    }

    def +=(elem: Char): this.type = {
      ensureSize(size + 1)
      elems(size) = elem
      size += 1
      this
    }

    override def ++=(xs: TraversableOnce[Char]): this.type = xs match {
      case xs: WrappedArray.ofChar =>
        ensureSize(this.size + xs.length)
        Array.copy(xs.array, 0, elems, this.size, xs.length)
        size += xs.length
        this
      case _ =>
        super.++=(xs)
    }

    def clear() { size = 0 }

    def result() = {
      if (capacity != 0 && capacity == size) {
        capacity = 0
        elems
      }
      else mkArray(size)
    }

    override def equals(other: Any): Boolean = other match {
      case x: ofChar => (size == x.size) && (elems == x.elems)
      case _ => false
    }

    override def toString = "ArrayBuilder.ofChar"
  }

  /** A class for array builders for arrays of `int`s. It can be reused. */
  final class ofInt extends ArrayBuilder[Int] {

    private var elems: Array[Int] = _
    private var capacity: Int = 0
    private var size: Int = 0

    private def mkArray(size: Int): Array[Int] = {
      val newelems = new Array[Int](size)
      if (this.size > 0) Array.copy(elems, 0, newelems, 0, this.size)
      newelems
    }

    private def resize(size: Int) {
      elems = mkArray(size)
      capacity = size
    }

    override def sizeHint(size: Int) {
      if (capacity < size) resize(size)
    }

    private def ensureSize(size: Int) {
      if (capacity < size || capacity == 0) {
        var newsize = if (capacity == 0) 16 else capacity * 2
        while (newsize < size) newsize *= 2
        resize(newsize)
      }
    }

    def +=(elem: Int): this.type = {
      ensureSize(size + 1)
      elems(size) = elem
      size += 1
      this
    }

    override def ++=(xs: TraversableOnce[Int]): this.type = xs match {
      case xs: WrappedArray.ofInt =>
        ensureSize(this.size + xs.length)
        Array.copy(xs.array, 0, elems, this.size, xs.length)
        size += xs.length
        this
      case _ =>
        super.++=(xs)
    }

    def clear() { size = 0 }

    def result() = {
      if (capacity != 0 && capacity == size) {
        capacity = 0
        elems
      }
      else mkArray(size)
    }

    override def equals(other: Any): Boolean = other match {
      case x: ofInt => (size == x.size) && (elems == x.elems)
      case _ => false
    }

    override def toString = "ArrayBuilder.ofInt"
  }

  /** A class for array builders for arrays of `long`s. It can be reused. */
  final class ofLong extends ArrayBuilder[Long] {

    private var elems: Array[Long] = _
    private var capacity: Int = 0
    private var size: Int = 0

    private def mkArray(size: Int): Array[Long] = {
      val newelems = new Array[Long](size)
      if (this.size > 0) Array.copy(elems, 0, newelems, 0, this.size)
      newelems
    }

    private def resize(size: Int) {
      elems = mkArray(size)
      capacity = size
    }

    override def sizeHint(size: Int) {
      if (capacity < size) resize(size)
    }

    private def ensureSize(size: Int) {
      if (capacity < size || capacity == 0) {
        var newsize = if (capacity == 0) 16 else capacity * 2
        while (newsize < size) newsize *= 2
        resize(newsize)
      }
    }

    def +=(elem: Long): this.type = {
      ensureSize(size + 1)
      elems(size) = elem
      size += 1
      this
    }

    override def ++=(xs: TraversableOnce[Long]): this.type = xs match {
      case xs: WrappedArray.ofLong =>
        ensureSize(this.size + xs.length)
        Array.copy(xs.array, 0, elems, this.size, xs.length)
        size += xs.length
        this
      case _ =>
        super.++=(xs)
    }

    def clear() { size = 0 }

    def result() = {
      if (capacity != 0 && capacity == size) {
        capacity = 0
        elems
      }
      else mkArray(size)
    }

    override def equals(other: Any): Boolean = other match {
      case x: ofLong => (size == x.size) && (elems == x.elems)
      case _ => false
    }

    override def toString = "ArrayBuilder.ofLong"
  }

  /** A class for array builders for arrays of `float`s. It can be reused. */
  final class ofFloat extends ArrayBuilder[Float] {

    private var elems: Array[Float] = _
    private var capacity: Int = 0
    private var size: Int = 0

    private def mkArray(size: Int): Array[Float] = {
      val newelems = new Array[Float](size)
      if (this.size > 0) Array.copy(elems, 0, newelems, 0, this.size)
      newelems
    }

    private def resize(size: Int) {
      elems = mkArray(size)
      capacity = size
    }

    override def sizeHint(size: Int) {
      if (capacity < size) resize(size)
    }

    private def ensureSize(size: Int) {
      if (capacity < size || capacity == 0) {
        var newsize = if (capacity == 0) 16 else capacity * 2
        while (newsize < size) newsize *= 2
        resize(newsize)
      }
    }

    def +=(elem: Float): this.type = {
      ensureSize(size + 1)
      elems(size) = elem
      size += 1
      this
    }

    override def ++=(xs: TraversableOnce[Float]): this.type = xs match {
      case xs: WrappedArray.ofFloat =>
        ensureSize(this.size + xs.length)
        Array.copy(xs.array, 0, elems, this.size, xs.length)
        size += xs.length
        this
      case _ =>
        super.++=(xs)
    }

    def clear() { size = 0 }

    def result() = {
      if (capacity != 0 && capacity == size) {
        capacity = 0
        elems
      }
      else mkArray(size)
    }

    override def equals(other: Any): Boolean = other match {
      case x: ofFloat => (size == x.size) && (elems == x.elems)
      case _ => false
    }

    override def toString = "ArrayBuilder.ofFloat"
  }

  /** A class for array builders for arrays of `double`s. It can be reused. */
  final class ofDouble extends ArrayBuilder[Double] {

    private var elems: Array[Double] = _
    private var capacity: Int = 0
    private var size: Int = 0

    private def mkArray(size: Int): Array[Double] = {
      val newelems = new Array[Double](size)
      if (this.size > 0) Array.copy(elems, 0, newelems, 0, this.size)
      newelems
    }

    private def resize(size: Int) {
      elems = mkArray(size)
      capacity = size
    }

    override def sizeHint(size: Int) {
      if (capacity < size) resize(size)
    }

    private def ensureSize(size: Int) {
      if (capacity < size || capacity == 0) {
        var newsize = if (capacity == 0) 16 else capacity * 2
        while (newsize < size) newsize *= 2
        resize(newsize)
      }
    }

    def +=(elem: Double): this.type = {
      ensureSize(size + 1)
      elems(size) = elem
      size += 1
      this
    }

    override def ++=(xs: TraversableOnce[Double]): this.type = xs match {
      case xs: WrappedArray.ofDouble =>
        ensureSize(this.size + xs.length)
        Array.copy(xs.array, 0, elems, this.size, xs.length)
        size += xs.length
        this
      case _ =>
        super.++=(xs)
    }

    def clear() { size = 0 }

    def result() = {
      if (capacity != 0 && capacity == size) {
        capacity = 0
        elems
      }
      else mkArray(size)
    }

    override def equals(other: Any): Boolean = other match {
      case x: ofDouble => (size == x.size) && (elems == x.elems)
      case _ => false
    }

    override def toString = "ArrayBuilder.ofDouble"
  }

  /** A class for array builders for arrays of `boolean`s. It can be reused. */
  class ofBoolean extends ArrayBuilder[Boolean] {

    private var elems: Array[Boolean] = _
    private var capacity: Int = 0
    private var size: Int = 0

    private def mkArray(size: Int): Array[Boolean] = {
      val newelems = new Array[Boolean](size)
      if (this.size > 0) Array.copy(elems, 0, newelems, 0, this.size)
      newelems
    }

    private def resize(size: Int) {
      elems = mkArray(size)
      capacity = size
    }

    override def sizeHint(size: Int) {
      if (capacity < size) resize(size)
    }

    private def ensureSize(size: Int) {
      if (capacity < size || capacity == 0) {
        var newsize = if (capacity == 0) 16 else capacity * 2
        while (newsize < size) newsize *= 2
        resize(newsize)
      }
    }

    def +=(elem: Boolean): this.type = {
      ensureSize(size + 1)
      elems(size) = elem
      size += 1
      this
    }

    override def ++=(xs: TraversableOnce[Boolean]): this.type = xs match {
      case xs: WrappedArray.ofBoolean =>
        ensureSize(this.size + xs.length)
        Array.copy(xs.array, 0, elems, this.size, xs.length)
        size += xs.length
        this
      case _ =>
        super.++=(xs)
    }

    def clear() { size = 0 }

    def result() = {
      if (capacity != 0 && capacity == size) {
        capacity = 0
        elems
      }
      else mkArray(size)
    }

    override def equals(other: Any): Boolean = other match {
      case x: ofBoolean => (size == x.size) && (elems == x.elems)
      case _ => false
    }

    override def toString = "ArrayBuilder.ofBoolean"
  }

  /** A class for array builders for arrays of `Unit` type. It can be reused. */
  final class ofUnit extends ArrayBuilder[Unit] {

    private var size: Int = 0

    def +=(elem: Unit): this.type = {
      size += 1
      this
    }

    override def ++=(xs: TraversableOnce[Unit]): this.type = {
      size += xs.size
      this
    }

    def clear() { size = 0 }

    def result() = {
      val ans = new Array[Unit](size)
      var i = 0
      while (i < size) { ans(i) = (); i += 1 }
      ans
    }

    override def equals(other: Any): Boolean = other match {
      case x: ofUnit => (size == x.size)
      case _ => false
    }

    override def toString = "ArrayBuilder.ofUnit"
  }
}
