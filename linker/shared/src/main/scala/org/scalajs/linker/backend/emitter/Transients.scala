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

package org.scalajs.linker.backend.emitter

import org.scalajs.ir.Position
import org.scalajs.ir.Printers._
import org.scalajs.ir.Transformers._
import org.scalajs.ir.Traversers._
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._

object Transients {

  /** Packs a `long` value from its `lo` and `hi` words. */
  final case class PackLong(lo: Tree, hi: Tree) extends Transient.Value {
    val tpe = LongType

    def traverse(traverser: Traverser): Unit = {
      traverser.traverse(lo)
      traverser.traverse(hi)
    }

    def transform(transformer: Transformer)(implicit pos: Position): Tree =
      Transient(PackLong(transformer.transform(lo), transformer.transform(hi)))

    def printIR(out: IRTreePrinter): Unit = {
      out.print("<packLong>(")
      out.print(lo)
      out.print(", ")
      out.print(hi)
      out.print(")")
    }
  }

  /** Extracts the `hi` word of a `long` value.
   *
   *  To extract the `lo` word, use a `UnaryOp.LongToInt`.
   */
  final case class ExtractLongHi(value: Tree) extends Transient.Value {
    val tpe = IntType

    def traverse(traverser: Traverser): Unit =
      traverser.traverse(value)

    def transform(transformer: Transformer)(implicit pos: Position): Tree =
      Transient(ExtractLongHi(transformer.transform(value)))

    def printIR(out: IRTreePrinter): Unit = {
      out.print("<hi>(")
      out.print(value)
      out.print(")")
    }
  }

  /** Casts `expr` to the given `tpe`, without any check.
   *
   *  `expr.tpe` and `tpe` must be subtypes of `AnyType`.
   *
   *  This operation is only valid if we know that `expr` is indeed a value of
   *  the given `tpe`.
   *
   *  `Cast` behaves like an unchecked `AsInstanceOf`, except that it does not
   *  convert `null` to the zero of primitive types. Attempting to cast `null`
   *  to a primitive type (that is not `NullType`) is undefined behavior.
   *
   *  `Cast` is not always a no-op. In some cases, a `Cast` may still have to
   *  be implemented using a conversion. For example, casting down from
   *  `jl.Character` to `char` requires to extract the primitive value from the
   *  box (although we know that the box is non-null, unlike with
   *  `AsInstanceOf`).
   */
  final case class Cast(expr: Tree, val tpe: Type) extends Transient.Value {
    def traverse(traverser: Traverser): Unit =
      traverser.traverse(expr)

    def transform(transformer: Transformer)(
        implicit pos: Position): Tree = {
      Transient(Cast(transformer.transform(expr), tpe))
    }

    def printIR(out: IRTreePrinter): Unit = {
      out.print(expr)
      out.print(".as![")
      out.print(tpe)
      out.print("]")
    }
  }

  /** Intrinsic for `System.arraycopy`.
   *
   *  This node *assumes* that `src` and `dest` are non-null. It is the
   *  responsibility of whoever creates a `SystemArrayCopy` to wrap those
   *  parameters with `CheckNotNull`s if necessary.
   */
  final case class SystemArrayCopy(src: Tree, srcPos: Tree, dest: Tree,
      destPos: Tree, length: Tree)
      extends Transient.Value {
    val tpe: Type = VoidType

    def traverse(traverser: Traverser): Unit = {
      traverser.traverse(src)
      traverser.traverse(srcPos)
      traverser.traverse(dest)
      traverser.traverse(destPos)
      traverser.traverse(length)
    }

    def transform(t: Transformer)(implicit pos: Position): Tree = {
      Transient(SystemArrayCopy(t.transform(src), t.transform(srcPos),
          t.transform(dest), t.transform(destPos), t.transform(length)))
    }

    def printIR(out: IRTreePrinter): Unit = {
      out.print("$systemArraycopy")
      out.printArgs(List(src, srcPos, dest, destPos, length))
    }
  }

  /** Intrinsic for the private method `ArrayBuilder.generic.zeroOf`.
   *
   *  This node *assumes* that `runtimeClass` is non-null. It is the
   *  responsibility of whoever creates a `ZeroOf` to wrap that parameter
   *  with `CheckNotNull`s if necessary.
   */
  final case class ZeroOf(runtimeClass: Tree) extends Transient.Value {
    /* The concrete value of ZeroOf will of course have a more concrete type.
     * However, if we knew this type, we could simply emit a plain literal.
     */
    val tpe: Type = AnyType

    def traverse(traverser: Traverser): Unit =
      traverser.traverse(runtimeClass)

    def transform(transformer: Transformer)(
        implicit pos: Position): Tree = {
      Transient(ZeroOf(transformer.transform(runtimeClass)))
    }

    def printIR(out: IRTreePrinter): Unit = {
      out.print("$zeroOf")
      out.printArgs(List(runtimeClass))
    }
  }

  /** Intrinsic for the private method `ArrayBuilder.generic.genericArrayBuilderResult`.
   *
   *  This node *assumes* that `elemClass` is non-null. It is the
   *  responsibility of whoever creates a `NativeArrayWrapper` to wrap that
   *  parameter with `CheckNotNull`s if necessary.
   */
  final case class NativeArrayWrapper(elemClass: Tree, nativeArray: Tree)(val tpe: Type)
      extends Transient.Value {

    def traverse(traverser: Traverser): Unit = {
      traverser.traverse(elemClass)
      traverser.traverse(nativeArray)
    }

    def transform(transformer: Transformer)(
        implicit pos: Position): Tree = {
      Transient(NativeArrayWrapper(transformer.transform(elemClass),
          transformer.transform(nativeArray))(tpe))
    }

    def printIR(out: IRTreePrinter): Unit = {
      out.print("$nativeArrayWrapper")
      out.printArgs(List(elemClass, nativeArray))
    }
  }

  /** Intrinsic for `obj.getClass().getName()`.
   *
   *  The argument's type must conform to `AnyNotNullType`.
   */
  final case class ObjectClassName(obj: Tree) extends Transient.Value {
    val tpe: Type = StringType

    def traverse(traverser: Traverser): Unit =
      traverser.traverse(obj)

    def transform(transformer: Transformer)(
        implicit pos: Position): Tree = {
      Transient(ObjectClassName(transformer.transform(obj)))
    }

    def printIR(out: IRTreePrinter): Unit = {
      out.print("$objectClassName")
      out.printArgs(List(obj))
    }
  }

  /** Gets the unique instance of `DataView` used for floating point bit manipulation.
   *
   *  When linking for ES 5.1, the resulting value can be `null`.
   */
  final case object GetFPBitsDataView extends Transient.Value {
    val tpe: Type = AnyType

    def traverse(traverser: Traverser): Unit = ()

    def transform(transformer: Transformer)(implicit pos: Position): Tree =
      Transient(this)

    def printIR(out: IRTreePrinter): Unit =
      out.print("$fpBitsDataView")
  }

  /** Copies a primitive `Array` into a new appropriate `TypedArray`.
   *
   *  This node accepts `null` values for `expr`. Its implementation takes care
   *  of throwing `NullPointerException`s as required.
   */
  final case class ArrayToTypedArray(expr: Tree, primRef: PrimRef) extends Transient.Value {
    val tpe: Type = AnyType

    def traverse(traverser: Traverser): Unit =
      traverser.traverse(expr)

    def transform(transformer: Transformer)(
        implicit pos: Position): Tree = {
      Transient(ArrayToTypedArray(transformer.transform(expr), primRef))
    }

    def printIR(out: IRTreePrinter): Unit = {
      out.print("$array2TypedArray[")
      out.print(primRef)
      out.print(']')
      out.printArgs(List(expr))
    }
  }

  /** Copies a `TypedArray` into a new `Array` of the specified type.
   *
   *  Invalid values of `expr` will cause `TypeError`s or other JavaScript
   *  exceptions, in an implementation-dependent way. It does not protect
   *  itself against values forged to look like typed arrays without being
   *  actual typed arrays.
   */
  final case class TypedArrayToArray(expr: Tree, primRef: PrimRef) extends Transient.Value {
    val tpe: Type = ArrayType(ArrayTypeRef.of(primRef), nullable = false, exact = true)

    def traverse(traverser: Traverser): Unit =
      traverser.traverse(expr)

    def transform(transformer: Transformer)(
        implicit pos: Position): Tree = {
      Transient(TypedArrayToArray(transformer.transform(expr), primRef))
    }

    def printIR(out: IRTreePrinter): Unit = {
      out.print("$typedArray2Array[")
      out.print(primRef)
      out.print(']')
      out.printArgs(List(expr))
    }
  }
}
