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

  /** Checks that `obj ne null`, then returns `obj`.
   *
   *  If `obj eq null`, throw a `NullPointerException`, or a corresponding
   *  `UndefinedBehaviorError`.
   *
   *  This node must not be used when NPEs are Unchecked.
   */
  final case class CheckNotNull(obj: Tree) extends Transient.Value {
    val tpe: Type = if (obj.tpe == NullType) NothingType else obj.tpe

    def traverse(traverser: Traverser): Unit =
      traverser.traverse(obj)

    def transform(transformer: Transformer, isStat: Boolean)(
        implicit pos: Position): Tree = {
      Transient(CheckNotNull(transformer.transformExpr(obj)))
    }

    def printIR(out: IRTreePrinter): Unit = {
      out.print("$n")
      out.printArgs(List(obj))
    }
  }

  /** Assumes that `obj ne null`, and always returns `obj`.
   *
   *  This is used by the optimizer to communicate to the emitter that an
   *  expression is known not to be `null`, so that it doesn't insert useless
   *  `null` checks.
   *
   *  This node should not be used when NPEs are Unchecked.
   */
  final case class AssumeNotNull(obj: Tree) extends Transient.Value {
    val tpe: Type = obj.tpe

    def traverse(traverser: Traverser): Unit =
      traverser.traverse(obj)

    def transform(transformer: Transformer, isStat: Boolean)(
        implicit pos: Position): Tree = {
      Transient(CheckNotNull(transformer.transformExpr(obj)))
    }

    def printIR(out: IRTreePrinter): Unit = {
      out.print(obj)
      out.print("!")
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
    val tpe: Type = NoType

    def traverse(traverser: Traverser): Unit = {
      traverser.traverse(src)
      traverser.traverse(srcPos)
      traverser.traverse(dest)
      traverser.traverse(destPos)
      traverser.traverse(length)
    }

    def transform(transformer: Transformer, isStat: Boolean)(
        implicit pos: Position): Tree = {
      import transformer.transformExpr

      Transient(SystemArrayCopy(transformExpr(src), transformExpr(srcPos),
          transformExpr(dest), transformExpr(destPos), transformExpr(length)))
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

    def transform(transformer: Transformer, isStat: Boolean)(
        implicit pos: Position): Tree = {
      Transient(ZeroOf(transformer.transformExpr(runtimeClass)))
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

    def transform(transformer: Transformer, isStat: Boolean)(
        implicit pos: Position): Tree = {
      Transient(NativeArrayWrapper(transformer.transformExpr(elemClass),
          transformer.transformExpr(nativeArray))(tpe))
    }

    def printIR(out: IRTreePrinter): Unit = {
      out.print("$nativeArrayWrapper")
      out.printArgs(List(elemClass, nativeArray))
    }
  }

  /** Intrinsic for `obj.getClass().getName()`.
   *
   *  This node accepts any value for `obj`, including `null`. Its
   *  implementation takes care of throwing `NullPointerException`s as
   *  required.
   */
  final case class ObjectClassName(obj: Tree) extends Transient.Value {
    val tpe: Type = StringType

    def traverse(traverser: Traverser): Unit =
      traverser.traverse(obj)

    def transform(transformer: Transformer, isStat: Boolean)(
        implicit pos: Position): Tree = {
      Transient(ObjectClassName(transformer.transformExpr(obj)))
    }

    def printIR(out: IRTreePrinter): Unit = {
      out.print("$objectClassName")
      out.printArgs(List(obj))
    }
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

    def transform(transformer: Transformer, isStat: Boolean)(
        implicit pos: Position): Tree = {
      Transient(ArrayToTypedArray(transformer.transformExpr(expr), primRef))
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
    val tpe: Type = ArrayType(ArrayTypeRef.of(primRef))

    def traverse(traverser: Traverser): Unit =
      traverser.traverse(expr)

    def transform(transformer: Transformer, isStat: Boolean)(
        implicit pos: Position): Tree = {
      Transient(TypedArrayToArray(transformer.transformExpr(expr), primRef))
    }

    def printIR(out: IRTreePrinter): Unit = {
      out.print("$typedArray2Array[")
      out.print(primRef)
      out.print(']')
      out.printArgs(List(expr))
    }
  }
}
