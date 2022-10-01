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
