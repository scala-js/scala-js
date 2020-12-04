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

package scala.scalajs

import scala.annotation.tailrec

package object runtime {

  import scala.scalajs.runtime.Compat._

  def wrapJavaScriptException(e: Any): Throwable = e match {
    case e: Throwable => e
    case _            => js.JavaScriptException(e)
  }

  def unwrapJavaScriptException(th: Throwable): Any = th match {
    case js.JavaScriptException(e) => e
    case _                         => th
  }

  @inline def toScalaVarArgs[A](array: js.Array[A]): Seq[A] =
    toScalaVarArgsImpl(array)

  @inline def toJSVarArgs[A](seq: Seq[A]): js.Array[A] =
    toJSVarArgsImpl(seq)

  /** Dummy method used to preserve the type parameter of
   *  `js.constructorOf[T]` through erasure.
   *
   *  An early phase of the compiler reroutes calls to `js.constructorOf[T]`
   *  into `runtime.constructorOf(classOf[T])`.
   *
   *  The `clazz` parameter must be a literal `classOf[T]` constant such that
   *  `T` represents a class extending `js.Any` (not a trait nor an object).
   */
  def constructorOf(clazz: Class[_ <: js.Any]): js.Dynamic =
    throw new Error("stub")

  /** Public access to `new ConstructorTag` for the codegen of
   *  `js.ConstructorTag.materialize`.
   */
  def newConstructorTag[T <: js.Any](constructor: js.Dynamic): js.ConstructorTag[T] =
    new js.ConstructorTag[T](constructor)

  /** Dummy method used to preserve where and how an inner JS class should be
   *  created.
   *
   *  @param clazz `classOf` of the class to be created
   *  @param superClass JS class value of the super class
   */
  def createInnerJSClass(clazz: Class[_], superClass: AnyRef): AnyRef =
    throw new Error("stub")

  /** Dummy method used to preserve where and how a local JS class should be
   *  created.
   *
   *  @param clazz
   *    `classOf` of the class to be created
   *  @param superClass
   *    JavaScript class value of the super class
   *  @param fakeNewInstances
   *    Fake `New` instantiations used to retrieve actual capture params
   */
  def createLocalJSClass(clazz: Class[_], superClass: AnyRef,
      fakeNewInstances: Array[AnyRef]): AnyRef = {
    throw new Error("stub")
  }

  /** Dummy method used to preserve a JS class value term associated with an
   *  expression tree.
   *
   *  This is used for:
   *  - New instances of nested JS classes and objects
   *  - Super calls in nested JS classes
   *
   *  @param jsclass
   *    The contextual JS class value
   *  @param inner
   *    The original inner tree
   */
  def withContextualJSClassValue[A](jsclass: AnyRef, inner: A): A =
    throw new Error("stub")

  @inline
  def privateFieldsSymbol(): Any =
    PrivateFieldsSymbolHolder.privateFieldsSymbol

  /** Information known at link-time, given the output configuration.
   *
   *  See [[LinkingInfo]] for details.
   */
  def linkingInfo: LinkingInfo = throw new Error("stub")

  /** Identity hash code of an object. */
  def identityHashCode(x: Object): Int = throw new Error("stub")

  def dynamicImport[A](thunk: DynamicImportThunk): js.Promise[A] =
    throw new Error("stub")
}
