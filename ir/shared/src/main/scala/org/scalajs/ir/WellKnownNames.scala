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

package org.scalajs.ir

import Names._
import Types._

/** Names for "well-known" classes and methods.
 *
 *  Well-known classes and methods have a dedicated meaning in the semantics of
 *  the IR. For example, `java.lang.Class` is well-known because it is the type
 *  of `ClassOf` nodes.
 */
object WellKnownNames {

  /** `java.lang.Object`, the root of the class hierarchy. */
  val ObjectClass: ClassName = ClassName("java.lang.Object")

  /** `ClassRef(ObjectClass)`. */
  val ObjectRef: ClassRef = ClassRef(ObjectClass)

  // Hijacked classes
  val BoxedUnitClass: ClassName = ClassName("java.lang.Void")
  val BoxedBooleanClass: ClassName = ClassName("java.lang.Boolean")
  val BoxedCharacterClass: ClassName = ClassName("java.lang.Character")
  val BoxedByteClass: ClassName = ClassName("java.lang.Byte")
  val BoxedShortClass: ClassName = ClassName("java.lang.Short")
  val BoxedIntegerClass: ClassName = ClassName("java.lang.Integer")
  val BoxedLongClass: ClassName = ClassName("java.lang.Long")
  val BoxedFloatClass: ClassName = ClassName("java.lang.Float")
  val BoxedDoubleClass: ClassName = ClassName("java.lang.Double")
  val BoxedStringClass: ClassName = ClassName("java.lang.String")

  /** The set of all hijacked classes. */
  val HijackedClasses: Set[ClassName] = Set(
      BoxedUnitClass,
      BoxedBooleanClass,
      BoxedCharacterClass,
      BoxedByteClass,
      BoxedShortClass,
      BoxedIntegerClass,
      BoxedLongClass,
      BoxedFloatClass,
      BoxedDoubleClass,
      BoxedStringClass
  )

  /** Map from hijacked classes to their respective primitive types. */
  val BoxedClassToPrimType: Map[ClassName, PrimType] = Map(
    BoxedUnitClass -> UndefType,
    BoxedBooleanClass -> BooleanType,
    BoxedCharacterClass -> CharType,
    BoxedByteClass -> ByteType,
    BoxedShortClass -> ShortType,
    BoxedIntegerClass -> IntType,
    BoxedLongClass -> LongType,
    BoxedFloatClass -> FloatType,
    BoxedDoubleClass -> DoubleType,
    BoxedStringClass -> StringType
  )

  /** Map from primitive types to their respective boxed (hijacked) classes. */
  val PrimTypeToBoxedClass: Map[PrimType, ClassName] =
    BoxedClassToPrimType.map(_.swap)

  /** The class of things returned by `ClassOf` and `GetClass`. */
  val ClassClass: ClassName = ClassName("java.lang.Class")

  /** `java.lang.Cloneable`, which is an ancestor of array classes and is used
   *  by `Clone`.
   */
  val CloneableClass: ClassName = ClassName("java.lang.Cloneable")

  /** `java.io.Serializable`, which is an ancestor of array classes. */
  val SerializableClass: ClassName = ClassName("java.io.Serializable")

  /** The superclass of all throwables.
   *
   *  This is the result type of `WrapAsThrowable` nodes, as well as the input
   *  type of `UnwrapFromThrowable`.
   */
  val ThrowableClass = ClassName("java.lang.Throwable")

  /** The exception thrown by a division by 0. */
  val ArithmeticExceptionClass: ClassName =
    ClassName("java.lang.ArithmeticException")

  /** The exception thrown by an `ArraySelect` that is out of bounds. */
  val ArrayIndexOutOfBoundsExceptionClass: ClassName =
    ClassName("java.lang.ArrayIndexOutOfBoundsException")

  /** The exception thrown by an `Assign(ArraySelect, ...)` where the value cannot be stored. */
  val ArrayStoreExceptionClass: ClassName =
    ClassName("java.lang.ArrayStoreException")

  /** The exception thrown by a `NewArray(...)` with a negative size. */
  val NegativeArraySizeExceptionClass: ClassName =
    ClassName("java.lang.NegativeArraySizeException")

  /** The exception thrown by a variety of nodes for `null` arguments.
   *
   *  - `Apply` and `ApplyStatically` for the receiver,
   *  - `Select` for the qualifier,
   *  - `ArrayLength` and `ArraySelect` for the array,
   *  - `GetClass`, `Clone` and `UnwrapFromException` for their respective only arguments.
   */
  val NullPointerExceptionClass: ClassName =
    ClassName("java.lang.NullPointerException")

  /** The exception thrown by a `BinaryOp.String_charAt` that is out of bounds. */
  val StringIndexOutOfBoundsExceptionClass: ClassName =
    ClassName("java.lang.StringIndexOutOfBoundsException")

  /** The exception thrown by an `AsInstanceOf` that fails. */
  val ClassCastExceptionClass: ClassName =
    ClassName("java.lang.ClassCastException")

  /** The exception thrown by a `Class_newArray` if the first argument is `classOf[Unit]`. */
  val IllegalArgumentExceptionClass: ClassName =
    ClassName("java.lang.IllegalArgumentException")

  val RuntimeClass =
    ClassName("scala.scalajs.runtime.package$")

  /** The set of classes and interfaces that are ancestors of array classes. */
  private[ir] val AncestorsOfPseudoArrayClass: Set[ClassName] = {
    /* This would logically be defined in Types, but that introduces a cyclic
     * dependency between the initialization of Names and Types.
     */
    Set(ObjectClass, CloneableClass, SerializableClass)
  }

  /** Name of a constructor without argument.
   *
   *  This is notably the signature of constructors of module classes.
   */
  final val NoArgConstructorName: MethodName =
    MethodName.constructor(Nil)

  /** Name of the static initializer method. */
  final val StaticInitializerName: MethodName =
    MethodName(SimpleMethodName.StaticInitializer, Nil, VoidRef)

  /** Name of the class initializer method. */
  final val ClassInitializerName: MethodName =
    MethodName(SimpleMethodName.ClassInitializer, Nil, VoidRef)

  final val toScalaVarArgs =
      MethodName("toScalaVarArgs",
      List(ClassRef(ClassName("scala.scalajs.js.Array"))),
      ClassRef(ClassName("scala.collection.immutable.Seq")))

  /** ModuleID of the default module */
  final val DefaultModuleID: String = "main"

}
