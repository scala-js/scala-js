/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js IR                **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2014, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.ir

import scala.annotation.tailrec

object Types {

  /** Type of an expression in the IR. */
  abstract sealed class Type {
    def show(): String = {
      val writer = new java.io.StringWriter
      val printer = new Printers.IRTreePrinter(writer, jsMode = false)
      printer.printType(this)
      writer.toString()
    }
  }

  /** Any type (the top type of this type system).
   *  A variable of this type can contain any value, including `undefined`
   *  and `null` and any raw JS value. This type supports a very limited set
   *  of operations, the ones common to all values. Basically only toString(),
   *  equality tests and instance tests.
   *  The type java.lang.Object in the back-end maps to [[AnyType]] because it
   *  can hold raw JS values (not only instances of Scala.js classes).
   */
  case object AnyType extends Type

  /** Nothing type (the bottom type of this type system).
   *  Expressions from which one can never come back are typed as [[Nothing]].
   *  For example, `throw` and `return`.
   */
  case object NothingType extends Type

  /** The type of `undefined`. */
  case object UndefType extends Type

  /** Boolean type.
   *  It does not accept `null` nor `undefined`.
   */
  case object BooleanType extends Type

  /** 32-bit signed integer type.
   *  It does not accept `null` nor `undefined`.
   */
  case object IntType extends Type

  /** Double type.
   *  It does not accept `null` nor `undefined`.
   */
  case object DoubleType extends Type

  /** String type.
   *  It does not accept `null` nor `undefined`.
   */
  case object StringType extends Type

  /** The type of `null`.
   *  It does not accept `undefined`.
   *  The null type is a subtype of all class types and array types.
   */
  case object NullType extends Type

  /** Reference types (allowed for classOf[], is/asInstanceOf[]). */
  sealed abstract class ReferenceType extends Type

  /** Class (or interface) type. */
  final case class ClassType(className: String) extends ReferenceType

  /** Array type. */
  final case class ArrayType(baseClassName: String, dimensions: Int) extends ReferenceType

  object ArrayType {
    def apply(innerType: ReferenceType): ArrayType = innerType match {
      case ClassType(className)      => ArrayType(className, 1)
      case ArrayType(className, dim) => ArrayType(className, dim + 1)
    }
  }

  /** Dynamic type.
   *  Used for raw JavaScript values, among others.
   *  A variable of this type can contain any value (just like [[AnyType]]).
   *  Unlike [[AnyType]], all JavaScript operations are permitted on an
   *  expression of this type.
   */
  case object DynType extends Type

  /** No type. */
  case object NoType extends Type
}
