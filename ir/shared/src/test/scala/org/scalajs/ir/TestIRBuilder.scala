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

import scala.language.implicitConversions

import Names._
import OriginalName.NoOriginalName
import Printers._
import Trees._
import Types._

object TestIRBuilder {

  implicit val dummyPos: Position = Position.NoPosition

  /** Empty ApplyFlags, for short. */
  val EAF = ApplyFlags.empty

  /** No original name, for short. */
  val NON = NoOriginalName

  /** Unversioned, for short */
  val UNV = Version.Unversioned

  /** No optimizer hints, for short. */
  val NoOptHints = OptimizerHints.empty

  // String -> Name conversions
  implicit def string2simpleFieldName(name: String): SimpleFieldName =
    SimpleFieldName(name)
  implicit def string2className(name: String): ClassName =
    ClassName(name)

  // String -> Ident conversions
  implicit def string2localIdent(name: String): LocalIdent =
    LocalIdent(LocalName(name))
  implicit def string2labelIdent(name: String): LabelIdent =
    LabelIdent(LabelName(name))
  implicit def string2simpleFieldIdent(name: String): SimpleFieldIdent =
    SimpleFieldIdent(SimpleFieldName(name))
  implicit def string2classIdent(name: String): ClassIdent =
    ClassIdent(ClassName(name))

  // String -> Type and TypeRef conversions
  implicit def string2classType(className: String): ClassType =
    ClassType(ClassName(className), nullable = true)
  implicit def string2classRef(className: String): ClassRef =
    ClassRef(ClassName(className))

  // Name -> Ident conversions
  implicit def fieldName2fieldIdent(name: FieldName): FieldIdent =
    FieldIdent(name)
  implicit def methodName2methodIdent(name: MethodName): MethodIdent =
    MethodIdent(name)
  implicit def className2classRef(className: ClassName): ClassRef =
    ClassRef(className)
  implicit def className2classIdent(name: ClassName): ClassIdent =
    ClassIdent(name)

  val V = VoidRef
  val I = IntRef
  val O = ClassRef(ObjectClass)

  def b(value: Boolean): BooleanLiteral = BooleanLiteral(value)
  def i(value: Int): IntLiteral = IntLiteral(value)
  def l(value: Long): LongLiteral = LongLiteral(value)
  def f(value: Float): FloatLiteral = FloatLiteral(value)
  def d(value: Double): DoubleLiteral = DoubleLiteral(value)
  def s(value: String): StringLiteral = StringLiteral(value)

  def ref(ident: LocalIdent, tpe: Type): VarRef = VarRef(ident)(tpe)

  def arrayType(base: NonArrayTypeRef, dimensions: Int): ArrayType =
    ArrayType(ArrayTypeRef(base, dimensions), nullable = true)

}
