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

import scala.annotation.tailrec

import org.scalajs.ir._
import org.scalajs.ir.Names._
import org.scalajs.ir.OriginalName.NoOriginalName
import org.scalajs.ir.Types._

import org.scalajs.linker.backend.javascript.Trees._

/** Manages name generation for non-local, generated fields. */
private[emitter] final class VarGen(nameGen: NameGen,
    mentionedDangerousGlobalRefs: Set[String]) {

  import nameGen._

  def codegenVar(field: String, typeRef: NonArrayTypeRef)(
      implicit pos: Position): VarRef = {
    VarRef(codegenVarIdent(field, typeRef))
  }

  def codegenVar(field: String, className: ClassName)(implicit pos: Position): VarRef =
    codegenVar(field, genName(className))

  def codegenVar(field: String, className: ClassName, fieldName: FieldName)(
      implicit pos: Position): VarRef = {
    codegenVar(field, className, fieldName, NoOriginalName)
  }

  def codegenVar(field: String, className: ClassName, fieldName: FieldName,
      origName: OriginalName)(
      implicit pos: Position): VarRef = {
    codegenVar(field, genName(className) + "__" + genName(fieldName), origName)
  }

  def codegenVar(field: String, className: ClassName, methodName: MethodName)(
      implicit pos: Position): VarRef = {
    codegenVar(field, className, methodName, NoOriginalName)
  }

  def codegenVar(field: String, className: ClassName, methodName: MethodName,
      origName: OriginalName)(
      implicit pos: Position): VarRef = {
    codegenVar(field, genName(className) + "__" + genName(methodName), origName)
  }

  def codegenVar(field: String, subField: String,
      origName: OriginalName = NoOriginalName)(
      implicit pos: Position): VarRef = {
    VarRef(codegenVarIdent(field, subField, origName))
  }

  def codegenVarIdent(field: String, typeRef: NonArrayTypeRef)(
      implicit pos: Position): Ident = {
    // The mapping in this function is an implementation detail of the emitter
    val subField = typeRef match {
      case PrimRef(tpe) =>
        tpe match {
          case NoType      => "V"
          case BooleanType => "Z"
          case CharType    => "C"
          case ByteType    => "B"
          case ShortType   => "S"
          case IntType     => "I"
          case LongType    => "J"
          case FloatType   => "F"
          case DoubleType  => "D"
          case NullType    => "N"
          case NothingType => "E"
        }
      case ClassRef(className) =>
        genName(className)
    }
    codegenVarIdent(field, subField)
  }

  def codegenVarIdent(field: String, subField: String,
      origName: OriginalName = NoOriginalName)(
      implicit pos: Position): Ident = {
    Ident(avoidClashWithGlobalRef("$" + field + "_" + subField), origName)
  }

  def codegenVar(field: String)(implicit pos: Position): VarRef =
    VarRef(codegenVarIdent(field))

  def codegenVarIdent(field: String)(implicit pos: Position): Ident =
    codegenVarIdent(field, NoOriginalName)

  def codegenVarIdent(field: String, origName: OriginalName)(
      implicit pos: Position): Ident = {
    Ident(avoidClashWithGlobalRef("$" + field), origName)
  }

  private def avoidClashWithGlobalRef(codegenVarName: String): String = {
    /* This is not cached because it should virtually never happen.
     * slowPath() is only called if we use a dangerous global ref, which should
     * already be very rare. And if do a second iteration in the loop only if
     * we refer to the global variables `$foo` *and* `$$foo`. At this point the
     * likelihood is so close to 0 that caching would be more expensive than
     * not caching.
     */
    @tailrec
    def slowPath(lastNameTried: String): String = {
      val nextNameToTry = "$" + lastNameTried
      if (mentionedDangerousGlobalRefs.contains(nextNameToTry))
        slowPath(nextNameToTry)
      else
        nextNameToTry
    }

    /* Hopefully this is JIT'ed away as `false` because
     * `mentionedDangerousGlobalRefs` is in fact `Set.EmptySet`.
     */
    if (mentionedDangerousGlobalRefs.contains(codegenVarName))
      slowPath(codegenVarName)
    else
      codegenVarName
  }
}
