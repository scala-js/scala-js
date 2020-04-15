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

/** Manages name generation for non-local, generated fields.
 *
 *  We distinguish three types of linker generated identifiers:
 *
 *  - classVar: Vars pertaining to a given class.
 *  - coreJSLibVar: Vars defined by the CoreJSLib.
 *  - fileLevelVar: Vars that are local to an individual file.
 *
 *  All of these have `*Ident` variants (e.g. `classVarIdent`). These are
 *  intended to be used for definitions.
 *
 *  At the moment, these distinctions are theoretical. They will become relevant
 *  for module splitting (#2681).
 */
private[emitter] final class VarGen(nameGen: NameGen,
    mentionedDangerousGlobalRefs: Set[String]) {

  import nameGen._

  // ClassName scoped.

  def classVar(field: String, className: ClassName)(implicit pos: Position): Tree =
    VarRef(classVarIdent(field, className))

  def classVarIdent(field: String, className: ClassName)(
      implicit pos: Position): Ident = {
    genericIdent(field, genName(className))
  }

  // ClassName, FieldName scoped.

  def classVar(field: String, className: ClassName, fieldName: FieldName)(
      implicit pos: Position): Tree = {
    classVar(field, className, fieldName, NoOriginalName)
  }

  def classVar(field: String, className: ClassName, fieldName: FieldName,
      origName: OriginalName)(
      implicit pos: Position): Tree = {
    VarRef(classVarIdent(field, className, fieldName, origName))
  }

  def classVarIdent(field: String, className: ClassName, fieldName: FieldName,
      origName: OriginalName)(implicit pos: Position): Ident = {
    genericIdent(field, genName(className) + "__" + genName(fieldName), origName)
  }

  // ClassName, MethodName scoped.

  def classVar(field: String, className: ClassName, methodName: MethodName)(
      implicit pos: Position): Tree = {
    classVar(field, className, methodName, NoOriginalName)
  }

  def classVar(field: String, className: ClassName, methodName: MethodName,
      origName: OriginalName)(
      implicit pos: Position): Tree = {
    VarRef(classVarIdent(field, className, methodName, origName))
  }

  def classVarIdent(field: String, className: ClassName, methodName: MethodName,
      origName: OriginalName)(implicit pos: Position): Ident = {
    genericIdent(field, genName(className) + "__" + genName(methodName), origName)
  }

  /** Dispatch based on type ref.
   *
   *  Returns the relevant coreJSLibVar for primitive types, classVar otherwise.
   */
  def typeRefVar(field: String, typeRef: NonArrayTypeRef)(
      implicit pos: Position): Tree = {
    typeRef match {
      case primRef: PrimRef =>
        VarRef(coreJSLibVarIdent(field, primRef))

      case ClassRef(className) =>
        classVar(field, className)
    }
  }

  def coreJSLibVar(field: String)(implicit pos: Position): Tree =
    VarRef(coreJSLibVarIdent(field))

  def coreJSLibVarIdent(field: String)(implicit pos: Position): Ident =
    genericIdent(field)

  def coreJSLibVarIdent(field: String, primRef: PrimRef)(
      implicit pos: Position): Ident = {
    // The mapping in this function is an implementation detail of the emitter
    val subField = primRef.tpe match {
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
    genericIdent(field, subField)
  }

  def fileLevelVar(field: String, subField: String,
      origName: OriginalName = NoOriginalName)(
      implicit pos: Position): VarRef = {
    VarRef(fileLevelVarIdent(field, subField, origName))
  }

  def fileLevelVar(field: String)(implicit pos: Position): VarRef =
    VarRef(fileLevelVarIdent(field))

  def fileLevelVarIdent(field: String, subField: String,
      origName: OriginalName = NoOriginalName)(
      implicit pos: Position): Ident = {
    genericIdent(field, subField, origName)
  }

  def fileLevelVarIdent(field: String)(implicit pos: Position): Ident =
    fileLevelVarIdent(field, NoOriginalName)

  def fileLevelVarIdent(field: String, origName: OriginalName)(
      implicit pos: Position): Ident = {
    genericIdent(field, origName)
  }

  private def genericIdent(field: String)(
      implicit pos: Position): Ident = {
    genericIdent(field, NoOriginalName)
  }

  private def genericIdent(field: String, origName: OriginalName)(
      implicit pos: Position): Ident = {
    Ident(avoidClashWithGlobalRef("$" + field), origName)
  }

  private def genericIdent(field: String, subField: String,
      origName: OriginalName = NoOriginalName)(
      implicit pos: Position): Ident = {
    Ident(avoidClashWithGlobalRef("$" + field + "_" + subField), origName)
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
