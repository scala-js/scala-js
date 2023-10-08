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
import org.scalajs.linker.interface.ModuleKind
import org.scalajs.linker.standard.ModuleSet.ModuleID

/** Manages name generation for non-local, generated fields.
 *
 *  We distinguish two types of linker generated identifiers:
 *
 *  - globalVar: Vars accessible in the entire generated JS program
 *    (typically pertaining to a given class).
 *  - fileLevelVar: Vars that are local to an individual file.
 *
 *  `globalVar`s have `*Def` variants (e.g. `classFunctionDef`) to define them.
 */
private[emitter] final class VarGen(jsGen: JSGen, nameGen: NameGen,
    mentionedDangerousGlobalRefs: Set[String]) {

  import jsGen._
  import nameGen._

  def globalVar[T: Scope](field: String, scope: T,
      origName: OriginalName = NoOriginalName)(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      pos: Position): Tree = {
    val ident = globalVarIdent(field, scope, origName)
    foldSameModule[T, Tree](scope) {
      VarRef(ident)
    } { moduleID =>
      DotSelect(VarRef(internalModuleFieldIdent(moduleID)), ident)
    }
  }

  def globalClassDef[T: Scope](field: String, scope: T,
      parentClass: Option[Tree], members: List[Tree],
      origName: OriginalName = NoOriginalName)(
      implicit moduleContext: ModuleContext, pos: Position): WithGlobals[Tree] = {
    val ident = globalVarIdent(field, scope, origName)
    maybeExport(ident, ClassDef(Some(ident), parentClass, members), mutable = false)
  }

  def globalFunctionDef[T: Scope](field: String, scope: T,
      args: List[ParamDef], restParam: Option[ParamDef], body: Tree,
      origName: OriginalName = NoOriginalName)(
      implicit moduleContext: ModuleContext, pos: Position): WithGlobals[Tree] = {
    val ident = globalVarIdent(field, scope, origName)
    maybeExport(ident, FunctionDef(ident, args, restParam, body), mutable = false)
  }

  def globalVarDef[T: Scope](field: String, scope: T, value: Tree,
      origName: OriginalName = NoOriginalName)(
      implicit moduleContext: ModuleContext, pos: Position): WithGlobals[Tree] = {
    val ident = globalVarIdent(field, scope, origName)
    maybeExport(ident, genConst(ident, value), mutable = false)
  }

  /** Attention: A globalVarDecl may only be modified from the module it was declared in. */
  def globalVarDecl[T: Scope](field: String, scope: T,
      origName: OriginalName = NoOriginalName)(
      implicit moduleContext: ModuleContext, pos: Position): WithGlobals[Tree] = {
    val ident = globalVarIdent(field, scope, origName)
    maybeExport(ident, genEmptyMutableLet(ident), mutable = true)
  }

  /** Unlike a mutable VarDecl, a globallyMutableVarDef may be modified from any
   *  module. As such, an additional field needs to be provided for an
   *  additional setter. This is used when generating ES modules.
   */
  def globallyMutableVarDef[T: Scope](field: String, setterField: String,
      scope: T, value: Tree, origName: OriginalName = NoOriginalName)(
      implicit moduleContext: ModuleContext, pos: Position): WithGlobals[Tree] = {
    val ident = globalVarIdent(field, scope, origName)
    val varDef = genLet(ident, mutable = true, value)

    if (config.moduleKind == ModuleKind.ESModule && !moduleContext.public) {
      val setterIdent = globalVarIdent(setterField, scope)
      val x = Ident("x")
      val setter = FunctionDef(setterIdent, List(ParamDef(x)), None, {
        Assign(VarRef(ident), VarRef(x))
      })

      val exports =
        Export(genExportIdent(ident) :: genExportIdent(setterIdent) :: Nil)

      WithGlobals(Block(varDef, setter, exports))
    } else {
      maybeExport(ident, varDef, mutable = true)
    }
  }

  /** Whether the var setter needs to be used on the given scope. */
  def needToUseGloballyMutableVarSetter[T](scope: T)(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      scopeType: Scope[T]): Boolean = {
    config.moduleKind == ModuleKind.ESModule &&
    globalKnowledge.getModule(scopeType.reprClass(scope)) != moduleContext.moduleID
  }

  def globalVarExport[T: Scope](field: String, scope: T, exportName: ExportName,
      origName: OriginalName = NoOriginalName)(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      pos: Position): Tree = {
    assert(config.moduleKind == ModuleKind.ESModule)

    val ident = globalVarIdent(field, scope, origName)
    foldSameModule[T, Tree](scope) {
      Export((ident -> exportName) :: Nil)
    } { moduleID =>
      val importName = ExportName(ident.name)
      val moduleName = config.internalModulePattern(moduleID)
      ExportImport((importName -> exportName) :: Nil, StringLiteral(moduleName))
    }
  }

  /** Apply the provided body to a dynamically loaded global var */
  def withDynamicGlobalVar[T: Scope](field: String, scope: T)(body: Tree => Tree)(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      pos: Position): WithGlobals[Tree] = {
    val ident = globalVarIdent(field, scope)

    val module = fileLevelVarIdent("$module")

    def unitPromise = {
      globalRef("Promise").map { promise =>
        Apply(genIdentBracketSelect(promise, "resolve"), List(Undefined()))
      }
    }

    def genThen(receiver: Tree, expr: Tree) = {
      Apply(genIdentBracketSelect(receiver, "then"), List(
          genArrowFunction(List(ParamDef(module)), None, Return(expr))))
    }

    foldSameModule(scope) {
      unitPromise.map { promise =>
        genThen(promise, body(VarRef(ident)))
      }
    } { moduleID =>
      val moduleName = config.internalModulePattern(moduleID)

      val moduleTree = config.moduleKind match {
        case ModuleKind.NoModule =>
          /* If we get here, it means that what we are trying to import is in a
           * different module than the module we're currently generating
           * (otherwise we'd take the other branch of `foldSameModule`). But
           * that in turn means we have more than one module, which is not
           * allowed in NoModule (and should have been caught by the analyzer).
           */
          throw new AssertionError("multiple modules in NoModule mode")

        case ModuleKind.ESModule =>
          WithGlobals(ImportCall(StringLiteral(moduleName)))

        case ModuleKind.CommonJSModule =>
          for {
            promise <- unitPromise
            require <- globalRef("require")
          } yield {
            genThen(promise, Apply(require, List(StringLiteral(moduleName))))
          }
      }

      moduleTree.map { mod =>
        genThen(mod, body(DotSelect(VarRef(module), ident)))
      }
    }
  }

  private def globalVarIdent[T](field: String, scope: T,
      origName: OriginalName = NoOriginalName)(
      implicit pos: Position, scopeType: Scope[T]): Ident = {
    genericIdent(field, scopeType.subField(scope), origName)
  }

  private def foldSameModule[T, S](scope: T)(same: => S)(other: ModuleID => S)(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      scopeType: Scope[T]): S = {
    val reprClass = scopeType.reprClass(scope)
    val targetModule = globalKnowledge.getModule(reprClass)
    if (targetModule == moduleContext.moduleID) same
    else other(targetModule)
  }

  /** Dispatch based on type ref.
   *
   *  Returns the relevant coreJSLibVar for primitive types, globalVar otherwise.
   */
  def typeRefVar(field: String, typeRef: NonArrayTypeRef)(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      pos: Position): Tree = {
    /* Explicitly bringing `PrimRefScope` and `ClassScope` as local implicit
     * vals should not be necessary, and used not to be there. When upgrading
     * to sbt 1.5.4 from 1.2.8, compilation started failing because of missing
     * implicit `Scope[PrimRef]` and `Scope[ClassName]` in this method, in
     * *some* environments (depending on machine, OS, JDK version, in or out
     * IDE, regular compile versus Scaladoc, etc., perhaps the phase of the
     * moon, for all I could tell). It is not even clear that the sbt version
     * is actually relevant.
     * Regardless, the explicit vals reliably fix the issue.
     */
    typeRef match {
      case primRef: PrimRef =>
        implicit val primRefScope: Scope[PrimRef] = Scope.PrimRefScope
        globalVar(field, primRef)

      case ClassRef(className) =>
        implicit val classScope: Scope[ClassName] = Scope.ClassScope
        globalVar(field, className)
    }
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
    genericIdent(field, "", origName)
  }

  def externalModuleFieldIdent(moduleName: String)(implicit pos: Position): Ident =
    fileLevelVarIdent("i", genModuleName(moduleName), OriginalName(moduleName))

  def internalModuleFieldIdent(module: ModuleID)(implicit pos: Position): Ident =
    fileLevelVarIdent("j", genModuleName(module.id), OriginalName(module.id))

  private def genericIdent(field: String, subField: String,
      origName: OriginalName = NoOriginalName)(
      implicit pos: Position): Ident = {
    val name =
      if (subField == "") ("$" + field).intern()
      else "$" + field + "_" + subField

    Ident(avoidClashWithGlobalRef(name), origName)
  }

  private def maybeExport(ident: Ident, tree: Tree, mutable: Boolean)(
      implicit moduleContext: ModuleContext, pos: Position): WithGlobals[Tree] = {
    if (moduleContext.public) {
      WithGlobals(tree)
    } else {
      val exportStat = config.moduleKind match {
        case ModuleKind.NoModule =>
          throw new AssertionError("non-public module in NoModule mode")

        case ModuleKind.ESModule =>
          WithGlobals(Export(genExportIdent(ident) :: Nil))

        case ModuleKind.CommonJSModule =>
          globalRef("exports").flatMap { exportsVarRef =>
            val name = StringLiteral(ident.name)

            if (mutable) {
              val x = Ident("x")
              genDefineProperty(exportsVarRef, name, List(
                  "get" -> Function(arrow = false, Nil, None, Return(VarRef(ident))),
                  "set" -> Function(arrow = false, List(ParamDef(x)), None, {
                      Assign(VarRef(ident), VarRef(x))
                  }),
                  "configurable" -> BooleanLiteral(true)
              ))
            } else {
              WithGlobals(Assign(genBracketSelect(exportsVarRef, name), VarRef(ident)))
            }
          }
      }

      exportStat.map(Block(tree, _))
    }
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

  /** Scopes a globalVar to a certain sub field. */
  trait Scope[T] {
    def subField(x: T): String
    def reprClass(x: T): ClassName
  }

  /** Marker value for a CoreJSLibVar. */
  object CoreVar

  object Scope {
    implicit object ClassScope extends Scope[ClassName] {
      def subField(x: ClassName): String = genName(x)
      def reprClass(x: ClassName): ClassName = x
    }

    implicit object FieldScope extends Scope[(ClassName, FieldName)] {
      def subField(x: (ClassName, FieldName)): String =
        genName(x._1) + "__" + genName(x._2)

      def reprClass(x: (ClassName, FieldName)): ClassName = x._1
    }

    implicit object MethodScope extends Scope[(ClassName, MethodName)] {
      def subField(x: (ClassName, MethodName)): String =
        genName(x._1) + "__" + genName(x._2)

      def reprClass(x: (ClassName, MethodName)): ClassName = x._1
    }

    implicit object CoreJSLibScope extends Scope[CoreVar.type] {
      def subField(x: CoreVar.type): String = ""
      def reprClass(x: CoreVar.type): ClassName = ObjectClass
    }

    /** The PrimRefScope is implied to be in the CoreJSLib. */
    implicit object PrimRefScope extends Scope[PrimRef] {
      def subField(x: PrimRef): String = {
        // The mapping in this function is an implementation detail of the emitter
        x.tpe match {
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
      }

      def reprClass(x: PrimRef): ClassName = ObjectClass
    }

    implicit object NonArrayTypeRefScope extends Scope[NonArrayTypeRef] {
      def subField(x: NonArrayTypeRef): String = x match {
        case ClassRef(className) => ClassScope.subField(className)
        case x: PrimRef          => PrimRefScope.subField(x)
      }

      def reprClass(x: Types.NonArrayTypeRef): Names.ClassName = x match {
        case ClassRef(className) => ClassScope.reprClass(className)
        case x: PrimRef          => PrimRefScope.reprClass(x)
      }
    }
  }

  private def genExportIdent(ident: Ident): (Ident, ExportName) = {
    implicit val pos = ident.pos
    ident -> ExportName(ident.name)
  }
}
