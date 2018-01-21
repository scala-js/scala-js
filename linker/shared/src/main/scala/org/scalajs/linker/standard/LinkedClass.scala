/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.linker.standard

import scala.collection.mutable

import org.scalajs.ir
import ir.Trees._
import ir.Position
import ir.ClassKind
import ir.Definitions

/** A ClassDef after linking.
 *
 *  Note that the [[version]] in the LinkedClass does not cover
 *  [[staticMethods]], [[memberMethods]] and [[exportedMembers]] as they have
 *  their individual versions. (The collections themselves are not versioned).
 *
 *  Moreover, the [[version]] is relative to the identity of a LinkedClass.
 *  The definition of identity varies as linked classes progress through the
 *  linking pipeline, but it only gets stronger, i.e., if two linked classes
 *  are id-different at phase P, then they must also be id-different at phase
 *  P+1. The converse is not true. This guarantees that versions can be used
 *  reliably to determine at phase P+1 whether a linked class coming from phase
 *  P must be reprocessed.
 */
final class LinkedClass(
    // Stuff from Tree
    val name: Ident,
    val kind: ClassKind,
    val jsClassCaptures: Option[List[ParamDef]],
    val superClass: Option[Ident],
    val interfaces: List[Ident],
    val jsSuperClass: Option[Tree],
    val jsNativeLoadSpec: Option[JSNativeLoadSpec],
    val fields: List[FieldDef],
    val staticMethods: List[Versioned[MethodDef]],
    val memberMethods: List[Versioned[MethodDef]],
    val exportedMembers: List[Versioned[MemberDef]],
    val topLevelExports: List[Versioned[TopLevelExportDef]],
    val optimizerHints: OptimizerHints,
    val pos: Position,

    // Actual Linking info
    val ancestors: List[String],
    val hasInstances: Boolean,
    val hasInstanceTests: Boolean,
    val hasRuntimeTypeInfo: Boolean,
    val version: Option[String]) {

  def encodedName: String = name.name

  val hasEntryPoint: Boolean = {
    topLevelExports.nonEmpty ||
    staticMethods.exists { m =>
      m.value.encodedName == Definitions.StaticInitializerName
    }
  }

  def fullName: String = Definitions.decodeClassName(encodedName)

  def copy(
      name: Ident = this.name,
      kind: ClassKind = this.kind,
      jsClassCaptures: Option[List[ParamDef]] = this.jsClassCaptures,
      superClass: Option[Ident] = this.superClass,
      interfaces: List[Ident] = this.interfaces,
      jsSuperClass: Option[Tree] = this.jsSuperClass,
      jsNativeLoadSpec: Option[JSNativeLoadSpec] = this.jsNativeLoadSpec,
      fields: List[FieldDef] = this.fields,
      staticMethods: List[Versioned[MethodDef]] = this.staticMethods,
      memberMethods: List[Versioned[MethodDef]] = this.memberMethods,
      exportedMembers: List[Versioned[MemberDef]] = this.exportedMembers,
      topLevelExports: List[Versioned[TopLevelExportDef]] = this.topLevelExports,
      optimizerHints: OptimizerHints = this.optimizerHints,
      pos: Position = this.pos,
      ancestors: List[String] = this.ancestors,
      hasInstances: Boolean = this.hasInstances,
      hasInstanceTests: Boolean = this.hasInstanceTests,
      hasRuntimeTypeInfo: Boolean = this.hasRuntimeTypeInfo,
      version: Option[String] = this.version): LinkedClass = {
    new LinkedClass(
        name,
        kind,
        jsClassCaptures,
        superClass,
        interfaces,
        jsSuperClass,
        jsNativeLoadSpec,
        fields,
        staticMethods,
        memberMethods,
        exportedMembers,
        topLevelExports,
        optimizerHints,
        pos,
        ancestors,
        hasInstances,
        hasInstanceTests,
        hasRuntimeTypeInfo,
        version)
  }
}
