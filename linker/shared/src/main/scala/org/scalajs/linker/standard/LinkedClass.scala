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

package org.scalajs.linker.standard

import org.scalajs.ir.Trees._
import org.scalajs.ir.{ClassKind, Position}
import org.scalajs.ir.Names.ClassName

/** A ClassDef after linking.
 *
 *  Note that the [[version]] in the LinkedClass does not cover [[methods]] nor
 *  [[exportedMembers]] as they have their individual versions. (The
 *  collections themselves are not versioned).
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
    val name: ClassIdent,
    val kind: ClassKind,
    val jsClassCaptures: Option[List[ParamDef]],
    val superClass: Option[ClassIdent],
    val interfaces: List[ClassIdent],
    val jsSuperClass: Option[Tree],
    val jsNativeLoadSpec: Option[JSNativeLoadSpec],
    val fields: List[AnyFieldDef],
    val methods: List[Versioned[MethodDef]],
    val exportedMembers: List[Versioned[JSMethodPropDef]],
    val jsNativeMembers: List[JSNativeMemberDef],
    val optimizerHints: OptimizerHints,
    val pos: Position,

    // Actual Linking info
    val ancestors: List[ClassName],
    val hasInstances: Boolean,
    val hasInstanceTests: Boolean,
    val hasRuntimeTypeInfo: Boolean,

    val staticDependencies: Set[ClassName],
    val externalDependencies: Set[String],

    val version: Option[String]) {

  def className: ClassName = name.name

  val hasStaticInitializer: Boolean = {
    methods.exists { m =>
      val methodDef = m.value
      methodDef.flags.namespace == MemberNamespace.StaticConstructor &&
      methodDef.methodName.isStaticInitializer
    }
  }

  def hasAnyDefinitions: Boolean = {
    fields.nonEmpty ||
    methods.nonEmpty ||
    exportedMembers.nonEmpty ||
    hasInstanceTests ||
    hasRuntimeTypeInfo
  }

  def fullName: String = className.nameString

  private[linker] def refined(
      kind: ClassKind,
      fields: List[AnyFieldDef],
      methods: List[Versioned[MethodDef]],
      jsNativeMembers: List[JSNativeMemberDef],
      hasInstances: Boolean,
      hasInstanceTests: Boolean,
      hasRuntimeTypeInfo: Boolean,
      staticDependencies: Set[ClassName]
  ): LinkedClass = {
    copy(
        kind = kind,
        fields = fields,
        methods = methods,
        jsNativeMembers = jsNativeMembers,
        hasInstances = hasInstances,
        hasInstanceTests = hasInstanceTests,
        hasRuntimeTypeInfo = hasRuntimeTypeInfo,
        staticDependencies = staticDependencies
    )
  }

  private[linker] def optimized(
      methods: List[Versioned[MethodDef]]
  ): LinkedClass = {
    copy(methods = methods)
  }

  private def copy(
      name: ClassIdent = this.name,
      kind: ClassKind = this.kind,
      jsClassCaptures: Option[List[ParamDef]] = this.jsClassCaptures,
      superClass: Option[ClassIdent] = this.superClass,
      interfaces: List[ClassIdent] = this.interfaces,
      jsSuperClass: Option[Tree] = this.jsSuperClass,
      jsNativeLoadSpec: Option[JSNativeLoadSpec] = this.jsNativeLoadSpec,
      fields: List[AnyFieldDef] = this.fields,
      methods: List[Versioned[MethodDef]] = this.methods,
      exportedMembers: List[Versioned[JSMethodPropDef]] = this.exportedMembers,
      jsNativeMembers: List[JSNativeMemberDef] = this.jsNativeMembers,
      optimizerHints: OptimizerHints = this.optimizerHints,
      pos: Position = this.pos,
      ancestors: List[ClassName] = this.ancestors,
      hasInstances: Boolean = this.hasInstances,
      hasInstanceTests: Boolean = this.hasInstanceTests,
      hasRuntimeTypeInfo: Boolean = this.hasRuntimeTypeInfo,
      staticDependencies: Set[ClassName] = this.staticDependencies,
      externalDependencies: Set[String] = this.externalDependencies,
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
        methods,
        exportedMembers,
        jsNativeMembers,
        optimizerHints,
        pos,
        ancestors,
        hasInstances,
        hasInstanceTests,
        hasRuntimeTypeInfo,
        staticDependencies,
        externalDependencies,
        version)
  }
}
