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
import org.scalajs.ir.{ClassKind, Position, Version}
import org.scalajs.ir.Names.{ClassName, FieldName}

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
    val methods: List[MethodDef],
    val jsConstructorDef: Option[JSConstructorDef],
    val exportedMembers: List[JSMethodPropDef],
    val jsNativeMembers: List[JSNativeMemberDef],
    val optimizerHints: OptimizerHints,
    val pos: Position,

    // Actual Linking info
    val ancestors: List[ClassName],
    val hasInstances: Boolean,
    val hasInstanceTests: Boolean,
    val hasRuntimeTypeInfo: Boolean,
    val fieldsRead: Set[FieldName],
    val staticFieldsRead: Set[FieldName],

    val staticDependencies: Set[ClassName],
    val externalDependencies: Set[String],
    val dynamicDependencies: Set[ClassName],

    val version: Version) {

  def className: ClassName = name.name

  val hasStaticInitializer: Boolean = {
    methods.exists { methodDef =>
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
}
