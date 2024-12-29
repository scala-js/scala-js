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
import org.scalajs.ir.Names.{ClassName, FieldName, MethodName}

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
 *
 *  @param ancestors
 *    List of all the ancestor classes and interfaces of this class. It always
 *    contains this class name and `java.lang.Object`. This class name is
 *    always the first element of the list.
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
    val hasDirectInstances: Boolean,
    val hasInstanceTests: Boolean,
    val hasRuntimeTypeInfo: Boolean,
    val fieldsRead: Set[FieldName],
    val staticFieldsRead: Set[FieldName],

    val staticDependencies: Set[ClassName],
    val externalDependencies: Set[String],
    val dynamicDependencies: Set[ClassName],

    // Desugaring requirements
    val desugaringRequirements: LinkedClass.DesugaringRequirements,

    val version: Version) {

  require(ancestors.headOption.contains(name.name),
      s"ancestors for ${name.name.nameString} must start with itself: $ancestors")

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

object LinkedClass {
  /** Desugaring requirements of a `LinkedClass`.
   *
   *  These requirements are a set of members that need desugaring.
   */
  final class DesugaringRequirements private (
    methods: Vector[Set[MethodName]], // indexed by MemberNamespace ordinal
    val exportedMembers: Boolean
  ) {
    private def this() = {
      this(
        methods = Vector.fill(MemberNamespace.Count)(Set.empty),
        exportedMembers = false
      )
    }

    /** Are these requirements empty, i.e., does the corresponding require no desugaring at all? */
    def isEmpty: Boolean =
      this eq DesugaringRequirements.Empty // by construction, only that specific instance is empty

    /** Do the requirements contain the given method, i.e., does that method need desugaring? */
    def containsMethod(namespace: MemberNamespace, methodName: MethodName): Boolean =
      methods(namespace.ordinal).contains(methodName)

    def addMethod(namespace: MemberNamespace, methodName: MethodName): DesugaringRequirements = {
      val newMethods =
        methods.updated(namespace.ordinal, methods(namespace.ordinal) + methodName)
      new DesugaringRequirements(newMethods, exportedMembers)
    }

    def addAnyExportedMember(): DesugaringRequirements =
      new DesugaringRequirements(methods, exportedMembers = true)
  }

  object DesugaringRequirements {
    val Empty: DesugaringRequirements = new DesugaringRequirements()
  }
}
