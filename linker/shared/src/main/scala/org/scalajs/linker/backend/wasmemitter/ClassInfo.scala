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

package org.scalajs.linker.backend.wasmemitter

import org.scalajs.ir.Names._
import org.scalajs.ir.Trees._
import org.scalajs.ir.ClassKind

final class ClassInfo(
    val name: ClassName,
    val kind: ClassKind,
    val jsClassCaptures: Option[List[ParamDef]],
    val allFieldDefs: List[FieldDef],
    val hasInstances: Boolean,
    val isAbstract: Boolean,
    val hasRuntimeTypeInfo: Boolean,
    val jsPrototypeHolder: Option[ClassName],
    val jsNativeLoadSpec: Option[JSNativeLoadSpec],
    val jsNativeMembers: Map[MethodName, JSNativeLoadSpec],
    val wasmImportedMembers: Map[MethodName, WasmImportedMethodDef],
    val staticFieldMirrors: Map[FieldName, List[String]],
    _specialInstanceTypes: Int, // should be `val` but there is a large Scaladoc for it below
    val resolvedMethodInfos: Map[MethodName, ClassInfo.ConcreteMethodInfo],
    val tableEntries: List[MethodName],
    _itableIdx: Int
) {
  override def toString(): String =
    s"ClassInfo(${name.nameString})"

  /** Returns the index of this interface's itable in the classes' vtables.
   *
   *  Only interfaces that have instances get an itable index.
   */
  def itableIdx: Int = {
    if (_itableIdx < 0) {
      val isInterface = kind == ClassKind.Interface
      if (isInterface && hasInstances) {
        // it should have received an itable idx
        throw new IllegalStateException(
            s"$this was not assigned an itable index although it needs one.")
      } else {
        throw new IllegalArgumentException(
            s"Trying to ask the itable idx for $this, which is not supposed to have one " +
            s"(isInterface = $isInterface; hasInstances = $hasInstances).")
      }
    }
    _itableIdx
  }

  /** A bitset of the `jsValueType`s corresponding to hijacked classes that extend this class.
   *
   *  This value is used for instance tests against this class. A JS value `x` is an instance of
   *  this type iff `jsValueType(x)` is a member of this bitset. Because of how a bitset works,
   *  this means testing the following formula:
   *
   *  {{{
   *  ((1 << jsValueType(x)) & specialInstanceTypes) != 0
   *  }}}
   *
   *  For example, if this class is `Comparable`, we want the bitset to contain the values for
   *  `boolean`, `string` and `number` (but not `undefined`), because `jl.Boolean`, `jl.String`
   *  and `jl.Double` implement `Comparable`.
   *
   *  This field is initialized with 0, and augmented during preprocessing by calls to
   *  `addSpecialInstanceType`.
   *
   *  This technique is used both for static `isInstanceOf` tests as well as reflective tests
   *  through `Class.isInstance`. For the latter, this value is stored in
   *  `typeData.specialInstanceTypes`. For the former, it is embedded as a constant in the
   *  generated code.
   *
   *  See the `isInstance` and `genInstanceTest` helpers.
   *
   *  Special cases: this value remains 0 for all the numeric hijacked classes except `jl.Double`,
   *  since `jsValueType(x) == JSValueTypeNumber` is not enough to deduce that
   *  `x.isInstanceOf[Int]`, for example.
   */
  val specialInstanceTypes: Int = _specialInstanceTypes

  /** Is this class an ancestor of any hijacked class?
   *
   *  This includes but is not limited to the hijacked classes themselves, as well as `jl.Object`.
   */
  def isAncestorOfHijackedClass: Boolean =
    specialInstanceTypes != 0 || kind == ClassKind.HijackedClass

  def isInterface: Boolean =
    kind == ClassKind.Interface
}

object ClassInfo {
  final class ConcreteMethodInfo(val ownerClass: ClassName, val methodName: MethodName) {
    val tableEntryID = VarGen.genFunctionID.forTableEntry(ownerClass, methodName)
  }
}
