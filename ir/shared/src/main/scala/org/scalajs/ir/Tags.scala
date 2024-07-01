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

/** Serialization and hashing tags for trees and types */
private[ir] object Tags {

  // Tags for Trees

  /** Use to denote optional trees. */
  final val TagEmptyTree = 1

  final val TagJSSpread = TagEmptyTree + 1

  final val TagVarDef = TagJSSpread + 1

  final val TagSkip = TagVarDef + 1
  final val TagBlock = TagSkip + 1
  final val TagLabeled = TagBlock + 1
  final val TagAssign = TagLabeled + 1
  final val TagReturn = TagAssign + 1
  final val TagIf = TagReturn + 1
  final val TagWhile = TagIf + 1
  final val TagDoWhile = TagWhile + 1 // removed in 1.13
  final val TagForIn = TagDoWhile + 1
  final val TagTryCatch = TagForIn + 1
  final val TagTryFinally = TagTryCatch + 1
  final val TagThrow = TagTryFinally + 1
  final val TagMatch = TagThrow + 1
  final val TagDebugger = TagMatch + 1

  final val TagNew = TagDebugger + 1
  final val TagLoadModule = TagNew + 1
  final val TagStoreModule = TagLoadModule + 1
  final val TagSelect = TagStoreModule + 1
  final val TagSelectStatic = TagSelect + 1
  final val TagApply = TagSelectStatic + 1
  final val TagApplyStatically = TagApply + 1
  final val TagApplyStatic = TagApplyStatically + 1
  final val TagUnaryOp = TagApplyStatic + 1
  final val TagBinaryOp = TagUnaryOp + 1
  final val TagNewArray = TagBinaryOp + 1
  final val TagArrayValue = TagNewArray + 1
  final val TagArrayLength = TagArrayValue + 1
  final val TagArraySelect = TagArrayLength + 1
  final val TagRecordValue = TagArraySelect + 1
  final val TagRecordSelect = TagRecordValue + 1
  final val TagIsInstanceOf = TagRecordSelect + 1
  final val TagAsInstanceOf = TagIsInstanceOf + 1
  final val TagGetClass = TagAsInstanceOf + 1

  final val TagJSNew = TagGetClass + 1
  final val TagJSPrivateSelect = TagJSNew + 1
  final val TagJSSelect = TagJSPrivateSelect + 1
  final val TagJSFunctionApply = TagJSSelect + 1
  final val TagJSMethodApply = TagJSFunctionApply + 1
  final val TagJSSuperSelect = TagJSMethodApply + 1
  final val TagJSSuperMethodCall = TagJSSuperSelect + 1
  final val TagJSSuperConstructorCall = TagJSSuperMethodCall + 1
  final val TagJSImportCall = TagJSSuperConstructorCall + 1
  final val TagLoadJSConstructor = TagJSImportCall + 1
  final val TagLoadJSModule = TagLoadJSConstructor + 1
  final val TagJSDelete = TagLoadJSModule + 1
  final val TagJSUnaryOp = TagJSDelete + 1
  final val TagJSBinaryOp = TagJSUnaryOp + 1
  final val TagJSArrayConstr = TagJSBinaryOp + 1
  final val TagJSObjectConstr = TagJSArrayConstr + 1
  final val TagJSGlobalRef = TagJSObjectConstr + 1
  final val TagJSTypeOfGlobalRef = TagJSGlobalRef + 1
  final val TagJSLinkingInfo = TagJSTypeOfGlobalRef + 1

  final val TagUndefined = TagJSLinkingInfo + 1
  final val TagNull = TagUndefined + 1
  final val TagBooleanLiteral = TagNull + 1
  final val TagCharLiteral = TagBooleanLiteral + 1
  final val TagByteLiteral = TagCharLiteral + 1
  final val TagShortLiteral = TagByteLiteral + 1
  final val TagIntLiteral = TagShortLiteral + 1
  final val TagLongLiteral = TagIntLiteral + 1
  final val TagFloatLiteral = TagLongLiteral + 1
  final val TagDoubleLiteral = TagFloatLiteral + 1
  final val TagStringLiteral = TagDoubleLiteral + 1
  final val TagClassOf = TagStringLiteral + 1

  final val TagVarRef = TagClassOf + 1
  final val TagThis = TagVarRef + 1
  final val TagClosure = TagThis + 1
  final val TagCreateJSClass = TagClosure + 1

  /* Note that there is no TagTransient, since transient nodes are never
   * serialized nor hashed.
   */

  // New in 1.1

  final val TagIdentityHashCode = TagCreateJSClass + 1
  final val TagSelectJSNativeMember = TagIdentityHashCode + 1

  // New in 1.4

  final val TagApplyDynamicImport = TagSelectJSNativeMember + 1

  // New in 1.5

  final val TagClone = TagApplyDynamicImport + 1

  // New in 1.6

  final val TagJSImportMeta = TagClone + 1

  // New in 1.8

  final val TagJSNewTarget = TagJSImportMeta + 1

  // New in 1.11

  final val TagWrapAsThrowable = TagJSNewTarget + 1
  final val TagUnwrapFromThrowable = TagWrapAsThrowable + 1

  // New in 1.17

  final val TagLinkTimeIf = TagUnwrapFromThrowable + 1

  // Tags for member defs

  final val TagFieldDef = 1
  final val TagJSFieldDef = TagFieldDef + 1
  final val TagMethodDef = TagJSFieldDef + 1
  final val TagJSMethodDef = TagMethodDef + 1
  final val TagJSPropertyDef = TagJSMethodDef + 1

  // New in 1.1

  final val TagJSNativeMemberDef = TagJSPropertyDef + 1

  // New in 1.11

  final val TagJSConstructorDef = TagJSNativeMemberDef + 1

  // Tags for top-level export defs

  final val TagTopLevelJSClassExportDef = 1
  final val TagTopLevelModuleExportDef = TagTopLevelJSClassExportDef + 1
  final val TagTopLevelMethodExportDef = TagTopLevelModuleExportDef + 1
  final val TagTopLevelFieldExportDef = TagTopLevelMethodExportDef + 1

  // Tags for Types

  final val TagAnyType = 1
  final val TagNothingType = TagAnyType + 1
  final val TagUndefType = TagNothingType + 1
  final val TagBooleanType = TagUndefType + 1
  final val TagCharType = TagBooleanType + 1
  final val TagByteType = TagCharType + 1
  final val TagShortType = TagByteType + 1
  final val TagIntType = TagShortType + 1
  final val TagLongType = TagIntType + 1
  final val TagFloatType = TagLongType + 1
  final val TagDoubleType = TagFloatType + 1
  final val TagStringType = TagDoubleType + 1
  final val TagNullType = TagStringType + 1
  final val TagClassType = TagNullType + 1
  final val TagArrayType = TagClassType + 1
  final val TagRecordType = TagArrayType + 1
  final val TagNoType = TagRecordType + 1

  // Tags for TypeRefs

  final val TagVoidRef = 1
  final val TagBooleanRef = TagVoidRef + 1
  final val TagCharRef = TagBooleanRef + 1
  final val TagByteRef = TagCharRef + 1
  final val TagShortRef = TagByteRef + 1
  final val TagIntRef = TagShortRef + 1
  final val TagLongRef = TagIntRef + 1
  final val TagFloatRef = TagLongRef + 1
  final val TagDoubleRef = TagFloatRef + 1
  final val TagNullRef = TagDoubleRef + 1
  final val TagNothingRef = TagNullRef + 1
  final val TagClassRef = TagNothingRef + 1
  final val TagArrayTypeRef = TagClassRef + 1

  // Tags for JS native loading specs

  final val TagJSNativeLoadSpecNone = 0
  final val TagJSNativeLoadSpecGlobal = TagJSNativeLoadSpecNone + 1
  final val TagJSNativeLoadSpecImport = TagJSNativeLoadSpecGlobal + 1
  final val TagJSNativeLoadSpecImportWithGlobalFallback = TagJSNativeLoadSpecImport + 1

  // Tags for LinkTimeTree

  final val TagLinkTimeProperty = 0
  final val TagLinkTimeBooleanConst = TagLinkTimeProperty + 1
  final val TagLinkTimeIntConst = TagLinkTimeBooleanConst + 1
  final val TagLinkTimeTreeBinary = TagLinkTimeIntConst + 1

}
