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

package org.scalajs.linker.frontend

import org.scalajs.ir.{ClassKind, Position, SHA1, UTF8String, Version}
import org.scalajs.ir.Names._
import org.scalajs.ir.OriginalName.NoOriginalName
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._
import org.scalajs.ir.WellKnownNames._

import org.scalajs.linker.analyzer.Infos._

private[linker] object LambdaSynthesizer {
  /* Everything we create has a constant version because the names are derived
   * from the descriptors themselves.
   */
  val constantVersion = Version.fromByte(0)

  private val ClosureTypeRefName = LabelName("c")
  private val fFieldSimpleName = SimpleFieldName("f")

  /** Deterministically makes a class name for the lambda class given its descriptor.
   *
   *  This computation is mildly expensive. Callers should cache it if possible.
   */
  def makeClassName(descriptor: NewLambda.Descriptor): ClassName = {
    // Choose a base class name that will "makes sense" for debugging purposes
    val baseClassName = {
      if (descriptor.superClass == ObjectClass && descriptor.interfaces.nonEmpty)
        descriptor.interfaces.head
      else
        descriptor.superClass
    }

    val digestBuilder = new SHA1.DigestBuilder()
    digestBuilder.updateUTF8String(descriptor.superClass.encoded)
    for (intf <- descriptor.interfaces)
      digestBuilder.updateUTF8String(intf.encoded)

    // FIXME This is not efficient
    digestBuilder.updateUTF8String(UTF8String(descriptor.methodName.nameString))

    // No need the hash the paramTypes and resultType because they derive from the method name

    val digest = digestBuilder.finalizeDigest()

    /* The "$$Lambda" segment is meant to match the way LambdaMetaFactory
     * names generated classes. This is mostly for test compatibility
     * purposes (like partests that test the class name to tell whether a
     * lambda was indeed encoded as an LMF).
     */
    val suffixBuilder = new java.lang.StringBuilder(".$$Lambda$")
    for (b <- digest) {
      val i = b & 0xff
      suffixBuilder.append(Character.forDigit(i >> 4, 16)).append(Character.forDigit(i & 0x0f, 16))
    }

    ClassName(baseClassName.encoded ++ UTF8String(suffixBuilder.toString()))
  }

  /** Computes the constructor name for the lambda class of a descriptor. */
  def makeConstructorName(descriptor: NewLambda.Descriptor): MethodName = {
    val closureTypeNonNull =
      ClosureType(descriptor.paramTypes, descriptor.resultType, nullable = false)
    MethodName.constructor(TransientTypeRef(ClosureTypeRefName)(closureTypeNonNull) :: Nil)
  }

  /** Computes the `ClassInfo` of a lambda class, for use by the `Analyzer`.
   *
   *  The `className` must be the result of `makeClassName(descriptor)`.
   */
  def makeClassInfo(descriptor: NewLambda.Descriptor, className: ClassName): ClassInfo = {
    val methodInfos = Array.fill(MemberNamespace.Count)(Map.empty[MethodName, MethodInfo])

    val fFieldName = FieldName(className, fFieldSimpleName)
    val ctorName = makeConstructorName(descriptor)

    val ctorInfo: MethodInfo = {
      val b = new ReachabilityInfoBuilder(constantVersion)
      b.addFieldWritten(fFieldName)
      b.addMethodCalledStatically(descriptor.superClass,
          NamespacedMethodName(MemberNamespace.Constructor, NoArgConstructorName))
      MethodInfo(isAbstract = false, b.result())
    }
    methodInfos(MemberNamespace.Constructor.ordinal) =
      Map(ctorName -> ctorInfo)

    val implMethodInfo: MethodInfo = {
      val b = new ReachabilityInfoBuilder(constantVersion)
      b.addFieldRead(fFieldName)
      MethodInfo(isAbstract = false, b.result())
    }
    methodInfos(MemberNamespace.Public.ordinal) =
      Map(descriptor.methodName -> implMethodInfo)

    new ClassInfo(className, ClassKind.Class, Some(SyntheticClassKind.Lambda(descriptor)),
        nonExistent = false, Some(descriptor.superClass), descriptor.interfaces,
        jsNativeLoadSpec = None, referencedFieldClasses = Map.empty, methodInfos,
        jsNativeMembers = Map.empty, jsMethodProps = Nil, topLevelExports = Nil)
  }

  /** Synthesizes the `ClassDef` for a lambda class, for use by the `BaseLinker`.
   *
   *  The `className` must be the result of `makeClassName(descriptor)`.
   */
  def makeClassDef(descriptor: NewLambda.Descriptor, className: ClassName): ClassDef = {
    implicit val pos = Position.NoPosition

    import descriptor._

    val closureType = ClosureType(paramTypes, resultType, nullable = true)

    val thiz = This()(ClassType(className, nullable = false, exact = false))

    val fFieldIdent = FieldIdent(FieldName(className, fFieldSimpleName))
    val fFieldDef = FieldDef(MemberFlags.empty, fFieldIdent, NoOriginalName, closureType)
    val fFieldSelect = Select(thiz, fFieldIdent)(closureType)

    val ctorParamDef = ParamDef(LocalIdent(LocalName("f")), NoOriginalName,
        closureType.toNonNullable, mutable = false)
    val ctorDef = MethodDef(
      MemberFlags.empty.withNamespace(MemberNamespace.Constructor),
      MethodIdent(makeConstructorName(descriptor)),
      NoOriginalName,
      ctorParamDef :: Nil,
      VoidType,
      Some(
        Block(
          Assign(fFieldSelect, ctorParamDef.ref),
          ApplyStatically(ApplyFlags.empty.withConstructor(true), thiz,
              superClass, MethodIdent(NoArgConstructorName), Nil)(VoidType)
        )
      )
    )(OptimizerHints.empty, constantVersion)

    val methodParamDefs = paramTypes.zipWithIndex.map { case (paramType, index) =>
      ParamDef(LocalIdent(LocalName("x" + index)), NoOriginalName, paramType, mutable = false)
    }
    val methodDef = MethodDef(
      MemberFlags.empty,
      MethodIdent(methodName),
      NoOriginalName,
      methodParamDefs,
      resultType,
      Some(
        ApplyTypedClosure(ApplyFlags.empty, fFieldSelect, methodParamDefs.map(_.ref))
      )
    )(OptimizerHints.empty, constantVersion)

    ClassDef(
      ClassIdent(className),
      NoOriginalName,
      ClassKind.Class,
      jsClassCaptures = None,
      superClass = Some(ClassIdent(superClass)),
      interfaces = interfaces.map(ClassIdent(_)),
      jsSuperClass = None,
      jsNativeLoadSpec = None,
      fields = List(fFieldDef),
      methods = List(ctorDef, methodDef),
      jsConstructor = None,
      jsMethodProps = Nil,
      jsNativeMembers = Nil,
      topLevelExportDefs = Nil
    )(OptimizerHints.empty.withInline(true))
  }
}
