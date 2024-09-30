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

import scala.annotation.tailrec

import scala.concurrent._

import org.scalajs.linker.analyzer._

import org.scalajs.ir
import org.scalajs.ir.Names._
import org.scalajs.ir.OriginalName.NoOriginalName
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._

import Analysis._

private[frontend] final class MethodSynthesizer(
    inputProvider: MethodSynthesizer.InputProvider) {

  def synthesizeLambdaClass(className: ClassName,
      descriptor: NewLambda.Descriptor, analysis: Analysis): ClassDef = {
    implicit val pos = ir.Position.NoPosition

    import descriptor._

    val constantVersion = ir.Version.fromByte(0)

    val fFieldName = FieldName(className, SimpleFieldName("f"))
    val closureTypeRef =
      ClosureTypeRef(descriptor.method.paramTypeRefs, descriptor.method.resultTypeRef)
    val ctorName = MethodName.constructor(closureTypeRef :: Nil)

    val thisType = ClassType(className, nullable = false)
    val paramTypes = closureTypeRef.paramTypeRefs.map(inferTypeFromTypeRef(analysis, _))
    val resultType = inferTypeFromTypeRef(analysis, closureTypeRef.resultTypeRef)
    val closureType = ClosureType(paramTypes, resultType, nullable = true)

    val thiz = This()(thisType)

    val fFieldDef = FieldDef(MemberFlags.empty, FieldIdent(fFieldName), NoOriginalName, closureType)

    val methodParamDefs = paramTypes.zipWithIndex.map { case (paramType, index) =>
      ParamDef(LocalIdent(LocalName("x" + index)), NoOriginalName, paramType, mutable = false)
    }

    val ctorParamDef = ParamDef(LocalIdent(LocalName("f")), NoOriginalName, closureType, mutable = false)

    val ctorDef = MethodDef(
      MemberFlags.empty.withNamespace(MemberNamespace.Constructor),
      MethodIdent(ctorName),
      NoOriginalName,
      ctorParamDef :: Nil,
      NoType,
      Some(
        Block(
          Assign(Select(thiz, FieldIdent(fFieldName))(closureType), ctorParamDef.ref),
          ApplyStatically(ApplyFlags.empty.withConstructor(true), thiz,
              superClass, MethodIdent(NoArgConstructorName), Nil)(NoType)
        )
      )
    )(OptimizerHints.empty, constantVersion)

    val methodDef = MethodDef(
      MemberFlags.empty,
      MethodIdent(method),
      NoOriginalName,
      methodParamDefs,
      resultType,
      Some(
        ApplyTypedClosure(
          ApplyFlags.empty,
          Select(thiz, FieldIdent(fFieldName))(closureType),
          methodParamDefs.map(_.ref)
        )
      )
    )(OptimizerHints.empty, constantVersion)

    ClassDef(
      ClassIdent(className),
      NoOriginalName,
      ir.ClassKind.Class,
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

  private def inferTypeFromTypeRef(analysis: Analysis, typeRef: TypeRef): Type = {
    typeRef match {
      case PrimRef(tpe) =>
        tpe
      case ClassRef(cls) =>
        if (cls == ObjectClass || analysis.classInfos(cls).kind.isJSType) AnyType
        else ClassType(cls, nullable = true)
      case typeRef: ArrayTypeRef =>
        ArrayType(typeRef, nullable = true)
      case typeRef: ClosureTypeRef =>
        ???
    }
  }

  def synthesizeMembers(classInfo: ClassInfo, analysis: Analysis)(
      implicit ec: ExecutionContext): Future[List[MethodDef]] = {
    val publicMethodInfos = classInfo.methodInfos(MemberNamespace.Public)
    val futures = publicMethodInfos.valuesIterator.filter(_.isReachable).flatMap { m =>
      m.syntheticKind match {
        case MethodSyntheticKind.None =>
          Nil

        case MethodSyntheticKind.ReflectiveProxy(targetName) =>
          List(synthesizeReflectiveProxy(classInfo, m, targetName, analysis))

        case MethodSyntheticKind.DefaultBridge(targetInterface) =>
          List(synthesizeDefaultBridge(classInfo, m, targetInterface, analysis))
      }
    }

    /* Sort for stability.
     * All synthetic members are in the Public namespace, so their `methodName`
     * uniquely identifies them.
     */
    Future.sequence(futures.toList)
      .map(_.sortBy(_.methodName))
  }

  private def synthesizeReflectiveProxy(
      classInfo: ClassInfo, methodInfo: MethodInfo,
      targetName: MethodName, analysis: Analysis)(
      implicit ec: ExecutionContext): Future[MethodDef] = {
    val methodName = methodInfo.methodName

    for {
      targetMDef <- findInheritedMethodDef(analysis, classInfo, targetName)
    } yield {
      implicit val pos = targetMDef.pos

      val targetIdent = targetMDef.name.copy() // for the new pos
      val proxyIdent = MethodIdent(methodName)
      val params = targetMDef.args.map(_.copy()) // for the new pos
      val instanceThisType = BoxedClassToPrimType.getOrElse(classInfo.className,
          ClassType(classInfo.className, nullable = false))

      val call = Apply(ApplyFlags.empty, This()(instanceThisType),
          targetIdent, params.map(_.ref))(targetMDef.resultType)

      val body = if (targetName.resultTypeRef == VoidRef) {
        // Materialize an `undefined` result for void methods
        Block(call, Undefined())
      } else {
        call
      }

      MethodDef(MemberFlags.empty, proxyIdent, targetMDef.originalName, params,
          AnyType, Some(body))(
          OptimizerHints.empty, targetMDef.version)
    }
  }

  private def synthesizeDefaultBridge(
      classInfo: ClassInfo, methodInfo: MethodInfo,
      targetInterface: ClassName, analysis: Analysis)(
      implicit ec: ExecutionContext): Future[MethodDef] = {
    val methodName = methodInfo.methodName

    val targetInterfaceInfo = analysis.classInfos(targetInterface)

    for {
      targetMDef <- findMethodDef(targetInterfaceInfo, methodName)
    } yield {
      implicit val pos = targetMDef.pos

      val targetIdent = targetMDef.name.copy() // for the new pos
      val bridgeIdent = targetIdent
      val params = targetMDef.args.map(_.copy()) // for the new pos
      val instanceThisType = BoxedClassToPrimType.getOrElse(classInfo.className,
          ClassType(classInfo.className, nullable = false))

      val body = ApplyStatically(
          ApplyFlags.empty, This()(instanceThisType), targetInterface,
          targetIdent, params.map(_.ref))(targetMDef.resultType)

      MethodDef(MemberFlags.empty, bridgeIdent, targetMDef.originalName,
          params, targetMDef.resultType, Some(body))(
          OptimizerHints.empty, targetMDef.version)
    }
  }

  private def findInheritedMethodDef(analysis: Analysis,
      classInfo: ClassInfo, methodName: MethodName,
      p: MethodInfo => Boolean = _ => true)(
      implicit ec: ExecutionContext): Future[MethodDef] = {
    @tailrec
    def loop(ancestorInfo: ClassInfo): Future[MethodDef] = {
      val inherited =
        ancestorInfo.methodInfos(MemberNamespace.Public).get(methodName)
      inherited.filter(p) match {
        case Some(m) =>
          m.syntheticKind match {
            case MethodSyntheticKind.None =>
              findMethodDef(ancestorInfo, methodName)

            case MethodSyntheticKind.DefaultBridge(targetInterface) =>
              val targetInterfaceInfo = analysis.classInfos(targetInterface)
              findMethodDef(targetInterfaceInfo, methodName)

            case MethodSyntheticKind.ReflectiveProxy(_) =>
              throw new AssertionError(
                  s"Cannot recursively follow $ancestorInfo.$methodName of " +
                  s"kind ${m.syntheticKind}")
          }

        case None =>
          assert(ancestorInfo.superClass.isDefined,
              s"Could not find $methodName anywhere in ${classInfo.className}")
          loop(ancestorInfo.superClass.get)
      }
    }

    loop(classInfo)
  }

  private def findMethodDef(classInfo: ClassInfo, methodName: MethodName)(
      implicit ec: ExecutionContext): Future[MethodDef] = {
    for {
      classDef <- inputProvider.loadClassDef(classInfo.className)
    } yield {
      classDef.methods.find { mDef =>
        mDef.flags.namespace == MemberNamespace.Public && mDef.methodName == methodName
      }.getOrElse {
        throw new AssertionError(
            s"Cannot find ${methodName.nameString} in ${classInfo.className.nameString}")
      }
    }
  }
}

private[frontend] object MethodSynthesizer {
  trait InputProvider {
    def loadClassDef(className: ClassName)(
        implicit ec: ExecutionContext): Future[ClassDef]
  }
}
