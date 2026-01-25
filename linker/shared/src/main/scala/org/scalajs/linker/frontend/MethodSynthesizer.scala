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
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._
import org.scalajs.ir.WellKnownNames._

import Analysis._

private[frontend] final class MethodSynthesizer(
    inputProvider: MethodSynthesizer.InputProvider) {

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
          ClassType(classInfo.className, nullable = false, exact = false))

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
          ClassType(classInfo.className, nullable = false, exact = false))

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
    val classDefFuture = classInfo.syntheticKind match {
      case None =>
        inputProvider.loadClassDef(classInfo.className)

      case Some(SyntheticClassKind.Lambda(descriptor)) =>
        /* We are *re*-generating the full ClassDef, in addition to the
         * generation done in `BaseLinker`.
         *
         * This happens at most once per lambda class (hopefully never for
         * most of them), because:
         *
         * - lambda classes are never interfaces, so we must be generating a
         *   reflective proxy, not a default bridge;
         * - lambda classes are never extended, so we don't get here through
         *   an inheritance chain, only when synthesizing methods in the same class;
         * - lambda classes have a single non-constructor method.
         */
        Future.successful(LambdaSynthesizer.makeClassDef(descriptor, classInfo.className))
    }

    for {
      classDef <- classDefFuture
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
