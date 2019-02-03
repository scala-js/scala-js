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

import org.scalajs.linker.analyzer._

import org.scalajs.ir
import ir.Trees._
import ir.Types._

import Analysis._

private[frontend] final class MethodSynthesizer(
    inputProvider: MethodSynthesizer.InputProvider) {

  def synthesizeMembers(classInfo: ClassInfo, analysis: Analysis): Iterator[MethodDef] = {
    classInfo.methodInfos.valuesIterator.filter(_.isReachable).flatMap { m =>
      m.syntheticKind match {
        case MethodSyntheticKind.None =>
          Nil

        case MethodSyntheticKind.ReflectiveProxy(targetName) =>
          List(synthesizeReflectiveProxy(classInfo, m, targetName, analysis))

        case MethodSyntheticKind.DefaultBridge(targetInterface) =>
          List(synthesizeDefaultBridge(classInfo, m, targetInterface, analysis))
      }
    }
  }

  private def synthesizeReflectiveProxy(
      classInfo: ClassInfo, methodInfo: MethodInfo,
      targetName: String, analysis: Analysis): MethodDef = {
    val encodedName = methodInfo.encodedName

    val targetMDef = findInheritedMethodDef(analysis, classInfo, targetName)

    implicit val pos = targetMDef.pos

    val targetIdent = targetMDef.name.asInstanceOf[Ident].copy() // for the new pos
    val proxyIdent = Ident(encodedName, None)
    val params = targetMDef.args.map(_.copy()) // for the new pos
    val currentClassType = ClassType(classInfo.encodedName)

    val call = Apply(This()(currentClassType),
        targetIdent, params.map(_.ref))(targetMDef.resultType)

    val body = if (targetName.endsWith("__V")) {
      // Materialize an `undefined` result for void methods
      Block(call, Undefined())
    } else {
      call
    }

    MethodDef(static = false, proxyIdent, params, AnyType, Some(body))(
        OptimizerHints.empty, targetMDef.hash)
  }

  private def synthesizeDefaultBridge(
      classInfo: ClassInfo, methodInfo: MethodInfo,
      targetInterface: String, analysis: Analysis): MethodDef = {
    val encodedName = methodInfo.encodedName

    val targetInterfaceInfo = analysis.classInfos(targetInterface)
    val targetMDef = findMethodDef(targetInterfaceInfo, encodedName)

    implicit val pos = targetMDef.pos

    val targetIdent = targetMDef.name.asInstanceOf[Ident].copy() // for the new pos
    val bridgeIdent = targetIdent
    val params = targetMDef.args.map(_.copy()) // for the new pos
    val currentClassType = ClassType(classInfo.encodedName)

    val body = ApplyStatically(
        This()(currentClassType), ClassRef(targetInterface), targetIdent,
        params.map(_.ref))(targetMDef.resultType)

    MethodDef(static = false, bridgeIdent, params, targetMDef.resultType, Some(body))(
        OptimizerHints.empty, targetMDef.hash)
  }

  private def findInheritedMethodDef(analysis: Analysis,
      classInfo: ClassInfo, methodName: String,
      p: MethodInfo => Boolean = _ => true): MethodDef = {
    @tailrec
    def loop(ancestorInfo: ClassInfo): MethodDef = {
      val inherited = ancestorInfo.methodInfos.get(methodName)
      inherited.find(p) match {
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
              s"Could not find $methodName anywhere in ${classInfo.encodedName}")
          loop(ancestorInfo.superClass.get)
      }
    }

    loop(classInfo)
  }

  private def findMethodDef(classInfo: ClassInfo, methodName: String): MethodDef = {
    val classDef = inputProvider.loadClassDef(classInfo.encodedName)
    classDef.memberDefs.collectFirst {
      case mDef: MethodDef
          if !mDef.static && mDef.encodedName == methodName => mDef
    }.getOrElse {
      throw new AssertionError(
          s"Cannot find $methodName in ${classInfo.encodedName}")
    }
  }
}

private[frontend] object MethodSynthesizer {
  trait InputProvider {
    def loadClassDef(encodedName: String): ClassDef
  }
}
