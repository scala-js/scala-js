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

import org.scalajs.ir.Names._

import org.scalajs.linker.interface.ModuleInitializer
import org.scalajs.linker.interface.unstable.ModuleInitializerImpl

sealed trait SymbolRequirement {
  final def ++(that: SymbolRequirement): SymbolRequirement =
    SymbolRequirement.multipleInternal(List(this, that))
}

object SymbolRequirement {
  import Nodes._

  def factory(originatingComponent: String): Factory =
    new Factory(originatingComponent)

  final class Factory private[SymbolRequirement] (origin: String) {
    def accessModule(moduleName: ClassName): SymbolRequirement =
      AccessModule(origin, moduleName)

    def callOnModule(moduleName: ClassName,
        methodName: MethodName): SymbolRequirement = {
      multiple(accessModule(moduleName), callMethod(moduleName, methodName))
    }

    def callOnModule(moduleName: ClassName,
        methodName: List[MethodName]): SymbolRequirement = {
      val methodCalls = methodName.map(callMethod(moduleName, _))
      multipleInternal(accessModule(moduleName) :: methodCalls)
    }

    def instantiateClass(className: ClassName,
        constructor: MethodName): SymbolRequirement = {
      InstantiateClass(origin, className, constructor)
    }

    def instantiateClass(className: ClassName,
        constructors: List[MethodName]): SymbolRequirement = {
      multipleInternal(constructors.map(instantiateClass(className, _)))
    }

    def instanceTests(className: ClassName): SymbolRequirement =
      InstanceTests(origin, className)

    def classData(className: ClassName): SymbolRequirement =
      ClassData(origin, className)

    def callMethod(className: ClassName,
        methodName: MethodName): SymbolRequirement = {
      CallMethod(origin, className, methodName, statically = false)
    }

    def callMethods(className: ClassName,
        methodNames: List[MethodName]): SymbolRequirement = {
      multipleInternal(methodNames.map(callMethod(className, _)))
    }

    def callMethodStatically(className: ClassName,
        methodName: MethodName): SymbolRequirement = {
      CallMethod(origin, className, methodName, statically = true)
    }

    def callStaticMethod(className: ClassName,
        methodName: MethodName): SymbolRequirement = {
      CallStaticMethod(origin, className, methodName)
    }

    def callStaticMethods(className: ClassName,
        methodNames: List[MethodName]): SymbolRequirement = {
      multipleInternal(methodNames.map(callStaticMethod(className, _)))
    }

    @deprecated("broken (not actually optional), do not use", "1.13.2")
    def optional(requirement: SymbolRequirement): SymbolRequirement = requirement

    def multiple(requirements: SymbolRequirement*): SymbolRequirement =
      multipleInternal(requirements.toList)

    def none(): SymbolRequirement = NoRequirement
  }

  private def multipleInternal(requirements: List[SymbolRequirement]) = {
    val flattened = requirements.flatMap {
      case NoRequirement          => Nil
      case Multiple(requirements) => requirements
      case requirement            => requirement :: Nil
    }

    flattened match {
      case Nil      => NoRequirement
      case x :: Nil => x
      case xs       => Multiple(xs)
    }
  }

  private[linker] object Nodes {
    final case class AccessModule(origin: String, moduleName: ClassName) extends SymbolRequirement
    final case class InstantiateClass(origin: String, className: ClassName,
        constructor: MethodName)
        extends SymbolRequirement
    final case class InstanceTests(origin: String, className: ClassName) extends SymbolRequirement
    final case class ClassData(origin: String, className: ClassName) extends SymbolRequirement
    final case class CallMethod(origin: String, className: ClassName,
        methodName: MethodName, statically: Boolean)
        extends SymbolRequirement
    final case class CallStaticMethod(origin: String, className: ClassName,
        methodName: MethodName)
        extends SymbolRequirement
    final case class Multiple(requirements: List[SymbolRequirement]) extends SymbolRequirement
    case object NoRequirement extends SymbolRequirement
  }
}
