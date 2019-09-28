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

import org.scalajs.linker.ModuleInitializer

sealed trait SymbolRequirement {
  final def ++(that: SymbolRequirement): SymbolRequirement =
    SymbolRequirement.multipleInternal(List(this, that))
}

object SymbolRequirement {
  import Nodes._

  def factory(originatingComponent: String): Factory =
    new Factory(originatingComponent)

  final class Factory private[SymbolRequirement] (origin: String) {
    def accessModule(moduleName: String): SymbolRequirement =
      AccessModule(origin, moduleName)

    def callOnModule(moduleName: String, methodName: String): SymbolRequirement =
      multiple(accessModule(moduleName), callMethod(moduleName, methodName))

    def callOnModule(moduleName: String,
        methodName: List[String]): SymbolRequirement = {
      val methodCalls = methodName.map(callMethod(moduleName, _))
      multipleInternal(accessModule(moduleName) :: methodCalls)
    }

    def instantiateClass(className: String,
        constructor: String): SymbolRequirement = {
      InstantiateClass(origin, className, constructor)
    }

    def instantiateClass(className: String,
        constructors: List[String]): SymbolRequirement = {
      multipleInternal(constructors.map(instantiateClass(className, _)))
    }

    def instanceTests(className: String): SymbolRequirement =
      InstanceTests(origin, className)

    def classData(className: String): SymbolRequirement =
      ClassData(origin, className)

    def callMethod(className: String, methodName: String): SymbolRequirement =
      CallMethod(origin, className, methodName, statically = false)

    def callMethods(className: String,
        methodNames: List[String]): SymbolRequirement = {
      multipleInternal(methodNames.map(callMethod(className, _)))
    }

    def callMethodStatically(className: String, methodName: String): SymbolRequirement =
      CallMethod(origin, className, methodName, statically = true)

    def callStaticMethod(className: String, methodName: String): SymbolRequirement =
      CallStaticMethod(origin, className, methodName)

    def optional(requirement: SymbolRequirement): SymbolRequirement = {
      requirement match {
        case NoRequirement      => NoRequirement
        case optional: Optional => optional
        case _                  => requirement
      }
    }

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
    case class AccessModule(origin: String, moduleName: String) extends SymbolRequirement
    case class InstantiateClass(origin: String, className: String,
        constructor: String) extends SymbolRequirement
    case class InstanceTests(origin: String, className: String) extends SymbolRequirement
    case class ClassData(origin: String, className: String) extends SymbolRequirement
    case class CallMethod(origin: String, className: String, methodName: String,
        statically: Boolean) extends SymbolRequirement
    case class CallStaticMethod(origin: String, className: String,
        methodName: String) extends SymbolRequirement
    case class Optional(requirement: SymbolRequirement) extends SymbolRequirement
    case class Multiple(requirements: List[SymbolRequirement]) extends SymbolRequirement
    case object NoRequirement extends SymbolRequirement
  }

  private[linker] def fromModuleInitializer(
      entryPoints: Seq[ModuleInitializer]): SymbolRequirement = {
    import ModuleInitializerImpl._

    val factory = SymbolRequirement.factory("module initializers")
    val requirements = for (entryPoint <- entryPoints) yield {
      ModuleInitializerImpl.fromModuleInitializer(entryPoint) match {
        case VoidMainMethod(moduleClassName, mainMethodName) =>
          factory.callOnModule(moduleClassName, mainMethodName)

        case MainMethodWithArgs(moduleClassName, mainMethodName, _) =>
          factory.callOnModule(moduleClassName, mainMethodName) ++
          factory.classData("T")
      }
    }
    factory.multiple(requirements: _*)
  }
}
