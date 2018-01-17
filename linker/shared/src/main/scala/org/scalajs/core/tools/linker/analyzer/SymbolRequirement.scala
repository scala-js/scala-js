/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.linker.analyzer

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
        methodName: Traversable[String]): SymbolRequirement = {
      val methodCalls = methodName.map(callMethod(moduleName, _)).toList
      multipleInternal(accessModule(moduleName) :: methodCalls)
    }

    def instantiateClass(className: String,
        constructor: String): SymbolRequirement = {
      InstantiateClass(origin, className, constructor)
    }

    def instantiateClass(className: String,
        constructors: Traversable[String]): SymbolRequirement = {
      multipleInternal(constructors.toList.map(instantiateClass(className, _)))
    }

    def instanceTests(className: String): SymbolRequirement =
      InstanceTests(origin, className)

    def classData(className: String): SymbolRequirement =
      ClassData(origin, className)

    def callMethod(className: String, methodName: String): SymbolRequirement =
      CallMethod(origin, className, methodName, statically = false)

    def callMethods(className: String,
        methodNames: Traversable[String]): SymbolRequirement = {
      multipleInternal(methodNames.toList.map(callMethod(className, _)))
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

  private[analyzer] object Nodes {
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
}
