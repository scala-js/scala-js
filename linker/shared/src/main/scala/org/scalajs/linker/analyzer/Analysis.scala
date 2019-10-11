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

package org.scalajs.linker.analyzer

import scala.annotation.tailrec

import scala.collection.mutable

import org.scalajs.logging._

import org.scalajs.ir
import org.scalajs.ir.ClassKind
import org.scalajs.ir.Definitions._
import org.scalajs.ir.Trees.MemberNamespace
import org.scalajs.ir.Types._

/** Reachability graph produced by the [[Analyzer]].
 *
 *  Warning: this trait is not meant to be extended by third-party libraries
 *  and applications. Methods and/or fields can be added in subsequent
 *  versions, possibly causing `LinkageError`s if you extend it.
 */
trait Analysis {
  import Analysis._

  def classInfos: scala.collection.Map[ClassName, ClassInfo]
  def errors: scala.collection.Seq[Error]
}

object Analysis {

  private val PrimRefDisplayNames: Map[PrimRef, String] = Map(
      VoidRef -> "void",
      BooleanRef -> "boolean",
      CharRef -> "char",
      ByteRef -> "byte",
      ShortRef -> "short",
      IntRef -> "int",
      LongRef -> "long",
      FloatRef -> "float",
      DoubleRef -> "double",
      NullRef -> "null",
      NothingRef -> "nothing"
  )

  /** Class node in a reachability graph produced by the [[Analyzer]].
   *
   *  Warning: this trait is not meant to be extended by third-party libraries
   *  and applications. Methods and/or fields can be added in subsequent
   *  versions, possibly causing `LinkageError`s if you extend it.
   */
  trait ClassInfo {
    def encodedName: ClassName
    def kind: ClassKind
    def superClass: Option[ClassInfo]
    def interfaces: scala.collection.Seq[ClassInfo]
    def ancestors: scala.collection.Seq[ClassInfo]
    def nonExistent: Boolean
    /** For a Scala class, it is instantiated with a `New`; for a JS class,
     *  its constructor is accessed with a `JSLoadConstructor` or because it
     *  is needed for a subclass.
     */
    def isInstantiated: Boolean
    def isAnySubclassInstantiated: Boolean
    def isModuleAccessed: Boolean
    def areInstanceTestsUsed: Boolean
    def isDataAccessed: Boolean
    def isAnyStaticFieldUsed: Boolean
    def isAnyPrivateJSFieldUsed: Boolean
    def linkedFrom: scala.collection.Seq[From]
    def instantiatedFrom: scala.collection.Seq[From]
    def methodInfos(
        namespace: MemberNamespace): scala.collection.Map[MethodName, MethodInfo]

    def displayName: String = decodeClassName(encodedName)
  }

  /** Method node in a reachability graph produced by the [[Analyzer]].
   *
   *  Warning: this trait is not meant to be extended by third-party libraries
   *  and applications. Methods and/or fields can be added in subsequent
   *  versions, possibly causing `LinkageError`s if you extend it.
   */
  trait MethodInfo {
    def owner: ClassInfo
    def encodedName: MethodName
    def namespace: MemberNamespace
    def isAbstract: Boolean
    def isReflProxy: Boolean
    def isReachable: Boolean
    def calledFrom: scala.collection.Seq[From]
    def instantiatedSubclasses: scala.collection.Seq[ClassInfo]
    def nonExistent: Boolean
    def syntheticKind: MethodSyntheticKind

    def displayName: String = {
      def typeRefDisplayName(tpe: TypeRef): String = tpe match {
        case primRef: PrimRef               => PrimRefDisplayNames(primRef)
        case ClassRef(encodedName)          => decodeClassName(encodedName)
        case ArrayTypeRef(base, dimensions) => "[" * dimensions + typeRefDisplayName(base)
      }

      val (simpleName, paramTypes, resultType) =
        ir.Definitions.decodeMethodName(encodedName)

      simpleName + "(" + paramTypes.map(typeRefDisplayName).mkString(",") + ")" +
      resultType.fold("")(typeRefDisplayName)
    }

    def fullDisplayName: String =
      this.namespace.prefixString + owner.displayName + "." + displayName
  }

  sealed trait MethodSyntheticKind

  object MethodSyntheticKind {
    /** Not a synthetic method. */
    final case object None extends MethodSyntheticKind

    /** A reflective proxy bridge to the appropriate target method.
     *
     *  A reflective proxy `method__xyz__` dynamically calls some `target`
     *  method `method__xyz__R` on `this`. `R` is boxed according to JVM boxing
     *  semantics, i.e.,
     *
     *  - `Char` is boxed in `java.lang.Character`
     *  - `void` is followed by a reified `()`, i.e., `undefined`
     *  - All other types are left as is
     *
     *  The basic shape is:
     *
     *  {{{
     *  def method__xyz__(p1: T1, ..., pn: TN): any = {
     *    this.method__xyz__R(p1, ..., pn)
     *  }
     *  }}}
     */
    final case class ReflectiveProxy(target: MethodName)
        extends MethodSyntheticKind

    /** Bridge to a default method.
     *
     *  After the linker, default methods are not inherited anymore. Bridges
     *  are generated where appropriate to statically call the corresponding
     *  default method in the target interface.
     *
     *  The shape of default bridges is
     *
     *  {{{
     *  def method__xyz(p1: T1, ..., pn: TN): R = {
     *    this.TargetInterface::method__xyz(p1, ..., pn)
     *  }
     *  }}}
     */
    final case class DefaultBridge(targetInterface: ClassName)
        extends MethodSyntheticKind
  }

  sealed trait Error {
    def from: From
  }

  final case class MissingJavaLangObjectClass(from: From) extends Error
  final case class InvalidJavaLangObjectClass(from: From) extends Error
  final case class CycleInInheritanceChain(encodedClassNames: List[ClassName], from: From) extends Error
  final case class MissingClass(info: ClassInfo, from: From) extends Error

  final case class MissingSuperClass(subClassInfo: ClassInfo, from: From)
      extends Error

  final case class InvalidSuperClass(superClassInfo: ClassInfo,
      subClassInfo: ClassInfo, from: From)
      extends Error

  final case class InvalidImplementedInterface(superIntfInfo: ClassInfo,
      subClassInfo: ClassInfo, from: From)
      extends Error

  final case class NotAModule(info: ClassInfo, from: From) extends Error
  final case class MissingMethod(info: MethodInfo, from: From) extends Error
  final case class ConflictingDefaultMethods(infos: List[MethodInfo], from: From) extends Error
  final case class ConflictingTopLevelExport(name: String, infos: List[ClassInfo]) extends Error {
    def from: From = FromExports
  }

  sealed trait From
  final case class FromMethod(methodInfo: MethodInfo) extends From
  final case class FromClass(classInfo: ClassInfo) extends From
  final case class FromCore(moduleName: String) extends From
  case object FromExports extends From

  def logError(error: Error, logger: Logger, level: Level): Unit = {
    val headMsg = error match {
      case MissingJavaLangObjectClass(_) =>
        "Fatal error: java.lang.Object is missing"
      case InvalidJavaLangObjectClass(_) =>
        "Fatal error: java.lang.Object is invalid (it must be a Scala class " +
        "without superclass nor any implemented interface)"
      case CycleInInheritanceChain(encodedClassNames, _) =>
        ("Fatal error: cycle in inheritance chain involving " +
            encodedClassNames.map(decodeClassName).mkString(", "))
      case MissingClass(info, _) =>
        s"Referring to non-existent class ${info.displayName}"
      case MissingSuperClass(subClassInfo, _) =>
        s"${subClassInfo.displayName} (of kind ${subClassInfo.kind}) is " +
        "missing a super class"
      case InvalidSuperClass(superClassInfo, subClassInfo, _) =>
        s"${superClassInfo.displayName} (of kind ${superClassInfo.kind}) is " +
        s"not a valid super class of ${subClassInfo.displayName} (of kind " +
        s"${subClassInfo.kind})"
      case InvalidImplementedInterface(superIntfInfo, subClassInfo, _) =>
        s"${superIntfInfo.displayName} (of kind ${superIntfInfo.kind}) is " +
        s"not a valid interface implemented by ${subClassInfo.displayName} " +
        s"(of kind ${subClassInfo.kind})"
      case NotAModule(info, _) =>
        s"Cannot access module for non-module ${info.displayName}"
      case MissingMethod(info, _) =>
        s"Referring to non-existent method ${info.fullDisplayName}"
      case ConflictingDefaultMethods(infos, _) =>
        s"Conflicting default methods: ${infos.map(_.fullDisplayName).mkString(" ")}"
      case ConflictingTopLevelExport(name, infos) =>
        s"Conflicting top level export for name $name involving " +
        infos.map(_.displayName).mkString(", ")
    }

    logger.log(level, headMsg)
    val csl = new CallStackLogger(logger)
    csl.logCallStack(error.from, level)
  }

  private class CallStackLogger(logger: Logger) {
    private[this] val seenInfos = mutable.Set.empty[AnyRef]
    private[this] var indentation: String = ""

    def logCallStack(from: From, level: Level): Unit = {
      logCallStackImpl(level, Some(from))
      seenInfos.clear()
    }

    private def log(level: Level, msg: String) =
      logger.log(level, indentation+msg)

    private def indented[A](body: => A): A = {
      indentation += "  "
      try body
      finally indentation = indentation.substring(2)
    }

    private def logCallStackImpl(level: Level, optFrom: Option[From],
        verb: String = "called"): Unit = {
      val involvedClasses = new mutable.ListBuffer[ClassInfo]

      def onlyOnce(level: Level, info: AnyRef): Boolean = {
        if (seenInfos.add(info)) {
          true
        } else {
          log(level, "  (already seen, not repeating call stack)")
          false
        }
      }

      @tailrec
      def loopTrace(optFrom: Option[From], verb: String = "called"): Unit = {
        optFrom match {
          case None =>
            log(level, s"$verb from ... er ... nowhere!? (this is a bug in dce)")
          case Some(from) =>
            from match {
              case FromMethod(methodInfo) =>
                log(level, s"$verb from ${methodInfo.fullDisplayName}")
                if (onlyOnce(level, methodInfo)) {
                  involvedClasses ++= methodInfo.instantiatedSubclasses
                  loopTrace(methodInfo.calledFrom.lastOption)
                }
              case FromClass(classInfo) =>
                log(level, s"$verb from ${classInfo.displayName}")
                loopTrace(classInfo.linkedFrom.lastOption)
              case FromCore(moduleName) =>
                log(level, s"$verb from core module $moduleName")
              case FromExports =>
                log(level, "exported to JavaScript with @JSExport")
            }
        }
      }

      indented {
        loopTrace(optFrom, verb = verb)
      }

      if (involvedClasses.nonEmpty) {
        log(level, "involving instantiated classes:")
        indented {
          for (classInfo <- involvedClasses.result().distinct) {
            log(level, classInfo.displayName)

            // recurse with Debug log level not to overwhelm the user
            if (onlyOnce(Level.Debug, classInfo)) {
              logCallStackImpl(Level.Debug,
                  classInfo.instantiatedFrom.lastOption, verb = "instantiated")
            }
          }
        }
      }
    }
  }

}
