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

import org.scalajs.linker.standard.ModuleSet.ModuleID

import org.scalajs.ir.ClassKind
import org.scalajs.ir.Names._
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
  def topLevelExportInfos: scala.collection.Map[(ModuleID, String), TopLevelExportInfo]

  def isClassSuperClassUsed: Boolean

  def errors: scala.collection.Seq[Error]
}

object Analysis {

  /** Class node in a reachability graph produced by the [[Analyzer]].
   *
   *  Warning: this trait is not meant to be extended by third-party libraries
   *  and applications. Methods and/or fields can be added in subsequent
   *  versions, possibly causing `LinkageError`s if you extend it.
   */
  trait ClassInfo {
    def className: ClassName
    def kind: ClassKind
    def superClass: Option[ClassInfo]
    def interfaces: scala.collection.Seq[ClassInfo]
    def ancestors: scala.collection.Seq[ClassInfo]
    def nonExistent: Boolean
    /** For a Scala class, it is instantiated with a `New`; for a JS class,
     *  its constructor is accessed with a `JSLoadConstructor` or because it
     *  is needed for a subclass. For modules (Scala or JS), the module is
     *  accessed.
     */
    def isInstantiated: Boolean
    def isAnySubclassInstantiated: Boolean
    def areInstanceTestsUsed: Boolean
    def isDataAccessed: Boolean

    def fieldsRead: scala.collection.Set[FieldName]
    def fieldsWritten: scala.collection.Set[FieldName]
    def staticFieldsRead: scala.collection.Set[FieldName]
    def staticFieldsWritten: scala.collection.Set[FieldName]

    def jsNativeMembersUsed: scala.collection.Set[MethodName]

    def staticDependencies: scala.collection.Set[ClassName]
    def externalDependencies: scala.collection.Set[String]
    def dynamicDependencies: scala.collection.Set[ClassName]

    def linkedFrom: scala.collection.Seq[From]
    def instantiatedFrom: scala.collection.Seq[From]
    def dispatchCalledFrom(methodName: MethodName): Option[scala.collection.Seq[From]]
    def methodInfos(
        namespace: MemberNamespace): scala.collection.Map[MethodName, MethodInfo]

    def displayName: String = className.nameString
  }

  /** Method node in a reachability graph produced by the [[Analyzer]].
   *
   *  Warning: this trait is not meant to be extended by third-party libraries
   *  and applications. Methods and/or fields can be added in subsequent
   *  versions, possibly causing `LinkageError`s if you extend it.
   */
  trait MethodInfo {
    def owner: ClassInfo
    def methodName: MethodName
    def namespace: MemberNamespace
    def isAbstractReachable: Boolean
    def isReachable: Boolean
    def calledFrom: scala.collection.Seq[From]
    def instantiatedSubclasses: scala.collection.Seq[ClassInfo]
    def nonExistent: Boolean
    def syntheticKind: MethodSyntheticKind

    def displayName: String = methodName.displayName

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

  trait TopLevelExportInfo {
    def moduleID: ModuleID
    def exportName: String
    def owningClass: ClassName
    def staticDependencies: scala.collection.Set[ClassName]
    def externalDependencies: scala.collection.Set[String]
  }

  sealed trait Error {
    def from: From
  }

  final case class CycleInInheritanceChain(encodedClassNames: List[ClassName], from: From) extends Error
  final case class MissingClass(info: ClassInfo, from: From) extends Error

  final case class InvalidSuperClass(superClassInfo: ClassInfo,
      subClassInfo: ClassInfo, from: From)
      extends Error

  final case class InvalidImplementedInterface(superIntfInfo: ClassInfo,
      subClassInfo: ClassInfo, from: From)
      extends Error

  final case class NotAModule(info: ClassInfo, from: From) extends Error
  final case class MissingMethod(info: MethodInfo, from: From) extends Error
  final case class MissingJSNativeMember(info: ClassInfo, name: MethodName, from: From) extends Error
  final case class ConflictingDefaultMethods(infos: List[MethodInfo], from: From) extends Error

  final case class InvalidTopLevelExportInScript(info: TopLevelExportInfo) extends Error {
    def from: From = FromExports
  }

  final case class ConflictingTopLevelExport(moduleID: ModuleID, exportName: String,
      infos: List[TopLevelExportInfo]) extends Error {
    def from: From = FromExports
  }

  final case class ImportWithoutModuleSupport(module: String, info: ClassInfo,
      jsNativeMember: Option[MethodName], from: From) extends Error

  final case class MultiplePublicModulesWithoutModuleSupport(
      moduleIDs: List[ModuleID]) extends Error {
    def from: From = FromExports
  }

  final case class DynamicImportWithoutModuleSupport(from: From) extends Error

  final case class NewTargetWithoutES2015Support(from: From) extends Error

  final case class ImportMetaWithoutESModule(from: From) extends Error

  final case class ExponentOperatorWithoutES2016Support(from: From) extends Error

  sealed trait From
  final case class FromMethod(methodInfo: MethodInfo) extends From
  final case class FromDispatch(classInfo: ClassInfo, methodName: MethodName) extends From
  final case class FromClass(classInfo: ClassInfo) extends From
  final case class FromCore(moduleName: String) extends From
  case object FromExports extends From

  def logError(error: Error, logger: Logger, level: Level): Unit = {
    val headMsg = error match {
      case CycleInInheritanceChain(encodedClassNames, _) =>
        ("Fatal error: cycle in inheritance chain involving " +
            encodedClassNames.map(_.nameString).mkString(", "))
      case MissingClass(info, _) =>
        s"Referring to non-existent class ${info.displayName}"
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
      case MissingJSNativeMember(info, name, _) =>
        s"Referring to non-existent js native member ${info.displayName}.${name.displayName}"
      case ConflictingDefaultMethods(infos, _) =>
        s"Conflicting default methods: ${infos.map(_.fullDisplayName).mkString(" ")}"
      case InvalidTopLevelExportInScript(info) =>
        s"Invalid top level export for name '${info.exportName}' in class " +
        s"${info.owningClass.nameString} when emitting a Script (NoModule) because it " +
        "is not a valid JavaScript identifier " +
        "(did you want to emit a module instead?)"
      case ConflictingTopLevelExport(moduleID, exportName, infos) =>
        s"Conflicting top level exports for module $moduleID, name $exportName " +
        "involving " + infos.map(_.owningClass.nameString).mkString(", ")
      case ImportWithoutModuleSupport(module, info, None, _) =>
        s"${info.displayName} needs to be imported from module " +
        s"'$module' but module support is disabled"
      case ImportWithoutModuleSupport(module, info, Some(jsNativeMember), _) =>
        s"${info.displayName}.${jsNativeMember.displayName} " +
        s"needs to be imported from module '$module' but " +
        "module support is disabled"
      case MultiplePublicModulesWithoutModuleSupport(moduleIDs) =>
        "Found multiple public modules but module support is disabled: " +
        moduleIDs.map(_.id).mkString("[", ", ", "]")
      case DynamicImportWithoutModuleSupport(_) =>
        "Uses dynamic import but module support is disabled"
      case NewTargetWithoutES2015Support(_) =>
        "Uses new.target with an ECMAScript version older than ES 2015"
      case ImportMetaWithoutESModule(_) =>
        "Uses import.meta with a module kind other than ESModule"
      case ExponentOperatorWithoutES2016Support(_) =>
        "Uses the ** operator with an ECMAScript version older than ES 2016"
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
        def sameMethod(methodInfo: MethodInfo, fromDispatch: FromDispatch): Boolean = {
          methodInfo.owner == fromDispatch.classInfo &&
          methodInfo.namespace == MemberNamespace.Public &&
          methodInfo.methodName == fromDispatch.methodName
        }

        def followDispatch(fromDispatch: FromDispatch): Option[From] =
          fromDispatch.classInfo.dispatchCalledFrom(fromDispatch.methodName).flatMap(_.lastOption)

        optFrom match {
          case None =>
            log(level, s"$verb from ... er ... nowhere!? (this is a bug in dce)")
          case Some(from) =>
            from match {
              case FromMethod(methodInfo) =>
                log(level, s"$verb from ${methodInfo.fullDisplayName}")
                if (onlyOnce(level, methodInfo)) {
                  involvedClasses ++= methodInfo.instantiatedSubclasses
                  methodInfo.calledFrom.lastOption match {
                    case Some(fromDispatch: FromDispatch) if sameMethod(methodInfo, fromDispatch) =>
                      // avoid logging "dispatch from C.m" just after "called from C.m"
                      loopTrace(followDispatch(fromDispatch))
                    case nextFrom =>
                      loopTrace(nextFrom)
                  }
                }
              case from @ FromDispatch(classInfo, methodName) =>
                log(level, s"dispatched from ${classInfo.displayName}.${methodName.displayName}")
                loopTrace(followDispatch(from))
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
