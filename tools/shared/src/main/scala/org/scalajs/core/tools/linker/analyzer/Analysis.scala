/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.linker.analyzer

import scala.annotation.tailrec

import scala.collection.mutable

import org.scalajs.core.tools.logging._

import org.scalajs.core.ir
import ir.ClassKind
import ir.Infos
import ir.Definitions.{decodeClassName, decodeMethodName}

/** Reachability graph produced by the [[Analyzer]].
 *
 *  Warning: this trait is not meant to be extended by third-party libraries
 *  and applications. Methods and/or fields can be added in subsequent
 *  versions, possibly causing `LinkageError`s if you extend it.
 */
trait Analysis {
  import Analysis._

  def allAvailable: Boolean
  def classInfos: scala.collection.Map[String, ClassInfo]
  def errors: Seq[Error]
}

object Analysis {

  /** Class node in a reachability graph produced by the [[Analyzer]].
   *
   *  Warning: this trait is not meant to be extended by third-party libraries
   *  and applications. Methods and/or fields can be added in subsequent
   *  versions, possibly causing `LinkageError`s if you extend it.
   */
  trait ClassInfo {
    def encodedName: String
    def kind: ClassKind
    def isExported: Boolean
    def superClass: ClassInfo
    def ancestors: Seq[ClassInfo]
    def descendants: Seq[ClassInfo]
    def nonExistent: Boolean
    def ancestorCount: Int
    def descendentClasses: Seq[ClassInfo]
    /** For a Scala class, it is instantiated with a `New`; for a JS class,
     *  its constructor is accessed with a `JSLoadConstructor` or because it
     *  is needed for a subclass.
     */
    def isInstantiated: Boolean
    def isAnySubclassInstantiated: Boolean
    def isModuleAccessed: Boolean
    def areInstanceTestsUsed: Boolean
    def isDataAccessed: Boolean
    def instantiatedFrom: Seq[From]
    def isNeededAtAll: Boolean
    def isAnyStaticMethodReachable: Boolean
    def methodInfos: scala.collection.Map[String, MethodInfo]
    def staticMethodInfos: scala.collection.Map[String, MethodInfo]

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
    def encodedName: String
    def isStatic: Boolean
    def isAbstract: Boolean
    def isExported: Boolean
    def isReflProxy: Boolean
    def isReachable: Boolean
    def calledFrom: Seq[From]
    def instantiatedSubclasses: Seq[ClassInfo]
    def nonExistent: Boolean
    def syntheticKind: MethodSyntheticKind

    def displayName: String = {
      if (isExported) {
        encodedName
      } else {
        import ir.Types._

        def typeDisplayName(tpe: ReferenceType): String = tpe match {
          case ClassType(encodedName)      => decodeClassName(encodedName)
          case ArrayType(base, dimensions) => "[" * dimensions + decodeClassName(base)
        }

        val (simpleName, paramTypes, resultType) =
          ir.Definitions.decodeMethodName(encodedName)

        simpleName + "(" + paramTypes.map(typeDisplayName).mkString(",") + ")" +
        resultType.fold("")(typeDisplayName)
      }
    }

    def fullDisplayName: String =
      owner.displayName + "." + displayName
  }

  sealed trait MethodSyntheticKind

  object MethodSyntheticKind {
    /** Not a synthetic method. */
    final case object None extends MethodSyntheticKind

    // TODO Get rid of InheritedConstructor when we can break binary compat
    /** An explicit call-super constructor.
     *
     *  In a class `Foo` with parent class `Bar`, an inherited
     *  constructor `init___xyz` looks like
     *
     *  {{{
     *  def init___xyz(p1: T1, ..., pn: TN) {
     *    this.Bar::init___xyz(p1, ..., pn)
     *  }
     *  }}}
     */
    final case object InheritedConstructor extends MethodSyntheticKind

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
    final case class ReflectiveProxy(target: String) extends MethodSyntheticKind

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
    final case class DefaultBridge(targetInterface: String) extends MethodSyntheticKind
  }

  sealed trait Error {
    def from: From
  }

  final case class MissingJavaLangObjectClass(from: From) extends Error
  final case class CycleInInheritanceChain(cycle: List[ClassInfo], from: From) extends Error
  final case class MissingClass(info: ClassInfo, from: From) extends Error
  final case class NotAModule(info: ClassInfo, from: From) extends Error
  final case class MissingMethod(info: MethodInfo, from: From) extends Error
  final case class ConflictingDefaultMethods(infos: List[MethodInfo], from: From) extends Error

  sealed trait From
  final case class FromMethod(methodInfo: MethodInfo) extends From
  final case class FromCore(moduleName: String) extends From
  case object FromExports extends From

  def logError(error: Error, logger: Logger, level: Level): Unit = {
    val headMsg = error match {
      case MissingJavaLangObjectClass(_) =>
        "Fatal error: java.lang.Object is missing"
      case CycleInInheritanceChain(cycle, _) =>
        ("Fatal error: cycle in inheritance chain involving " +
            cycle.map(_.displayName).mkString(", "))
      case MissingClass(info, _) =>
        s"Referring to non-existent class ${info.displayName}"
      case NotAModule(info, _) =>
        s"Cannot access module for non-module ${info.displayName}"
      case MissingMethod(info, _) =>
        s"Referring to non-existent method ${info.fullDisplayName}"
      case ConflictingDefaultMethods(infos, _) =>
        s"Conflicting default methods: ${infos.map(_.fullDisplayName).mkString(" ")}"
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
