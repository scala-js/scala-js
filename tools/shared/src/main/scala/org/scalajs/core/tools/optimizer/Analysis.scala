/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.optimizer

import scala.annotation.tailrec

import scala.collection.mutable

import org.scalajs.core.tools.logging._

import org.scalajs.core.ir
import ir.ClassKind
import ir.Infos

trait Analysis {
  import Analysis._

  def allAvailable: Boolean
  def classInfos: scala.collection.Map[String, ClassInfo]
  def errors: Seq[Error]
}

object Analysis {

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
  }

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
  }

  sealed trait MethodSyntheticKind

  object MethodSyntheticKind {
    final case object None extends MethodSyntheticKind
    // TODO Get rid of InheritedConstructor when we can break binary compat
    final case object InheritedConstructor extends MethodSyntheticKind
  }

  sealed trait Error {
    def from: From
  }

  final case class MissingClass(info: ClassInfo, from: From) extends Error
  final case class NotAModule(info: ClassInfo, from: From) extends Error
  final case class MissingMethod(info: MethodInfo, from: From) extends Error

  sealed trait From
  final case class FromMethod(methodInfo: MethodInfo) extends From
  case object FromCore extends From
  case object FromExports extends From

  def logError(error: Error, logger: Logger, level: Level): Unit = {
    val headMsg = error match {
      case MissingClass(info, _) =>
        s"Referring to non-existent class $info"
      case NotAModule(info, _) =>
        s"Cannot access module for non-module $info"
      case MissingMethod(info, _) =>
        s"Referring to non-existent method $info"
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

      def onlyOnce(info: AnyRef): Boolean = {
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
                log(level, s"$verb from $methodInfo")
                if (onlyOnce(methodInfo)) {
                  involvedClasses ++= methodInfo.instantiatedSubclasses
                  loopTrace(methodInfo.calledFrom.headOption)
                }
              case FromCore =>
                log(level, s"$verb from scalajs-corejslib.js")
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
            log(level, s"$classInfo")
            if (onlyOnce(classInfo))
              logCallStackImpl(Level.Debug,
                  classInfo.instantiatedFrom.headOption, verb = "instantiated")
            // recurse with Debug log level not to overwhelm the user
          }
        }
      }
    }
  }

}
