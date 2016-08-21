/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.linker.checker

import scala.language.implicitConversions

import scala.annotation.switch

import scala.collection.mutable

import java.io.StringWriter

import org.scalajs.core.ir._
import Definitions._
import Infos._
import Trees._
import Types._
import Printers._

import org.scalajs.core.tools.logging._

/** Checker for the validity of the IR. */
private final class InfoChecker(
    infoAndTrees: Traversable[(ClassInfo, ClassDef)], logger: Logger) {

  private var errorCount: Int = 0

  def check(): Int = {
    for ((info, classDef) <- infoAndTrees) {
      val generatedInfo = generateClassInfo(classDef)
      checkClassInfo(info, generatedInfo)
    }
    errorCount
  }

  private def checkClassInfo(info: ClassInfo, expectedInfo: ClassInfo): Unit = {
    val className = expectedInfo.encodedName

    /* Due to the hack for AbstractJSType and NativeJSClass in the
     * deserializer, it is possible to get info where `kind == AbstractJSType`
     * but `expectedInfo == NativeJSClass`. We need to tolerate that here.
     * TODO Get rid of this when we break binary compatibility.
     */
    val patchedInfoKind = {
      import ClassKind._
      if (info.kind == AbstractJSType && expectedInfo.kind == NativeJSClass)
        NativeJSClass
      else
        info.kind
    }

    if (info.encodedName != expectedInfo.encodedName ||
        info.isExported != expectedInfo.isExported ||
        patchedInfoKind != expectedInfo.kind ||
        info.superClass != expectedInfo.superClass ||
        info.interfaces.toSet != expectedInfo.interfaces.toSet) {
      errorCount += 1
      logger.error(s"Class info mismatch for $className")
      logger.error(s"Expected:\n${classInfoHeaderString(expectedInfo)}")
      logger.error(s"Got:\n${classInfoHeaderString(info)}")
    }

    def methodID(methodInfo: MethodInfo) =
      (methodInfo.encodedName, methodInfo.isStatic)

    val actualMethods = info.methods.map(m => methodID(m) -> m).toMap
    val expectedMethods = expectedInfo.methods.map(m => methodID(m) -> m).toMap

    val actualMethodIDs = actualMethods.keySet
    val expectedMethodIDs = expectedMethods.keySet

    if (actualMethodIDs != expectedMethodIDs) {
      val missingMethods = expectedMethodIDs -- actualMethodIDs
      if (missingMethods.nonEmpty) {
        errorCount += 1
        logger.error(
            s"Missing methods in $className: $missingMethods")
      }

      val unexpectedMethods = actualMethodIDs -- expectedMethodIDs
      if (unexpectedMethods.nonEmpty) {
        errorCount += 1
        logger.error(
            s"Unexpected methods in $className: $unexpectedMethods")
      }
    }

    for {
      (id, actualMethodInfo) <- actualMethods
      expectedMethodInfo <- expectedMethods.get(id)
    } {
      checkMethodInfo(className, actualMethodInfo, expectedMethodInfo)
    }
  }

  private def checkMethodInfo(className: String, info: MethodInfo,
      expectedInfo: MethodInfo): Unit = {

    /* Note that it is fine for the actual info to contain *more* than the
     * expected info. It can produce non-optimal results, but it is still
     * correct.
     * Our compiler does generate such non-optimal info in some cases, for
     * example when it deconstructs a `new AnonFunctionN` when converting it
     * immediately to a js.FunctionN.
     */

    def listIncludes(actual: List[String], expected: List[String]) = {
      val actualSet = actual.toSet
      expected.forall(actualSet)
    }

    def mapIncludes(actual: Map[String, List[String]],
        expected: Map[String, List[String]]) = {
      expected forall { case (cls, expectedMethods) =>
        actual.get(cls) exists { actualMethods =>
          listIncludes(actualMethods, expectedMethods)
        }
      }
    }

    if (info.encodedName != expectedInfo.encodedName ||
        info.isStatic != expectedInfo.isStatic ||
        info.isAbstract != expectedInfo.isAbstract ||
        info.isExported != expectedInfo.isExported ||
        !mapIncludes(info.methodsCalled, expectedInfo.methodsCalled) ||
        !mapIncludes(info.methodsCalledStatically, expectedInfo.methodsCalledStatically) ||
        !mapIncludes(info.staticMethodsCalled, expectedInfo.staticMethodsCalled) ||
        !listIncludes(info.instantiatedClasses, expectedInfo.instantiatedClasses) ||
        !listIncludes(info.accessedModules, expectedInfo.accessedModules) ||
        !listIncludes(info.usedInstanceTests, expectedInfo.usedInstanceTests) ||
        !listIncludes(info.accessedClassData, expectedInfo.accessedClassData)) {
      errorCount += 1
      logger.error(s"Method info mismatch for $className.${expectedInfo.encodedName}" +
          (if (expectedInfo.isStatic) " (static)" else ""))
      logger.error(s"Expected:\n${methodInfoString(expectedInfo)}")
      logger.error(s"Got:\n${methodInfoString(info)}")
    }
  }

  private def classInfoHeaderString(info: ClassInfo): String = {
    val writer = new StringWriter
    val printer = new InfoPrinter(writer)
    printer.printClassInfoHeader(info)
    writer.toString()
  }

  private def methodInfoString(info: MethodInfo): String = {
    val writer = new StringWriter
    val printer = new InfoPrinter(writer)
    printer.print(info)
    writer.toString()
  }
}

object InfoChecker {
  /** Checks that the `ClassInfo`s associated with `ClassDef`s are correct.
   *
   *  @return Count of info checking errors (0 in case of success)
   */
  def check(infoAndTrees: Traversable[(ClassInfo, ClassDef)],
      logger: Logger): Int = {
    new InfoChecker(infoAndTrees, logger).check()
  }
}
