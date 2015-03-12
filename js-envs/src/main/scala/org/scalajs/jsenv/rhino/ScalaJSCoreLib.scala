/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.jsenv.rhino

import scala.collection.mutable

import org.mozilla.javascript.{Context, Scriptable}

import org.scalajs.core.ir

import org.scalajs.core.tools.sem.Semantics
import org.scalajs.core.tools.optimizer.{LinkedClass, LinkingUnit}
import org.scalajs.core.tools.javascript._
import org.scalajs.core.tools.io._
import org.scalajs.core.tools.classpath._
import org.scalajs.core.tools.corelib._

import OutputMode.ECMAScript51Global

class ScalaJSCoreLib(semantics: Semantics, classpath: IRClasspath) {
  import ScalaJSCoreLib._

  private val (providers, exportedSymbols) = {
    val providers = mutable.Map.empty[String, VirtualScalaJSIRFile]
    val exportedSymbols = mutable.ListBuffer.empty[String]

    for (irFile <- classpath.scalaJSIR) {
      val info = irFile.info
      providers += info.encodedName -> irFile
      if (info.isExported)
        exportedSymbols += info.encodedName
    }

    (providers, exportedSymbols)
  }

  private val ancestorStore = mutable.Map.empty[String, List[String]]

  def insertInto(context: Context, scope: Scriptable) = {
    CoreJSLibs.libs(semantics, ECMAScript51Global).foreach(
        context.evaluateFile(scope, _))
    lazifyScalaJSFields(scope)

    // Make sure exported symbols are loaded
    val ScalaJS = Context.toObject(scope.get("ScalaJS", scope), scope)
    val c = Context.toObject(ScalaJS.get("c", ScalaJS), scope)
    for (encodedName <- exportedSymbols)
      c.get(encodedName, c)
  }

  /** Source maps the given stack trace (where possible) */
  def mapStackTrace(stackTrace: Scriptable,
      context: Context, scope: Scriptable): Scriptable = {
    val count = Context.toNumber(stackTrace.get("length", stackTrace)).toInt

    // Maps file -> max line (0-based)
    val neededMaps = mutable.Map.empty[String, Int]

    // Collect required line counts
    for (i <- 0 until count) {
      val elem = Context.toObject(stackTrace.get(i, stackTrace), scope)
      val fileName = Context.toString(elem.get("fileName", elem))

      if (fileName.endsWith(PseudoFileSuffix) &&
          providers.contains(fileName.stripSuffix(PseudoFileSuffix))) {

        val curMaxLine = neededMaps.getOrElse(fileName, -1)
        val reqLine = Context.toNumber(elem.get("lineNumber", elem)).toInt - 1

        if (reqLine > curMaxLine)
          neededMaps.put(fileName, reqLine)
      }
    }

    // Map required files
    val maps =
      for ((fileName, maxLine) <- neededMaps)
        yield (fileName, getSourceMapper(fileName, maxLine))

    // Create new stack trace to return
    val res = context.newArray(scope, count)

    for (i <- 0 until count) {
      val elem = Context.toObject(stackTrace.get(i, stackTrace), scope)
      val fileName = Context.toString(elem.get("fileName", elem))
      val line = Context.toNumber(elem.get("lineNumber", elem)).toInt - 1

      val pos = maps.get(fileName).fold(ir.Position.NoPosition)(_(line))

      val newElem =
        if (pos.isDefined) newPosElem(scope, context, elem, pos)
        else elem

      res.put(i, res, newElem)
    }

    res
  }

  private def getSourceMapper(fileName: String, untilLine: Int) = {
    val irFile = providers(fileName.stripSuffix(PseudoFileSuffix))
    val mapper = new Printers.ReverseSourceMapPrinter(untilLine)
    val (info, classDef) = irFile.infoAndTree
    val className = classDef.name.name
    val ancestors = ancestorStore.getOrElse(className,
        throw new AssertionError(s"$className should be loaded"))
    val linked = LinkedClass(info, classDef, ancestors)
    val desugared = new ScalaJSClassEmitter(semantics, ECMAScript51Global,
        LinkingUnit.GlobalInfo.SafeApproximation).genClassDef(linked)
    mapper.reverseSourceMap(desugared)
    mapper
  }

  private def newPosElem(scope: Scriptable, context: Context,
      origElem: Scriptable, pos: ir.Position): Scriptable = {
    assert(pos.isDefined)

    val elem = context.newObject(scope)

    elem.put("declaringClass", elem, origElem.get("declaringClass", origElem))
    elem.put("methodName", elem, origElem.get("methodName", origElem))
    elem.put("fileName", elem, pos.source.toString)
    elem.put("lineNumber", elem, pos.line + 1)
    elem.put("columnNumber", elem, pos.column + 1)

    elem
  }

  private val scalaJSLazyFields = Seq(
      Info("d"),
      Info("c"),
      Info("h"),
      Info("s", isStatics = true),
      Info("n"),
      Info("m"),
      Info("is"),
      Info("as"),
      Info("isArrayOf"),
      Info("asArrayOf"))

  private def lazifyScalaJSFields(scope: Scriptable) = {
    val ScalaJS = Context.toObject(scope.get("ScalaJS", scope), scope)

    def makeLazyScalaJSScope(base: Scriptable, isStatics: Boolean) =
      new LazyScalaJSScope(this, scope, base, isStatics)

    for (Info(name, isStatics) <- scalaJSLazyFields) {
      val base = ScalaJS.get(name, ScalaJS).asInstanceOf[Scriptable]
      val lazified = makeLazyScalaJSScope(base, isStatics)
      ScalaJS.put(name, ScalaJS, lazified)
    }
  }

  private[rhino] def load(scope: Scriptable, encodedName: String): Unit =
    loadAncestors(scope, encodedName)

  private def loadAncestors(scope: Scriptable,
      encodedName: String): List[String] = {
    ancestorStore.getOrElseUpdate(encodedName, {
      val irFile = providers.getOrElse(encodedName,
        throw new ClassNotFoundException(encodedName))

      // Desugar tree
      val (info, classDef) = irFile.infoAndTree
      val className = classDef.name.name

      val strictAncestors = for {
        parent   <- classDef.superClass ++: classDef.interfaces
        ancestor <- loadAncestors(scope, parent.name)
      } yield ancestor

      val ancestors = className :: strictAncestors.distinct
      val linked = LinkedClass(info, classDef, ancestors)

      val desugared = new ScalaJSClassEmitter(semantics, ECMAScript51Global,
          LinkingUnit.GlobalInfo.SafeApproximation).genClassDef(linked)

      // Write tree
      val codeWriter = new java.io.StringWriter
      val printer = new Printers.JSTreePrinter(codeWriter)
      printer.printTopLevelTree(desugared)
      printer.complete()
      val ctx = Context.getCurrentContext()
      val fakeFileName = encodedName + PseudoFileSuffix
      ctx.evaluateString(scope, codeWriter.toString(),
          fakeFileName, 1, null)

      ancestors
    })
  }
}

object ScalaJSCoreLib {
  private case class Info(name: String, isStatics: Boolean = false)

  private val EncodedNameLine = raw""""encodedName": *"([^"]+)"""".r.unanchored

  private final val PseudoFileSuffix = ".sjsir"

  final class ClassNotFoundException(className: String) extends Exception(
    s"Rhino was unable to load Scala.js class: $className")

}
