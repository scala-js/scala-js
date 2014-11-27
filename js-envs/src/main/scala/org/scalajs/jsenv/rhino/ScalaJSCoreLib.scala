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
import org.scalajs.core.tools.javascript.{Printers, ScalaJSClassEmitter}
import org.scalajs.core.tools.io._
import org.scalajs.core.tools.classpath._
import org.scalajs.core.tools.corelib._

class ScalaJSCoreLib(semantics: Semantics, classpath: IRClasspath) {
  import ScalaJSCoreLib._

  private val (providers, exportedSymbols) = {
    val providers = mutable.Map.empty[String, VirtualScalaJSIRFile]
    val exportedSymbols = mutable.ListBuffer.empty[String]

    for (irFile <- classpath.scalaJSIR) {
      val info = irFile.roughInfo
      providers += info.encodedName -> irFile
      if (info.isExported)
        exportedSymbols += info.encodedName
    }

    (providers, exportedSymbols)
  }

  def insertInto(context: Context, scope: Scriptable) = {
    CoreJSLibs.libs(semantics).foreach(context.evaluateFile(scope, _))
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
    val classDef = irFile.tree
    val desugared = new ScalaJSClassEmitter(semantics).genClassDef(classDef)
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
      Info("i", isTraitImpl = true),
      Info("n", isModule = true),
      Info("m", isModule = true),
      Info("is"),
      Info("as"),
      Info("isArrayOf"),
      Info("asArrayOf"))

  private def lazifyScalaJSFields(scope: Scriptable) = {
    val ScalaJS = Context.toObject(scope.get("ScalaJS", scope), scope)

    def makeLazyScalaJSScope(base: Scriptable, isModule: Boolean, isTraitImpl: Boolean) =
      new LazyScalaJSScope(this, scope, base, isModule, isTraitImpl)

    for (Info(name, isModule, isTraitImpl) <- scalaJSLazyFields) {
      val base = ScalaJS.get(name, ScalaJS).asInstanceOf[Scriptable]
      val lazified = makeLazyScalaJSScope(base, isModule, isTraitImpl)
      ScalaJS.put(name, ScalaJS, lazified)
    }
  }

  private[rhino] def load(scope: Scriptable, encodedName: String): Unit = {
    providers.get(encodedName) foreach { irFile =>
      val codeWriter = new java.io.StringWriter
      val printer = new Printers.JSTreePrinter(codeWriter)
      val classDef = irFile.tree
      val desugared = new ScalaJSClassEmitter(semantics).genClassDef(classDef)
      printer.printTopLevelTree(desugared)
      printer.complete()
      val ctx = Context.getCurrentContext()
      val fakeFileName = encodedName + PseudoFileSuffix
      ctx.evaluateString(scope, codeWriter.toString(),
          fakeFileName, 1, null)
    }
  }
}

object ScalaJSCoreLib {
  private case class Info(name: String,
      isModule: Boolean = false, isTraitImpl: Boolean = false)

  private val EncodedNameLine = raw""""encodedName": *"([^"]+)"""".r.unanchored

  private final val PseudoFileSuffix = ".sjsir"
}
