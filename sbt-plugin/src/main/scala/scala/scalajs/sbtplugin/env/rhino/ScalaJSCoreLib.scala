/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.sbtplugin.env.rhino

import scala.collection.mutable

import org.mozilla.javascript.{Context, Scriptable}

import scala.scalajs.ir

import scala.scalajs.tools.io._
import scala.scalajs.tools.classpath._

class ScalaJSCoreLib(classpath: ScalaJSClasspath) {
  import ScalaJSCoreLib._

  private val (providers, exportedSymbols) = {
    import ir.Trees._

    val providers = mutable.Map.empty[String, VirtualScalaJSIRFile]
    val exportedSymbols = mutable.ListBuffer.empty[String]

    for (irFile <- classpath.irFiles) {
      val (encodedName, _, isExported) =
        ScalaJSClasspath.extractCoreInfo(irFile.info)
      providers += encodedName -> irFile
      if (isExported)
        exportedSymbols += encodedName
    }

    (providers, exportedSymbols)
  }

  def insertInto(context: Context, scope: Scriptable) = {
    context.evaluateFile(scope, classpath.coreJSLibFile)
    lazifyScalaJSFields(scope)

    // Make sure exported symbols are loaded
    val ScalaJS = scope.get("ScalaJS", scope).asInstanceOf[Scriptable]
    val c = ScalaJS.get("c", ScalaJS).asInstanceOf[Scriptable]
    for (encodedName <- exportedSymbols)
      c.get(encodedName, c)
  }

  private val scalaJSLazyFields = Seq(
      Info("data"),
      Info("c"),
      Info("inheritable"),
      Info("impls", isTraitImpl = true),
      Info("moduleInstances", isModule = true),
      Info("modules", isModule = true),
      Info("is"),
      Info("as"),
      Info("isArrayOf"),
      Info("asArrayOf"))

  private def lazifyScalaJSFields(scope: Scriptable) = {
    val ScalaJS = scope.get("ScalaJS", scope).asInstanceOf[Scriptable]

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
      // TODO? Convert the desugared IR tree directly to Rhino ASTs?
      val codeWriter = new java.io.StringWriter
      val printer = new ir.Printers.IRTreePrinter(codeWriter)
      val classDef = irFile.tree
      val desugared = ir.JSDesugaring.desugarJavaScript(classDef)
      printer.printTopLevelTree(desugared)
      printer.close()
      val ctx = Context.getCurrentContext()
      ctx.evaluateString(scope, codeWriter.toString(),
          classDef.pos.source.toString, classDef.pos.line, null)
    }
  }
}

object ScalaJSCoreLib {
  private case class Info(name: String,
      isModule: Boolean = false, isTraitImpl: Boolean = false)

  private val EncodedNameLine = raw""""encodedName": *"([^"]+)"""".r.unanchored
}
