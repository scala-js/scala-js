/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.sbtplugin.environment.rhino

import org.mozilla.javascript.Scriptable
import java.io.File
import org.mozilla.javascript.Context
import scala.collection.Map

class ScalaJSCoreLib(file: File,
    scalaJSProviders: Map[String, File] = Map.empty) {

  import ScalaJSCoreLib.Info

  def withProviders(scalaJSProviders: Map[String, File]) =
    new ScalaJSCoreLib(this.file, scalaJSProviders)

  def insertInto(context: Context, scope: Scriptable) = {
    context.evaluateFile(scope, file)
    lazifyScalaJSFields(scope)
  }

  private val scalaJSLazyFields = Seq(
      Info("data"),
      Info("c"),
      Info("inheritable"),
      Info("classes"),
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
      new LazyScalaJSScope(scalaJSProviders, scope, base, isModule, isTraitImpl)

    for (Info(name, isModule, isTraitImpl) <- scalaJSLazyFields) {
      val base = ScalaJS.get(name, ScalaJS).asInstanceOf[Scriptable]
      val lazified = makeLazyScalaJSScope(base, isModule, isTraitImpl)
      ScalaJS.put(name, ScalaJS, lazified)
    }
  }
}

object ScalaJSCoreLib {
  private case class Info(name: String,
      isModule: Boolean = false, isTraitImpl: Boolean = false)
}
