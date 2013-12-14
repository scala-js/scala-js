package scala.scalajs.sbtplugin.environment.rhino

import org.mozilla.javascript.Scriptable
import java.io.File
import org.mozilla.javascript.Context
import scala.collection.Map

case class ScalaJSCoreLib(file: File, scalaJSProviders: Map[String, File] = Map.empty) {

  def withProviders(scalaJSProviders: Map[String, File]) =
    copy(scalaJSProviders = scalaJSProviders)

  def insertInto(context: Context, scope: Scriptable) = {

    context.evaluateFile(scope, file)

    lazifyScalaJSFields(scope)
  }

  import ScalaJSCoreLib.Info

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

    val partialLazyScalaJSScope =
      LazyScalaJSScope(scalaJSProviders, scope, _: Scriptable, _: Boolean, _: Boolean)

    for (Info(name, isModule, isTraitImpl) <- scalaJSLazyFields) {
      val base = ScalaJS.get(name, ScalaJS).asInstanceOf[Scriptable]
      val lazyfied = partialLazyScalaJSScope(base, isModule, isTraitImpl)
      ScalaJS.put(name, ScalaJS, lazyfied)
    }
  }
}

object ScalaJSCoreLib {

  private case class Info(name: String, isModule: Boolean = false, isTraitImpl: Boolean = false)
}
