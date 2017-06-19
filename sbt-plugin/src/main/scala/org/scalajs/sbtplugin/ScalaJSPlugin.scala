/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.sbtplugin

import scala.language.implicitConversions

import sbt._

import sbtcrossproject._

import org.scalajs.core.ir.ScalaJSVersions

import org.scalajs.core.tools.io._
import org.scalajs.core.tools.linker._
import org.scalajs.core.tools.linker.standard._

import org.scalajs.jsenv.JSEnv

object ScalaJSPlugin extends AutoPlugin {
  override def requires: Plugins = plugins.JvmPlugin

  object autoImport {
    import KeyRanks._

    /** The current version of the Scala.js sbt plugin and tool chain. */
    val scalaJSVersion = ScalaJSVersions.current

    // The JS platform for sbt-crossproject
    val JSPlatform = org.scalajs.sbtplugin.JSPlatform

    implicit def JSCrossProjectBuilderOps(
        builder: CrossProject.Builder): JSCrossProjectOps = {
      new JSCrossProjectOps(builder.crossType(CrossType.Full))
    }

    implicit class JSCrossProjectOps(project: CrossProject) {
      def js: Project = project.projects(JSPlatform)

      def jsSettings(ss: Def.SettingsDefinition*): CrossProject =
        jsConfigure(_.settings(ss: _*))

      def jsConfigure(transformer: Project => Project): CrossProject =
        project.configurePlatform(JSPlatform)(transformer)
    }

    // Stage values
    val FastOptStage = Stage.FastOpt
    val FullOptStage = Stage.FullOpt

    // ModuleKind
    val ModuleKind = org.scalajs.core.tools.linker.backend.ModuleKind

    // All our public-facing keys

    val fastOptJS = TaskKey[Attributed[File]]("fastOptJS",
        "Quickly link all compiled JavaScript into a single file", APlusTask)

    val fullOptJS = TaskKey[Attributed[File]]("fullOptJS",
        "Link all compiled JavaScript into a single file and fully optimize", APlusTask)

    val testHtml = TaskKey[Attributed[File]]("testHtml",
        "Create an HTML test runner. Honors `scalaJSStage`.", AMinusTask)

    val scalaJSIR = TaskKey[Attributed[Seq[VirtualScalaJSIRFile with RelativeVirtualFile]]](
        "scalaJSIR", "All the *.sjsir files on the classpath", CTask)

    val scalaJSModuleInitializers = TaskKey[Seq[ModuleInitializer]]("scalaJSModuleInitializers",
        "Module initializers of the Scala.js application, to be called when it starts.",
        AMinusTask)

    val scalaJSUseMainModuleInitializer = SettingKey[Boolean]("scalaJSUseMainModuleInitializer",
        "If true, adds the `mainClass` as a module initializer of the Scala.js module",
        APlusSetting)

    val scalaJSMainModuleInitializer = TaskKey[Option[ModuleInitializer]](
        "scalaJSMainModuleInitializer",
        "The main module initializer, used if " +
        "`scalaJSUseMainModuleInitializer` is true",
        CTask)

    val scalaJSLinkerConfig = SettingKey[StandardLinker.Config](
        "scalaJSLinkerConfig",
        "Configuration of the Scala.js linker",
        BPlusSetting)

    val scalaJSStage = SettingKey[Stage]("scalaJSStage",
        "The optimization stage at which run and test are executed", APlusSetting)

    val scalaJSLinkedFile = TaskKey[VirtualJSFile]("scalaJSLinkedFile",
        "Linked Scala.js file. This is the result of fastOptJS or fullOptJS, " +
        "depending on the stage.", DTask)

    val jsEnv = TaskKey[JSEnv]("jsEnv",
        "The JavaScript environment in which to run and test Scala.js applications.",
        AMinusTask)

    /** Prints the content of a .sjsir file in human readable form. */
    val scalajsp = InputKey[Unit]("scalajsp",
        "Prints the content of a .sjsir file in human readable form.",
        CTask)

    val jsExecutionFiles = TaskKey[Seq[VirtualJSFile]](
        "jsExecutionFiles",
        "All the JS files given to JS environments on `run`, `test`, etc.",
        BTask)

    val scalaJSJavaSystemProperties = TaskKey[Map[String, String]](
        "scalaJSJavaSystemProperties",
        "List of arguments to pass to the Scala.js Java System.properties.",
        CTask)

    val scalaJSSourceMap = AttributeKey[File]("scalaJSSourceMap",
        "Source map file attached to an Attributed .js file.",
        BSetting)
  }

  import autoImport._
  import ScalaJSPluginInternal._

  override def globalSettings: Seq[Setting[_]] = {
    Seq(
        scalaJSStage := Stage.FastOpt,
        scalaJSClearCacheStats := globalIRCache.clearStats()
    )
  }

  override def projectSettings: Seq[Setting[_]] = scalaJSProjectSettings
}
