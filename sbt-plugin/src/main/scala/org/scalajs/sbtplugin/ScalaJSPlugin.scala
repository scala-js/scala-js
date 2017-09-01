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
import sbt.Keys._

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
    val ModuleKind = org.scalajs.core.tools.linker.ModuleKind

    // All our public-facing keys

    val isScalaJSProject = SettingKey[Boolean]("isScalaJSProject",
        "Tests whether the current project is a Scala.js project. " +
        "Do not set the value of this setting (only use it as read-only).",
        BSetting)

    // This is lazy to avoid initialization order issues
    lazy val scalaJSIRCache = SettingKey[ScalaJSPluginInternal.globalIRCache.Cache](
        "scalaJSIRCache",
        "Scala.js internal: Task to access a cache.", KeyRanks.Invisible)

    /** Persisted instance of the Scala.js linker.
     *
     *  This setting must be scoped per project, configuration, and stage task
     *  (`fastOptJS` or `fullOptJS`).
     *
     *  If a task uses the `link` method of the `ClearableLinker`, it must be
     *  protected from running in parallel with any other task doing the same
     *  thing, by tagging the task with the value of [[usesScalaJSLinkerTag]]
     *  in the same scope. The typical shape of such a task will be:
     *  {{{
     *  myTask in (Compile, fastOptJS) := Def.taskDyn {
     *    val linker = (scalaJSLinker in (Compile, fastOptJS)).value
     *    val usesLinkerTag = (usesScalaJSLinkerTag in (Compile, fastOptJS)).value
     *    // Read the `.value` of other settings and tasks here
     *
     *    Def.task {
     *      // Do the actual work of the task here, in particular calling
     *      linker.link(...)
     *    }.tag(usesLinkerTag)
     *  }.value,
     *  }}}
     */
    val scalaJSLinker = SettingKey[ClearableLinker]("scalaJSLinker",
        "Persisted instance of the Scala.js linker", KeyRanks.Invisible)

    /** A tag to indicate that a task is using the value of [[scalaJSLinker]]
     *  and its `link` method.
     *
     *  This setting's value should always be retrieved from the same scope
     *  than [[scalaJSLinker]] was retrieved from.
     *
     *  @see [[scalaJSLinker]]
     */
    val usesScalaJSLinkerTag = SettingKey[Tags.Tag]("usesScalaJSLinkerTag",
        "Tag to indicate that a task uses the link method of the value of " +
        "scalaJSLinker",
        KeyRanks.Invisible)

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

    /** All .sjsir files on the fullClasspath, used by scalajsp. */
    val sjsirFilesOnClasspath = TaskKey[Seq[String]]("sjsirFilesOnClasspath",
        "All .sjsir files on the fullClasspath, used by scalajsp",
        KeyRanks.Invisible)

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

    val scalaJSSourceFiles = AttributeKey[Seq[File]]("scalaJSSourceFiles",
        "Files used to compute this value (can be used in FileFunctions later).",
        KeyRanks.Invisible)

    val scalaJSSourceMap = AttributeKey[File]("scalaJSSourceMap",
        "Source map file attached to an Attributed .js file.",
        BSetting)
  }

  import autoImport._

  /** Maps a [[Stage]] to the corresponding `TaskKey`.
   *
   *  For example, [[Stage.FastOpt]] (aka `FastOptStage`) is mapped to
   *  [[autoImport.fastOptJS fastOptJS]].
   */
  val stageKeys: Map[Stage, TaskKey[Attributed[File]]] = Map(
      Stage.FastOpt -> fastOptJS,
      Stage.FullOpt -> fullOptJS
  )

  /** Logs the current statistics about the global IR cache. */
  def logIRCacheStats(logger: Logger): Unit = {
    import ScalaJSPluginInternal.globalIRCache
    logger.debug("Global IR cache stats: " + globalIRCache.stats.logLine)
  }

  override def globalSettings: Seq[Setting[_]] = {
    Seq(
        scalaJSStage := Stage.FastOpt,

        // Clear the IR cache stats every time a sequence of tasks ends
        onComplete := {
          val prev = onComplete.value

          { () =>
            prev()
            ScalaJSPluginInternal.globalIRCache.clearStats()
          }
        },

        /* When unloading the build, free all the IR caches.
         * Note that this runs on `reload`s, for example, but not when we
         * *exit* sbt. That is fine, though, since in that case the process
         * is killed altogether.
         */
        onUnload := {
          onUnload.value.andThen { state =>
            ScalaJSPluginInternal.freeAllIRCaches()
            state
          }
        }
    )
  }

  override def projectSettings: Seq[Setting[_]] =
    ScalaJSPluginInternal.scalaJSProjectSettings

  /** Basic set of settings enabling Scala.js for a configuration.
   *
   *  The `Compile` and `Test` configurations of sbt are already equipped with
   *  these settings. Directly using this method is only necessary if you want
   *  to configure a custom configuration.
   *
   *  Moreover, if your custom configuration is similar in spirit to `Compile`
   *  (resp. `Test`), you should use [[compileConfigSettings]] (resp.
   *  [[testConfigSettings]]) instead.
   */
  def baseConfigSettings: Seq[Setting[_]] =
    ScalaJSPluginInternal.scalaJSConfigSettings

  /** Complete set of settings enabling Scala.js for a `Compile`-like
   *  configuration.
   *
   *  The `Compile` configuration of sbt is already equipped with these
   *  settings. Directly using this method is only necessary if you want to
   *  configure a custom `Compile`-like configuration.
   */
  def compileConfigSettings: Seq[Setting[_]] =
    ScalaJSPluginInternal.scalaJSCompileSettings

  /** Complete set of settings enabling Scala.js for a `Test`-like
   *  configuration.
   *
   *  The `Test` configuration of sbt is already equipped with these settings.
   *  Directly using this method is only necessary if you want to configure a
   *  custom `Test`-like configuration, e.g., `IntegrationTest`.
   */
  def testConfigSettings: Seq[Setting[_]] =
    ScalaJSPluginInternal.scalaJSTestSettings
}
