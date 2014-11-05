/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.sbtplugin

import sbt._

import scala.scalajs.tools.sem.Semantics
import scala.scalajs.tools.classpath._
import scala.scalajs.tools.io.VirtualJSFile
import scala.scalajs.tools.env.{JSEnv, JSConsole}
import scala.scalajs.tools.optimizer.ScalaJSOptimizer

import scala.scalajs.ir.ScalaJSVersions

object ScalaJSPlugin extends Plugin with impl.DependencyBuilders {
  val scalaJSVersion = ScalaJSVersions.current
  val scalaJSIsSnapshotVersion = ScalaJSVersions.currentIsSnapshot
  val scalaJSBinaryVersion = ScalaJSCrossVersion.currentBinaryVersion

  object ScalaJSKeys {
    import KeyRanks._

    val fastOptJS = TaskKey[Attributed[File]]("fastOptJS",
        "Quickly link all compiled JavaScript into a single file", APlusTask)
    val fullOptJS = TaskKey[Attributed[File]]("fullOptJS",
        "Link all compiled JavaScript into a single file and fully optimize", APlusTask)

    val packageScalaJSLauncher = TaskKey[Attributed[File]]("packageScalaJSLauncher",
        "Writes the persistent launcher file. Fails if the mainClass is ambigous", CTask)

    val packageJSDependencies = TaskKey[File]("packageJSDependencies",
        "Packages all dependencies of the preLink classpath in a single file. " +
        "Set skip in packageJSDependencies := false to run automatically", AMinusTask)

    val jsDependencyManifest = TaskKey[File]("jsDependencyManifest",
        "Writes the JS_DEPENDENCIES file.", DTask)

    val scalaJSPreLinkClasspath = TaskKey[IRClasspath]("scalaJSPreLinkClasspath",
        "Completely resolved classpath just after compilation", DTask)

    val scalaJSExecClasspath = TaskKey[CompleteClasspath]("scalaJSExecClasspath",
        "The classpath used for running and testing", DTask)

    val scalaJSLauncher = TaskKey[Attributed[VirtualJSFile]]("scalaJSLauncher",
        "Code used to run. (Attributed with used class name)", DTask)

    val scalaJSConsole = TaskKey[JSConsole]("scalaJSConsole",
        "The JS console used by the Scala.js runner/tester", DTask)

    val preLinkJSEnv = TaskKey[JSEnv]("preLinkJSEnv",
        "The jsEnv used to execute before linking (packaging / optimizing) Scala.js files", BSetting)
    val postLinkJSEnv = TaskKey[JSEnv]("postLinkJSEnv",
        "The jsEnv used to execute after linking (packaging / optimizing) Scala.js files", AMinusSetting)

    val jsEnv = TaskKey[JSEnv]("jsEnv",
        "A JVM-like environment where Scala.js files can be run and tested", DTask)

    val requiresDOM = SettingKey[Boolean]("requiresDOM",
        "Whether this projects needs the DOM. Overrides anything inherited through dependencies.", AMinusSetting)

    val scalaJSTestFramework = SettingKey[String]("scalaJSTestFramework",
        "The Scala.js class that is used as a test framework, for example a class that wraps Jasmine", ASetting)

    val relativeSourceMaps = SettingKey[Boolean]("relativeSourceMaps",
        "Make the referenced paths on source maps relative to target path", BPlusSetting)

    val emitSourceMaps = SettingKey[Boolean]("emitSourceMaps",
        "Whether package and optimize stages should emit source maps at all", BPlusSetting)

    val jsDependencies = SettingKey[Seq[AbstractJSDep]]("jsDependencies",
        "JavaScript libraries this project depends upon. Also used to depend on the DOM.", APlusSetting)

    val scalaJSSemantics = SettingKey[Semantics]("scalaJSSemantics",
        "Configurable semantics of Scala.js.", BPlusSetting)

    val jsDependencyFilter = SettingKey[PartialClasspath.DependencyFilter]("jsDependencyFilter",
        "The filter applied to the raw JavaScript dependencies before execution", CSetting)

    val persistLauncher = SettingKey[Boolean]("persistLauncher",
        "Tell optimize/package tasks to write the laucher file to disk. " +
        "If this is set, your project may only have a single mainClass or you must explicitly set it", AMinusSetting)

    val scalaJSOptimizerOptions = SettingKey[OptimizerOptions]("scalaJSOptimizerOptions",
        "All kinds of options for the Scala.js optimizer stages", DSetting)

    // Task keys to re-wire sources and run with other VM
    val fastOptStage = TaskKey[Unit]("fastOptStage",
        "Run/test stuff after fastOptJS. (type fastOptStage::run)", AMinusTask)
    val fullOptStage = TaskKey[Unit]("fullOptStage",
        "Run/test stuff after fullOptJS. (type fullOptStage::run)", AMinusTask)
  }

  import ScalaJSPluginInternal._

  /** All Scala.js settings */
  val scalaJSSettings: Seq[Setting[_]] = (
      scalaJSAbstractSettings ++
      scalaJSEcosystemSettings
  )

  /** Scala.js build settings: Allows to build Scala.js, but doesn't change
   *  run / test commands
   */
  val scalaJSBuildSettings: Seq[Setting[_]] = (
      scalaJSAbstractBuildSettings ++
      scalaJSEcosystemSettings
  )
}
