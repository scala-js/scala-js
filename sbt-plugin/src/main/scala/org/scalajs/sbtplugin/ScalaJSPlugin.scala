/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package org.scalajs.sbtplugin

import scala.language.implicitConversions

import sbt._
import sbt.Keys._

import org.scalajs.ir.ScalaJSVersions

import org.scalajs.linker.interface._

import org.scalajs.jsenv.{Input, JSEnv}
import org.scalajs.jsenv.nodejs.NodeJSEnv

object ScalaJSPlugin extends AutoPlugin {
  override def requires: Plugins = plugins.JvmPlugin

  object autoImport {
    import KeyRanks._

    /** The current version of the Scala.js sbt plugin and tool chain. */
    val scalaJSVersion = ScalaJSVersions.current

    /** Declares `Tag`s which may be used to limit the concurrency of build
     *  tasks.
     *
     *  For example, the following snippet can be used to limit the
     *  number of linking tasks which are able to run at once:
     *
     *  {{{
     *  Global / concurrentRestrictions += Tags.limit(ScalaJSTags.Link, 2)
     *  }}}
     */
    object ScalaJSTags {
      /** This tag is applied to the [[fastLinkJS]] and [[fullLinkJS]] tasks. */
      val Link = Tags.Tag("scalajs-link")
    }

    // Stage values
    val FastOptStage = Stage.FastOpt
    val FullOptStage = Stage.FullOpt

    // ModuleKind
    val ModuleKind = org.scalajs.linker.interface.ModuleKind

    // All our public-facing keys

    /** A cache box for the IR found on a classpath.
     *
     *  @note
     *    **Unstable API**: this API is subject to backward incompatible
     *    changes in future minor versions of Scala.js.
     */
    val scalaJSIRCacheBox = SettingKey[CacheBox[IRFileCache.Cache]](
        "scalaJSIRCacheBox",
        "Scala.js internal: CacheBox for a cache.", KeyRanks.Invisible)

    /** A cache box for the global IR cache.
     *
     *  @note
     *    **Unstable API**: this API is subject to backward incompatible
     *    changes in future minor versions of Scala.js.
     */
    val scalaJSGlobalIRCacheBox = SettingKey[CacheBox[IRFileCache]](
        "scalaJSGlobalIRCacheBox",
        "Scala.js internal: CacheBox for the global cache.", KeyRanks.Invisible)

    val scalaJSGlobalIRCacheConfig = SettingKey[IRFileCacheConfig](
        "scalaJSGlobalIRCacheConfig",
        "Configuration for the global IR cache.", CSetting)

    val scalaJSGlobalIRCache = TaskKey[IRFileCache](
        "scalaJSGlobalIRCache",
        "Scala.js internal: Access task for a the global IR cache")

    /** Instance of the Scala.js linker.
     *
     *  This task must be scoped per project, configuration, and stage task
     *  (`fastLinkJS` or `fullLinkJS`).
     *
     *  If a task uses the `link` method of the `ClearableLinker`, it must be
     *  protected from running in parallel with any other task doing the same
     *  thing, by tagging the task with the value of [[usesScalaJSLinkerTag]]
     *  in the same scope. The typical shape of such a task will be:
     *  {{{
     *  Compile / fastLinkJS / myTask := Def.taskDyn {
     *    val linker = (Compile / fastLinkJS / scalaJSLinker).value
     *    val usesLinkerTag = (Compile / fastLinkJS / usesScalaJSLinkerTag).value
     *    // Read the `.value` of other settings and tasks here
     *
     *    Def.task {
     *      // Do the actual work of the task here, in particular calling
     *      linker.link(...)
     *    }.tag(usesLinkerTag)
     *  }.value,
     *  }}}
     *
     *  Do not set this value. Instead, set [[scalaJSLinkerImpl]]. This will
     *  automatically set up the correct caching behavior.
     *
     *  @note
     *    **Writing to this key is an unstable API**: the caching contracts
     *    are subject to backward incompatible changes in future minor versions
     *    of Scala.js.
     */
    val scalaJSLinker = TaskKey[ClearableLinker]("scalaJSLinker",
        "Access task for a Scala.js linker. Use this if you want to use the linker.",
        KeyRanks.Invisible)

    /** Implementation of the Scala.js linker to use.
     *
     *  By default, this is reflectively loading the standard linker
     *  implementation. Users may set this to provide custom linker
     *  implementations. In that case, they *must* store the linker impl in
     *  [[scalaJSLinkerImplBox]].
     *
     *  @note
     *    **Unstable API**: this API is subject to backward incompatible
     *    changes in future minor versions of Scala.js.
     */
    val scalaJSLinkerImpl = TaskKey[LinkerImpl]("scalaJSLinkerImpl",
        "Implementation of the Scala.js linker to use: By default, this is " +
        "reflectively loading the standard linker implementation. Users may " +
        "set this to provide custom linker implementations. In that case, " +
        "they *must* store the linker impl in scalaJSLinkerImplBox.",
        KeyRanks.Invisible)

    /** A cache box for the [[scalaJSLinkerImpl]].
     *
     *  @note
     *    **Unstable API**: this API is subject to backward incompatible
     *    changes in future minor versions of Scala.js.
     */
    val scalaJSLinkerImplBox = SettingKey[CacheBox[LinkerImpl]]("scalaJSLinkerImplBox",
        "CacheBox for scalaJSLinkerImpl", KeyRanks.Invisible)

    /** A cache box for [[scalaJSLinker]].
     *
     *  @note
     *    **Unstable API**: this API is subject to backward incompatible
     *    changes in future minor versions of Scala.js.
     */
    val scalaJSLinkerBox = SettingKey[CacheBox[ClearableLinker]]("scalaJSLinkerBox",
        "Scala.js internal: CacheBox for a Scala.js linker", KeyRanks.Invisible)

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
        "Deprecated: Use fastLinkJS instead", KeyRanks.Invisible)

    val fullOptJS = TaskKey[Attributed[File]]("fullOptJS",
        "Deprecated: Use fullLinkJS instead", KeyRanks.Invisible)

    val fastLinkJS = TaskKey[Attributed[Report]]("fastLinkJS",
        "Quickly link all compiled JavaScript", APlusTask)

    val fullLinkJS = TaskKey[Attributed[Report]]("fullLinkJS",
        "Link all compiled JavaScript and fully optimize", APlusTask)

    val testHtml = TaskKey[Attributed[File]]("testHtml",
        "Create an HTML test runner. Honors `scalaJSStage`.", AMinusTask)

    val scalaJSIR = TaskKey[Attributed[Seq[IRFile]]](
        "scalaJSIR", "All the *.sjsir files on the classpath", CTask)

    val scalaJSModuleInitializers = TaskKey[Seq[ModuleInitializer]]("scalaJSModuleInitializers",
        "Module initializers of the Scala.js application, to be called when it starts.",
        AMinusTask)

    val scalaJSModuleInitializersFingerprints = TaskKey[Seq[String]]("scalaJSModuleInitializersFingerprints",
        "An internal task used to track changes to the `scalaJSModuleInitializers` setting",
        KeyRanks.Invisible)

    val scalaJSUseMainModuleInitializer = SettingKey[Boolean]("scalaJSUseMainModuleInitializer",
        "If true, adds the `mainClass` as a module initializer of the Scala.js module",
        APlusSetting)

    val scalaJSUseTestModuleInitializer = SettingKey[Boolean]("scalaJSUseTestModuleInitializer",
        "If true, adds the module initializer required for testing to the Scala.js module",
        BMinusSetting)

    val scalaJSMainModuleInitializer = TaskKey[Option[ModuleInitializer]](
        "scalaJSMainModuleInitializer",
        "The main module initializer, used if " +
        "`scalaJSUseMainModuleInitializer` is true",
        CTask)

    val scalaJSLinkerConfig = SettingKey[StandardConfig](
        "scalaJSLinkerConfig",
        "Configuration of the Scala.js linker",
        BPlusSetting)

    val scalaJSLinkerConfigFingerprint = TaskKey[String]("scalaJSLinkerConfigFingerprint",
        "An internal task used to track changes to the `scalaJSLinkerConfig` setting",
        KeyRanks.Invisible)

    val scalaJSStage = SettingKey[Stage]("scalaJSStage",
        "The optimization stage at which run and test are executed", APlusSetting)

    val scalaJSLinkerResult = TaskKey[Attributed[Report]]("scalaJSLinkerResult",
        "Result of the Scala.js linker. This is the result of fastLinkJS or fullLinkJS, " +
        "depending on the stage.", DTask)

    val scalaJSLinkedFile = TaskKey[Attributed[File]]("scalaJSLinkedFile",
        "Deprecated: Use scalaJSLinkerResult instead", KeyRanks.Invisible)

    val jsEnv = TaskKey[JSEnv]("jsEnv",
        "The JavaScript environment in which to run and test Scala.js applications.",
        AMinusTask)

    /** All Scala.js class names on the fullClasspath, used by scalajsp. */
    val scalaJSClassNamesOnClasspath = TaskKey[Seq[String]]("scalaJSClassNamesOnClasspath",
        "All Scala.js class names on the fullClasspath, used by scalajsp",
        KeyRanks.Invisible)

    /** Prints the content of a .sjsir file in human readable form. */
    val scalajsp = InputKey[Unit]("scalajsp",
        "Prints the content of a .sjsir file in human readable form.",
        CTask)

    val jsEnvInput = TaskKey[Seq[Input]](
        "jsEnvInput",
        "The JSEnv.Inputs to give to the jsEnv for tasks such as `run` and `test`",
        BTask)

    val scalaJSSourceFiles = AttributeKey[Seq[File]]("scalaJSSourceFiles",
        "Files used to compute this value (can be used in FileFunctions later).",
        KeyRanks.Invisible)

    val scalaJSSourceMap = AttributeKey[File]("scalaJSSourceMap",
        "Source map file attached to an Attributed .js file.",
        BSetting)

    val scalaJSModuleKind = AttributeKey[ModuleKind]("scalaJSModuleKind",
        "ModuleKind attached to an Attributed .js file.",
        BSetting)

    val scalaJSTestHTMLArtifactDirectory = SettingKey[File]("scalaJSTestHTMLArtifactDirectory",
        "Directory for artifacts produced by testHtml.",
        BSetting)

    val scalaJSLinkerOutputDirectory = SettingKey[File]("scalaJSLinkerOutputDirectory",
        "Directory for linker output.",
        BSetting)
  }

  import autoImport._

  override def globalSettings: Seq[Setting[_]] = {
    Seq(
        scalaJSStage := Stage.FastOpt,

        scalaJSLinkerConfig := StandardConfig(),

        dependencyResolution in scalaJSLinkerImpl := {
          val log = streams.value.log

          /* We first try to use the dependencyResolution of the root project
           * of this build. In a typical build, this will always have a value.
           * However, if someone does something weird and has a build whose
           * root project does not have the built-in sbt.plugins.IvyPlugin,
           * `dependencyResolution` won't be set, and this will be None.
           */
          val rootDependencyResolution =
            (dependencyResolution in LocalRootProject).?.value

          /* In case the above is None, fall back to something reasonable, and
           * warn.
           */
          rootDependencyResolution.getOrElse {
            log.warn(
                "Falling back on a default `dependencyResolution` to " +
                "resolve the Scala.js linker because `dependencyResolution` " +
                "is not set in the root project of this build.")
            log.warn(
                "Consider explicitly setting " +
                "`Global / scalaJSLinkerImpl / dependencyResolution` " +
                "instead of relying on the default.")

            import sbt.librarymanagement.ivy._
            val ivyConfig = InlineIvyConfiguration()
              .withResolvers(Vector(Resolver.defaultLocal, Resolver.mavenCentral))
              .withLog(log)
            IvyDependencyResolution(ivyConfig)
          }
        },

        scalaJSLinkerImplBox := new CacheBox,

        fullClasspath in scalaJSLinkerImpl := {
          val s = streams.value
          val log = s.log
          val retrieveDir = s.cacheDirectory / "scalajs-linker" / scalaJSVersion
          val lm = (dependencyResolution in scalaJSLinkerImpl).value
          lm.retrieve(
              "org.scala-js" % "scalajs-linker_2.12" % scalaJSVersion,
              scalaModuleInfo = None, retrieveDir, log)
            .fold(w => throw w.resolveException, Attributed.blankSeq(_))
        },

        scalaJSLinkerImpl := {
          val linkerImplClasspath = (fullClasspath in scalaJSLinkerImpl).value
          scalaJSLinkerImplBox.value.ensure {
            LinkerImpl.reflect(Attributed.data(linkerImplClasspath))
          }
        },

        scalaJSGlobalIRCacheConfig := IRFileCacheConfig(),

        scalaJSGlobalIRCacheBox := new CacheBox,

        scalaJSGlobalIRCache := {
          val linkerImpl = scalaJSLinkerImpl.value
          val config = scalaJSGlobalIRCacheConfig.value
          scalaJSGlobalIRCacheBox.value
            .ensure(linkerImpl.irFileCache(config))
        },

        jsEnv := new NodeJSEnv(),

        // Clear the IR cache stats every time a sequence of tasks ends
        onComplete := {
          val prev = onComplete.value
          val globalIRCacheBox = scalaJSGlobalIRCacheBox.value

          { () =>
            prev()
            ScalaJSPluginInternal.closeAllTestAdapters()

            for (irCache <- globalIRCacheBox) {
              sLog.value.debug("Global IR cache stats: " + irCache.stats.logLine)
              irCache.clearStats()
            }
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
