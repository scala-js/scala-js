package org.scalajs.sbtplugin

import scala.annotation.tailrec

import java.io.FileNotFoundException

import java.util.concurrent.atomic.AtomicReference

import sbt._
import sbt.Keys._
import sbt.complete.DefaultParsers._

import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._

import org.scalajs.io.{IO => _, _}
import org.scalajs.io.JSUtils.escapeJS

import org.scalajs.linker._
import org.scalajs.linker.irio._

import org.scalajs.jsenv._

import org.scalajs.ir.Printers.IRTreePrinter

import org.scalajs.testadapter.{TestAdapter, HTMLRunnerBuilder, TestAdapterInitializer}

import Loggers._
import SBTCompat._
import SBTCompat.formatImplicits._
import SBTCompat.formatImplicits.seqFormat

/** Implementation details of `ScalaJSPlugin`. */
private[sbtplugin] object ScalaJSPluginInternal {

  import ScalaJSPlugin.autoImport.{ModuleKind => _, _}
  import ScalaJSPlugin.logIRCacheStats

  /** The global Scala.js IR cache */
  val globalIRCache: IRFileCache = new IRFileCache()

  @tailrec
  final private def registerResource[T <: AnyRef](
      l: AtomicReference[List[T]], r: T): r.type = {
    val prev = l.get()
    if (l.compareAndSet(prev, r :: prev)) r
    else registerResource(l, r)
  }

  private val allocatedIRCaches =
    new AtomicReference[List[IRFileCache.Cache]](Nil)

  /** Allocates a new IR cache linked to the [[globalIRCache]].
   *
   *  The allocated IR cache will automatically be freed when the build is
   *  unloaded.
   */
  private def newIRCache: IRFileCache.Cache =
    registerResource(allocatedIRCaches, globalIRCache.newCache)

  private[sbtplugin] def freeAllIRCaches(): Unit =
    allocatedIRCaches.getAndSet(Nil).foreach(_.free())

  private val createdTestAdapters =
    new AtomicReference[List[TestAdapter]](Nil)

  private def newTestAdapter(jsEnv: JSEnv, input: Input,
      config: TestAdapter.Config): TestAdapter = {
    registerResource(createdTestAdapters, new TestAdapter(jsEnv, input, config))
  }

  private[sbtplugin] def closeAllTestAdapters(): Unit =
    createdTestAdapters.getAndSet(Nil).foreach(_.close())

  /* #2798 -- On Java 9+, the parallel collections on 2.10 die with a
   * `NumberFormatException` and prevent the linker from working.
   *
   * By default, we therefore pre-emptively disable the parallel optimizer in
   * case the parallel collections cannot deal with the current version of
   * Java.
   *
   * TODO This will automatically "fix itself" once we upgrade to sbt 1.x,
   * which uses Scala 2.12. We should get rid of that workaround at that point
   * for tidiness, though.
   */
  val DefaultParallelLinker: Boolean = {
    try {
      scala.util.Properties.isJavaAtLeast("1.8")
      true
    } catch {
      case _: NumberFormatException => false
    }
  }

  private val scalajspParser = {
    loadForParser(sjsirFilesOnClasspath) { (_, relPaths) =>
      val examples = ScalajspUtils.relPathsExamples(relPaths.getOrElse(Nil))
      OptSpace ~> StringBasic.examples(examples)
    }
  }

  /** Patches the IncOptions so that .sjsir files are pruned as needed. */
  def scalaJSPatchIncOptions(incOptions: IncOptions): IncOptions =
    SBTCompat.scalaJSPatchIncOptions(incOptions)

  /** Settings for the production key (e.g. fastOptJS) of a given stage */
  private def scalaJSStageSettings(stage: Stage,
      key: TaskKey[Attributed[File]]): Seq[Setting[_]] = Seq(

      scalaJSLinker in key := {
        val config = (scalaJSLinkerConfig in key).value

        if (config.moduleKind != scalaJSLinkerConfig.value.moduleKind) {
          val projectID = thisProject.value.id
          val configName = configuration.value.name
          val keyName = key.key.label
          sLog.value.warn(
              s"The module kind in `scalaJSLinkerConfig in ($projectID, " +
              s"$configName, $keyName)` is different than the one `in " +
              s"`($projectID, $configName)`. " +
              "Some things will go wrong.")
        }

        StandardLinker.clearable(config)
      },

      // Have `clean` reset the state of the incremental linker
      clean in (This, Zero, This) := {
        val _ = (clean in (This, Zero, This)).value
        (scalaJSLinker in key).value.clear()
        ()
      },

      usesScalaJSLinkerTag in key := {
        val projectPart = thisProject.value.id
        val configPart = configuration.value.name

        val stagePart = stage match {
          case Stage.FastOpt => "fastopt"
          case Stage.FullOpt => "fullopt"
        }

        Tags.Tag(s"uses-scalajs-linker-$projectPart-$configPart-$stagePart")
      },

      // Prevent this linker from being used concurrently
      concurrentRestrictions in Global +=
        Tags.limit((usesScalaJSLinkerTag in key).value, 1),

      key := Def.taskDyn {
        /* It is very important that we evaluate all of those `.value`s from
         * here, and not from within the `Def.task { ... }`, otherwise the
         * relevant dependencies will not show up in `inspect tree`. We use a
         * `Def.taskDyn` only to be able to tag the inner task with a tag that
         * is setting-dependent. But otherwise, the task does not have actually
         * dynamic dependencies, so `inspect tree` is happy with it.
         */
        val s = streams.value
        val irInfo = (scalaJSIR in key).value
        val moduleInitializers = scalaJSModuleInitializers.value
        val output = (artifactPath in key).value
        val linker = (scalaJSLinker in key).value
        val usesLinkerTag = (usesScalaJSLinkerTag in key).value
        val sourceMapFile = new File(output.getPath + ".map")

        Def.task {
          val log = s.log
          val realFiles = irInfo.get(scalaJSSourceFiles).get
          val ir = irInfo.data

          FileFunction.cached(s.cacheDirectory, FilesInfo.lastModified,
              FilesInfo.exists) { _ => // We don't need the files

            val stageName = stage match {
              case Stage.FastOpt => "Fast"
              case Stage.FullOpt => "Full"
            }

            log.info(s"$stageName optimizing $output")

            IO.createDirectory(output.getParentFile)

            def relURI(path: String) = new URI(null, null, path, null)

            val out = LinkerOutput(AtomicWritableFileVirtualBinaryFile(output))
              .withSourceMap(AtomicWritableFileVirtualBinaryFile(sourceMapFile))
              .withSourceMapURI(relURI(sourceMapFile.getName))
              .withJSFileURI(relURI(output.getName))

            linker.link(ir, moduleInitializers, out, sbtLogger2ToolsLogger(log))

            logIRCacheStats(log)

            Set(output, sourceMapFile)
          } (realFiles.toSet)

          Attributed.blank(output).put(scalaJSSourceMap, sourceMapFile)
        }.tag(usesLinkerTag)
      }.value
  )

  val scalaJSConfigSettings: Seq[Setting[_]] = Seq(
      incOptions ~= scalaJSPatchIncOptions
  ) ++ (
      scalaJSStageSettings(Stage.FastOpt, fastOptJS) ++
      scalaJSStageSettings(Stage.FullOpt, fullOptJS)
  ) ++ (
      Seq(fastOptJS, fullOptJS).map { key =>
        moduleName in key := {
          val configSuffix = configuration.value match {
            case Compile => ""
            case config  => "-" + config.name
          }
          moduleName.value + configSuffix
        }
      }
  ) ++ Seq(
      // Note: this cache is not cleared by the sbt's clean task.
      scalaJSIRCache := newIRCache,

      scalaJSIR := {
        val cache = scalaJSIRCache.value
        val classpath = Attributed.data(fullClasspath.value)
        val irContainers = FileScalaJSIRContainer.fromClasspath(classpath)
        val irFiles = cache.cached(irContainers)
        Attributed
          .blank[Seq[VirtualScalaJSIRFile]](irFiles)
          .put(scalaJSSourceFiles, irContainers.map(_.file))
      },

      sjsirFilesOnClasspath := Def.task {
        scalaJSIR.value.data.map(_.relativePath).toSeq
      }.storeAs(sjsirFilesOnClasspath).triggeredBy(scalaJSIR).value,

      scalajsp := {
        val relPath = scalajspParser.parsed

        val vfile = scalaJSIR.value.data
          .find(_.relativePath == relPath)
          .getOrElse(throw new FileNotFoundException(relPath))

        val stdout = new java.io.PrintWriter(System.out)
        new IRTreePrinter(stdout).print(vfile.tree)
        stdout.flush()

        logIRCacheStats(streams.value.log)
      },

      artifactPath in fastOptJS :=
        ((crossTarget in fastOptJS).value /
            ((moduleName in fastOptJS).value + "-fastopt.js")),

      artifactPath in fullOptJS :=
        ((crossTarget in fullOptJS).value /
            ((moduleName in fullOptJS).value + "-opt.js")),

      scalaJSLinkerConfig in fullOptJS ~= { prevConfig =>
        prevConfig
          .withSemantics(_.optimized)
          .withClosureCompiler(!prevConfig.esFeatures.useECMAScript2015)
      },

      scalaJSLinkedFile := Def.settingDyn {
        scalaJSStage.value match {
          case Stage.FastOpt => fastOptJS
          case Stage.FullOpt => fullOptJS
        }
      }.value,

      console := console.dependsOn(Def.task {
        streams.value.log.warn("Scala REPL doesn't work with Scala.js. You " +
            "are running a JVM REPL. JavaScript things won't work.")
      }).value,

      /* Do not inherit jsExecutionFiles from the parent configuration.
       * Instead, always derive them straight from the Zero configuration
       * scope.
       */
      jsExecutionFiles := (jsExecutionFiles in (This, Zero, This)).value,

      // Add the Scala.js linked file to the JS files (by default, the only one)
      jsExecutionFiles +=
        new FileVirtualBinaryFile(scalaJSLinkedFile.value.data),

      scalaJSMainModuleInitializer := {
        mainClass.value.map { mainCl =>
          ModuleInitializer.mainMethodWithArgs(mainCl, "main")
        }
      },

      /* Do not inherit scalaJSModuleInitializers from the parent configuration.
       * Instead, always derive them straight from the Zero configuration
       * scope.
       */
      scalaJSModuleInitializers :=
        (scalaJSModuleInitializers in (This, Zero, This)).value,

      scalaJSModuleInitializers ++= {
        if (scalaJSUseMainModuleInitializer.value) {
          Seq(scalaJSMainModuleInitializer.value.getOrElse {
            throw new MessageOnlyException(
                "No main module initializer was specified (possibly because " +
                "no or multiple main classes were found), but " +
                "scalaJSUseMainModuleInitializer was set to true. " +
                "You can explicitly specify it either with " +
                "`mainClass := Some(...)` or with " +
                "`scalaJSMainModuleInitializer := Some(...)`")
          })
        } else {
          Seq.empty
        }
      },

      run := {
        if (!scalaJSUseMainModuleInitializer.value) {
          throw new MessageOnlyException("`run` is only supported with " +
              "scalaJSUseMainModuleInitializer := true")
        }

        val log = streams.value.log
        val env = jsEnv.value

        val className = mainClass.value.getOrElse("<unknown class>")
        log.info(s"Running $className. Hit any key to interrupt.")
        log.debug(s"with JSEnv ${env.name}")

        val input = Input.ScriptsToLoad(jsExecutionFiles.value.toList)
        val config = RunConfig().withLogger(sbtLogger2ToolsLogger(log))

        Run.runInterruptible(env, input, config)
      },

      runMain := {
        throw new MessageOnlyException("`runMain` is not supported in Scala.js")
      }
  )

  val scalaJSCompileSettings: Seq[Setting[_]] = (
      scalaJSConfigSettings
  )

  val scalaJSTestSettings: Seq[Setting[_]] = (
      scalaJSConfigSettings
  ) ++ Seq(
      /* Always default to false for scalaJSUseMainModuleInitializer in testing
       * configurations, even if it is true in the Global configuration scope.
       */
      scalaJSUseMainModuleInitializer := false,

      // Use test module initializer by default.
      scalaJSUseTestModuleInitializer := true,

      scalaJSModuleInitializers ++= {
        val useMain = scalaJSUseMainModuleInitializer.value
        val useTest = scalaJSUseTestModuleInitializer.value
        val configName = configuration.value.name

        if (useTest) {
          if (useMain) {
            throw new MessageOnlyException("You may only set one of " +
                s"`scalaJSUseMainModuleInitializer in $configName` and " +
                s"`scalaJSUseTestModuleInitializer in $configName` to true")
          }

          Seq(
              ModuleInitializer.mainMethod(
                  TestAdapterInitializer.ModuleClassName,
                  TestAdapterInitializer.MainMethodName)
          )
        } else {
          Seq.empty
        }
      },

      loadedTestFrameworks := {
        val configName = configuration.value.name

        if (fork.value) {
          throw new MessageOnlyException(
              s"`test in $configName` tasks in a Scala.js project require " +
              s"`fork in $configName := false`.")
        }

        if (!scalaJSUseTestModuleInitializer.value) {
          throw new MessageOnlyException(
              s"You may only use `test in $configName` tasks in " +
              "a Scala.js project if `scalaJSUseTestModuleInitializer in " +
              s"$configName := true`")
        }

        val frameworks = testFrameworks.value
        val env = jsEnv.value
        val input = Input.ScriptsToLoad(jsExecutionFiles.value.toList)
        val frameworkNames = frameworks.map(_.implClassNames.toList).toList

        val logger = sbtLogger2ToolsLogger(streams.value.log)
        val config = TestAdapter.Config()
          .withLogger(logger)

        val adapter = newTestAdapter(env, input, config)
        val frameworkAdapters = adapter.loadFrameworks(frameworkNames)

        frameworks.zip(frameworkAdapters).collect {
          case (tf, Some(adapter)) => (tf, adapter)
        }.toMap
      },

      // Override default to avoid triggering a test:fastOptJS in a test:compile
      // without loosing autocompletion.
      definedTestNames := {
        definedTests.map(_.map(_.name).distinct)
          .storeAs(definedTestNames).triggeredBy(loadedTestFrameworks).value
      },

      artifactPath in testHtml := {
        val stageSuffix = scalaJSStage.value match {
          case Stage.FastOpt => "fastopt"
          case Stage.FullOpt => "opt"
        }
        val config = configuration.value.name
        ((crossTarget in testHtml).value /
            ((moduleName in testHtml).value + s"-$stageSuffix-$config.html"))
      },

      testHtml := {
        val log = streams.value.log
        val output = (artifactPath in testHtml).value
        val title = name.value + " - tests"
        val jsFiles = (jsExecutionFiles in testHtml).value

        val frameworks = (loadedTestFrameworks in testHtml).value.toList
        val frameworkImplClassNames =
          frameworks.map(_._1.implClassNames.toList)

        val taskDefs = for (td <- (definedTests in testHtml).value) yield {
          new sbt.testing.TaskDef(td.name, td.fingerprint,
              td.explicitlySpecified, td.selectors)
        }

        HTMLRunnerBuilder.writeToFile(output, title, jsFiles,
            frameworkImplClassNames, taskDefs.toList)

        log.info(s"Wrote HTML test runner. Point your browser to ${output.toURI}")

        Attributed.blank(output)
      }
  )

  private val scalaJSProjectBaseSettings = Seq(
      platformDepsCrossVersion := ScalaJSCrossVersion.binary,

      scalaJSModuleInitializers := Seq(),
      scalaJSUseMainModuleInitializer := false,

      // you will need the Scala.js compiler plugin
      addCompilerPlugin(
          "org.scala-js" % "scalajs-compiler" % scalaJSVersion cross CrossVersion.full),

      libraryDependencies ++= Seq(
          // and of course the Scala.js library
          "org.scala-js" %% "scalajs-library" % scalaJSVersion,
          // also bump the version of the test-interface
          "org.scala-js" %% "scalajs-test-interface" % scalaJSVersion % "test"
      ),

      // and you will want to be cross-compiled on the Scala.js binary version
      crossVersion := ScalaJSCrossVersion.binary
  )

  val scalaJSProjectSettings: Seq[Setting[_]] = (
      scalaJSProjectBaseSettings ++
      inConfig(Compile)(scalaJSCompileSettings) ++
      inConfig(Test)(scalaJSTestSettings)
  )
}
