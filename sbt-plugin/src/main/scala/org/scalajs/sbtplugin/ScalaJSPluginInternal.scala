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

import scala.annotation.tailrec

// Import Future explicitly to avoid collision with sbt.Future.
import scala.concurrent.{Future, _}
import scala.concurrent.duration._

import scala.util.{Failure, Success}

import java.util.concurrent.atomic.AtomicReference

import sbt._
import sbt.Keys._
import sbt.complete.DefaultParsers._

import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._

import org.scalajs.linker._
import org.scalajs.linker.standard.IRFileImpl

import org.scalajs.jsenv._

import org.scalajs.ir.{Definitions, IRVersionNotSupportedException}
import org.scalajs.ir.Printers.IRTreePrinter

import org.scalajs.testing.adapter.{TestAdapter, HTMLRunnerBuilder, TestAdapterInitializer}

import Loggers._

import sjsonnew.BasicJsonProtocol._
import sjsonnew.BasicJsonProtocol.seqFormat

/** Implementation details of `ScalaJSPlugin`. */
private[sbtplugin] object ScalaJSPluginInternal {

  import ScalaJSPlugin.autoImport.{ModuleKind => _, _}
  import ScalaJSPlugin.logIRCacheStats

  /** The global Scala.js IR cache */
  val globalIRCache: IRFileCache = IRFileCache()

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

  private def newTestAdapter(jsEnv: JSEnv, input: Seq[Input],
      config: TestAdapter.Config): TestAdapter = {
    registerResource(createdTestAdapters, new TestAdapter(jsEnv, input, config))
  }

  private[sbtplugin] def closeAllTestAdapters(): Unit =
    createdTestAdapters.getAndSet(Nil).foreach(_.close())

  private def enhanceIRVersionNotSupportedException[A](body: => A): A = {
    try {
      body
    } catch {
      case e: IRVersionNotSupportedException =>
        throw new IRVersionNotSupportedException(e.version, e.supported,
            s"${e.getMessage}\nYou may need to upgrade the Scala.js sbt " +
            s"plugin to version ${e.version} or later.",
            e)
    }
  }

  private def await[T](log: Logger)(body: ExecutionContext => Future[T]): T = {
    val ec = ExecutionContext.fromExecutor(
        ExecutionContext.global, t => log.trace(t))

    Await.result(body(ec), Duration.Inf)
  }

  private val scalajspParser = {
    loadForParser(scalaJSClassNamesOnClasspath) { (_, names) =>
      val examples = sbt.complete.FixedSetExamples(names.getOrElse(Nil))
      OptSpace ~> StringBasic.examples(examples)
    }
  }

  /** Patches the IncOptions so that .sjsir files are pruned, backed up and
   *  restored as needed.
   */
  def scalaJSPatchIncOptions(incOptions: IncOptions): IncOptions = {
    val sjsirFileManager = new SJSIRFileManager
    val newExternalHooks =
      incOptions.externalHooks.withExternalClassFileManager(sjsirFileManager)
    incOptions.withExternalHooks(newExternalHooks)
  }

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

            val out = LinkerOutput(LinkerOutput.newAtomicPathFile(output.toPath))
              .withSourceMap(LinkerOutput.newAtomicPathFile(sourceMapFile.toPath))
              .withSourceMapURI(relURI(sourceMapFile.getName))
              .withJSFileURI(relURI(output.getName))

            try {
              enhanceIRVersionNotSupportedException {
                val tlog = sbtLogger2ToolsLogger(log)
                await(log)(linker.link(ir, moduleInitializers, out, tlog)(_))
              }
            } catch {
              case e: LinkingException =>
                throw new MessageOnlyException(e.getMessage)
            }

            logIRCacheStats(log)

            Set(output, sourceMapFile)
          } (realFiles.toSet)

          Attributed.blank(output).put(scalaJSSourceMap, sourceMapFile)
        }.tag(usesLinkerTag, ScalaJSTags.Link)
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
        val log = streams.value.log
        val tlog = sbtLogger2ToolsLogger(log)

        val (irFiles, paths) = enhanceIRVersionNotSupportedException {
          tlog.time("Update IR cache") {
            await(log) { eci =>
              implicit val ec = eci
              for {
                (irContainers, paths) <- IRContainer.fromPathClasspath(classpath.map(_.toPath))
                irFiles <- cache.cached(irContainers)
              } yield (irFiles, paths)
            }
          }
        }

        Attributed
          .blank[Seq[IRFile]](irFiles)
          .put(scalaJSSourceFiles, paths.map(_.toFile))
      },

      scalaJSClassNamesOnClasspath := Def.task {
        val none = Future.successful(None)

        await(streams.value.log) { eci =>
          implicit val ec = eci
          Future.traverse(scalaJSIR.value.data) { ir =>
            IRFileImpl.fromIRFile(ir)
              .entryPointsInfo
              .map(i => Some(Definitions.decodeClassName(i.encodedName)))
              .fallbackTo(none)
          }
        }.flatten
      }.storeAs(scalaJSClassNamesOnClasspath).triggeredBy(scalaJSIR).value,

      scalajsp := {
        val name = scalajspParser.parsed

        enhanceIRVersionNotSupportedException {
          val log = streams.value.log
          val stdout = new java.io.PrintWriter(System.out)
          val tree = await(log) { eci =>
            implicit val ec = eci
            Future.traverse(scalaJSIR.value.data) { irFile =>
              val ir = IRFileImpl.fromIRFile(irFile)
              ir.entryPointsInfo.map { i =>
                if (i.encodedName == name || Definitions.decodeClassName(i.encodedName) == name) Success(Some(ir))
                else Success(None)
              }.recover { case t => Failure(t) }
            }.flatMap { irs =>
              irs.collectFirst {
                case Success(Some(f)) => f.tree
              }.getOrElse {
                val t = new MessageOnlyException(s"class $name not found on classpath")
                irs.collect { case Failure(st) => t.addSuppressed(st) }
                throw t
              }
            }
          }

          new IRTreePrinter(stdout).print(tree)
          stdout.flush()
        }

        logIRCacheStats(streams.value.log)
      },

      artifactPath in fastOptJS :=
        ((crossTarget in fastOptJS).value /
            ((moduleName in fastOptJS).value + "-fastopt.js")),

      artifactPath in fullOptJS :=
        ((crossTarget in fullOptJS).value /
            ((moduleName in fullOptJS).value + "-opt.js")),

      scalaJSLinkerConfig in fullOptJS ~= { prevConfig =>
        val useClosure = prevConfig.moduleKind != ModuleKind.ESModule
        prevConfig
          .withSemantics(_.optimized)
          .withClosureCompiler(useClosure)
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

      // Use the Scala.js linked file as the default Input for the JSEnv
      jsEnvInput := {
        val linkedFile = scalaJSLinkedFile.value.data.toPath
        val input = scalaJSLinkerConfig.value.moduleKind match {
          case ModuleKind.NoModule       => Input.Script(linkedFile)
          case ModuleKind.ESModule       => Input.ESModule(linkedFile)
          case ModuleKind.CommonJSModule => Input.CommonJSModule(linkedFile)
        }
        List(input)
      },

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
        val mainClasses = discoveredMainClasses.value
        if (scalaJSUseMainModuleInitializer.value) {
          Seq(scalaJSMainModuleInitializer.value.getOrElse {
            if (mainClasses.isEmpty) {
              throw new MessageOnlyException(
                  "No main module initializer was specified, but " +
                  "scalaJSUseMainModuleInitializer was set to true. " +
                  "You can explicitly specify it either with " +
                  "`mainClass := Some(...)` or with " +
                  "`scalaJSMainModuleInitializer := Some(...)`")
            } else {
              throw new MessageOnlyException(
                  s"Multiple main classes (${mainClasses.mkString(", ")}) " +
                  "were found. " +
                  "You can explicitly specify the one you want with " +
                  "`mainClass := Some(...)` or with " +
                  "`scalaJSMainModuleInitializer := Some(...)`")
            }
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

        val input = jsEnvInput.value
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
        val input = jsEnvInput.value
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
        val input = (jsEnvInput in testHtml).value

        val frameworks = (loadedTestFrameworks in testHtml).value.toList
        val frameworkImplClassNames =
          frameworks.map(_._1.implClassNames.toList)

        val taskDefs = for (td <- (definedTests in testHtml).value) yield {
          new sbt.testing.TaskDef(td.name, td.fingerprint,
              td.explicitlySpecified, td.selectors)
        }

        HTMLRunnerBuilder.writeToFile(output, title, input,
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
          // as well as the test-bridge in the Test configuration
          "org.scala-js" %% "scalajs-test-bridge" % scalaJSVersion % "test"
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
