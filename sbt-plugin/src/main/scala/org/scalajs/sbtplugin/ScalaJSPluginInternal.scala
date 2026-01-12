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

/*
  scalafmt: {
    newlines.configStyle.fallBack.prefer = false
    runner.optimizer.callSite.minCount = 1000
  }
 */

package org.scalajs.sbtplugin

import scala.annotation.tailrec

// Import Future explicitly to avoid collision with sbt.Future.
import scala.concurrent.{Future, _}
import scala.concurrent.duration._

import scala.util.{Failure, Success}
import scala.util.control.NonFatal

import java.io.{InputStream, OutputStream}
import java.util.concurrent.atomic.AtomicReference

import sbt._
import sbt.Keys._
import sbt.complete.DefaultParsers._

import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._

import org.scalajs.linker.interface._
import org.scalajs.linker.interface.unstable.IRFileImpl

import org.scalajs.jsenv._

import org.scalajs.ir.IRVersionNotSupportedException
import org.scalajs.ir.Printers.IRTreePrinter

import org.scalajs.testing.adapter.{TestAdapter, HTMLRunnerBuilder, TestAdapterInitializer}

import sjsonnew.BasicJsonProtocol._

/** Implementation details of `ScalaJSPlugin`. */
private[sbtplugin] object ScalaJSPluginInternal {

  import ScalaJSPlugin.autoImport.{ModuleKind => _, _}

  @tailrec
  final private def registerResource[T <: AnyRef](
      l: AtomicReference[List[T]], r: T): r.type = {
    val prev = l.get()
    if (l.compareAndSet(prev, r :: prev)) r
    else registerResource(l, r)
  }

  private val allocatedIRCaches =
    new AtomicReference[List[IRFileCache.Cache]](Nil)

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

  private object FailedToStartCmd {
    def unapply(t: Throwable): Option[String] = {
      val causeChain = Iterator
        .iterate(t)(_.getCause())
        .takeWhile(_ != null)

      causeChain.collectFirst {
        case ExternalJSRun.FailedToStartException(cmd :: _, _) =>
          cmd
      }
    }
  }

  private def enhanceNotInstalledException[A](skey: ScopedKey[_], log: Logger)(body: => A): A = {
    try {
      body
    } catch {
      case NonFatal(t @ FailedToStartCmd(cmd)) =>
        // trace the original failure in case there is another problem.
        log.debug(StackTrace.trimmed(t, 0))

        val keyStr = Scope.display(skey.scope, skey.key.label)
        throw new MessageOnlyException(
            s"failed to start $cmd; did you install it? (run `last $keyStr` for the full stack trace)")
    }
  }

  private def linkerOutputDirectory(v: Attributed[Report], scope: Scope, key: TaskKey[_]): File = {
    v.get(scalaJSLinkerOutputDirectory.key).getOrElse {
      val keyStr = Scope.display(scope, key.key.label)
      throw new MessageOnlyException(
          s"The linking report produced by $keyStr was not attributed with an output directory. " +
          "Please report this as a Scala.js bug.")
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

  /** Settings for the production key (e.g. fastLinkJS) of a given stage */
  private def scalaJSStageSettings(stage: Stage,
      key: TaskKey[Attributed[Report]], outputKey: TaskKey[File],
      legacyKey: TaskKey[Attributed[File]]): Seq[Setting[_]] = Seq(

      key / scalaJSLinkerBox := new CacheBox,

      legacyKey / scalaJSLinker := {
        val config = (key / scalaJSLinkerConfig).value
        val box = (key / scalaJSLinkerBox).value
        val linkerImpl = (key / scalaJSLinkerImpl).value

        box.ensure(linkerImpl.clearableLinker(config))
      },

      key / scalaJSLinker := (legacyKey / scalaJSLinker).value,

      // Have `clean` reset the state of the incremental linker
      clean in Scope(This, Zero, This, This) := {
        val _ = (clean in Scope(This, Zero, This, This)).value
        (key / scalaJSLinkerBox).value.foreach(_.clear())
        ()
      },

      legacyKey / usesScalaJSLinkerTag := {
        val projectPart = thisProject.value.id
        val configPart = configuration.value.name

        val stagePart = stage match {
          case Stage.FastOpt => "fastopt"
          case Stage.FullOpt => "fullopt"
        }

        Tags.Tag(s"uses-scalajs-linker-$projectPart-$configPart-$stagePart")
      },

      key / usesScalaJSLinkerTag := (legacyKey / usesScalaJSLinkerTag).value,

      // Prevent this linker from being used concurrently
      concurrentRestrictions in Global +=
        Tags.limit((key / usesScalaJSLinkerTag).value, 1),

      key / scalaJSModuleInitializersFingerprints :=
        scalaJSModuleInitializers.value.map(ModuleInitializer.fingerprint),

      key / scalaJSLinkerConfigFingerprint :=
        StandardConfig.fingerprint((key / scalaJSLinkerConfig).value),

      key / moduleName := (legacyKey / moduleName).value,

      key / scalaJSLinkerConfig := (legacyKey / scalaJSLinkerConfig).value,

      key := Def.taskDyn {
        /* It is very important that we evaluate all of those `.value`s from
         * here, and not from within the `Def.task { ... }`, otherwise the
         * relevant dependencies will not show up in `inspect tree`. We use a
         * `Def.taskDyn` only to be able to tag the inner task with a tag that
         * is setting-dependent. But otherwise, the task does not have actually
         * dynamic dependencies, so `inspect tree` is happy with it.
         */
        val s = streams.value
        val irInfo = (key / scalaJSIR).value
        val moduleInitializers = scalaJSModuleInitializers.value
        val reportFile = s.cacheDirectory / "linking-report.bin"
        val outputDir = (key / scalaJSLinkerOutputDirectory).value
        val linker = (key / scalaJSLinker).value
        val linkerImpl = (key / scalaJSLinkerImpl).value
        val usesLinkerTag = (key / usesScalaJSLinkerTag).value

        val configChanged = {
          def moduleInitializersChanged = (key / scalaJSModuleInitializersFingerprints)
            .previous
            .exists(_ != (key / scalaJSModuleInitializersFingerprints).value)

          def linkerConfigChanged = (key / scalaJSLinkerConfigFingerprint)
            .previous
            .exists(_ != (key / scalaJSLinkerConfigFingerprint).value)

          moduleInitializersChanged || linkerConfigChanged
        }

        def reportIncompatible =
          Report.deserialize(IO.readBytes(reportFile)).isEmpty

        if (reportFile.exists() && (configChanged || reportIncompatible)) {
          reportFile.delete() // triggers re-linking through FileFunction.cached
        }

        Def.task {
          val log = s.log
          val tlog = scalaJSLoggerFactory.value(log)

          val realFiles = irInfo.get(scalaJSSourceFiles).get
          val ir = irInfo.data

          FileFunction.cached(s.cacheDirectory, FilesInfo.lastModified,
              FilesInfo.exists) { _ => // We don't need the files

            val stageName = stage match {
              case Stage.FastOpt => "Fast"
              case Stage.FullOpt => "Full"
            }

            log.info(s"$stageName optimizing $outputDir")

            IO.createDirectory(outputDir)

            val out = linkerImpl.outputDirectory(outputDir.toPath)

            val report = try {
              enhanceIRVersionNotSupportedException {
                await(log)(linker.link(ir, moduleInitializers, out, tlog)(_))
              }
            } catch {
              case e: LinkingException =>
                throw new MessageOnlyException(e.getMessage)
            }

            IO.write(reportFile, Report.serialize(report))

            IO.listFiles(outputDir).toSet + reportFile
          } (realFiles.toSet)

          val report = Report.deserialize(IO.readBytes(reportFile)).getOrElse {
            throw new MessageOnlyException(
                "Failed to deserialize report after linking. " +
                "Please report this as a Scala.js bug.")
          }

          Attributed.blank(report)
            .put(scalaJSLinkerOutputDirectory.key, outputDir)
        }.tag(usesLinkerTag, ScalaJSTags.Link)
      }.value,

      outputKey := linkerOutputDirectory(key.value, resolvedScoped.value.scope, key),

      legacyKey := {
        val linkerImpl = (key / scalaJSLinkerImpl).value
        val report = key.value.data
        val linkerOutputDir = outputKey.value

        val outDir = linkerImpl.outputDirectory(linkerOutputDir.toPath())

        val outputJSFile = (legacyKey / artifactPath).value
        val outputSourceMapFile = new File(outputJSFile.getPath + ".map")

        IO.createDirectory(outputJSFile.getParentFile)

        // Dummy class to silence deprecation warnings.
        abstract class Converter {
          def convert(): Unit
        }

        object Converter extends Converter {
          @deprecated("Deprecate to silence warnings", "never/always")
          def convert(): Unit = {
            val legacyOutput = {
              def relURI(path: String) = new URI(null, null, path, null)
              LinkerOutput(linkerImpl.outputFile(outputJSFile.toPath()))
                .withSourceMap(linkerImpl.outputFile(outputSourceMapFile.toPath()))
                .withSourceMapURI(relURI(outputSourceMapFile.getName()))
                .withJSFileURI(relURI(outputJSFile.getName()))
            }

            await(streams.value.log) { eci =>
              implicit val ec = eci
              ReportToLinkerOutputAdapter.convert(report, outDir, legacyOutput).recover {
                case e: ReportToLinkerOutputAdapter.UnsupportedLinkerOutputException =>
                  throw new MessageOnlyException(
                      "The linker produced a result not supported by the legacy " +
                      s"task ${legacyKey.key}. Did you mean to invoke ${key.key} " +
                      "instead? " + e.getMessage())
              }
            }
          }
        }

        (Converter: Converter).convert()

        /* We always need to supply a module kind, but if we do not have an
         * output module, we do not know the module kind.
         * Therefore, we do what we used to do in the older implementation: We
         * take it from the config itself.
         */
        val linkerConfig = (legacyKey / scalaJSLinkerConfig).value
        val moduleKind = report.publicModules.headOption
          .fold(linkerConfig.moduleKind)(_.moduleKind)

        Attributed.blank(outputJSFile)
          .put(scalaJSSourceMap, outputSourceMapFile)
          .put(scalaJSModuleKind, moduleKind)
      }
  )

  val scalaJSConfigSettings: Seq[Setting[_]] = Seq(
      incOptions ~= scalaJSPatchIncOptions
  ) ++ (
      scalaJSStageSettings(Stage.FastOpt, fastLinkJS, fastLinkJSOutput, fastOptJS) ++
      scalaJSStageSettings(Stage.FullOpt, fullLinkJS, fullLinkJSOutput, fullOptJS)
  ) ++ (
      Seq(fastOptJS, fullOptJS).map { key =>
        key / moduleName := {
          val configSuffix = configuration.value match {
            case Compile => ""
            case config  => "-" + config.name
          }
          moduleName.value + configSuffix
        }
      }
  ) ++ Seq(
      // Note: this cache is not cleared by the sbt's clean task.
      scalaJSIRCacheBox := new CacheBox,

      scalaJSIR := {
        val linkerImpl = (scalaJSIR / scalaJSLinkerImpl).value
        val globalIRCache = (scalaJSIR / scalaJSGlobalIRCache).value

        val cache = scalaJSIRCacheBox.value
          .ensure(registerResource(allocatedIRCaches, globalIRCache.newCache))

        val classpath = Attributed.data(fullClasspath.value)
        val log = streams.value.log
        val tlog = scalaJSLoggerFactory.value(log)
        val config = configuration.value.name

        /* #4610 Warn if `-Xplugin:scalajs-compiler.jar` (Scala 2) or
         * `-scalajs` (Scala 3) is missing from the `scalacOptions`.
         * This feature is not automatically tested.
         */
        def warnMissingScalacOption(thingMissing: String): Unit = {
          log.warn(
              s"$thingMissing was missing from `$config / scalacOptions`, but it is required to produce Scala.js IR.")
          log.warn("Linking, running and/or testing will probably go wrong.")
          log.warn("The most likely cause is that you used `scalacOptions := ...` instead of using `++=`.")
        }
        val scalacOpts = scalacOptions.value
        if (scalaVersion.value.startsWith("2.")) {
          if (!scalacOpts.exists(opt => opt.startsWith("-Xplugin:") && opt.contains("scalajs-compiler")))
            warnMissingScalacOption("The `scalajs-compiler.jar` compiler plugin")
        } else {
          if (!scalacOpts.contains("-scalajs"))
            warnMissingScalacOption("The `-scalajs` flag")
        }

        val (irFiles, paths) = enhanceIRVersionNotSupportedException {
          tlog.time("Update IR cache") {
            await(log) { eci =>
              implicit val ec = eci
              for {
                (irContainers, paths) <- linkerImpl.irContainers(classpath.map(_.toPath))
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
              .map(i => Some(i.className.nameString))
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
                if (i.className.nameString == name) Success(Some(ir))
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
      },

      fastLinkJS / scalaJSLinkerOutputDirectory :=
        ((fastLinkJS / crossTarget).value /
            ((fastLinkJS / moduleName).value + "-fastopt")),

      fullLinkJS / scalaJSLinkerOutputDirectory :=
        ((fullLinkJS / crossTarget).value /
            ((fullLinkJS / moduleName).value + "-opt")),

      fastOptJS / artifactPath :=
        ((fastOptJS / crossTarget).value /
            ((fastOptJS / moduleName).value + "-fastopt.js")),

      fullOptJS / artifactPath :=
        ((fullOptJS / crossTarget).value /
            ((fullOptJS / moduleName).value + "-opt.js")),

      fullOptJS / scalaJSLinkerConfig ~= { prevConfig =>
        prevConfig
          .withSemantics(_.optimized)
          .withMinify(true)
          .withCheckIR(true) // for safety, fullOpt is slow anyways.
      },

      scalaJSLinkerResult := Def.settingDyn {
        scalaJSStage.value match {
          case Stage.FastOpt => fastLinkJS
          case Stage.FullOpt => fullLinkJS
        }
      }.value,

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

      /* Do not inherit jsEnvInput from the parent configuration.
       * Instead, always derive it straight from the Zero configuration scope.
       */
      jsEnvInput := (jsEnvInput in Scope(This, Zero, This, This)).value,

      // Add the Scala.js linked file to the Input for the JSEnv.
      jsEnvInput += {
        val linkingResult = scalaJSLinkerResult.value

        val report = linkingResult.data

        val mainModule = report.publicModules.find(_.moduleID == "main").getOrElse {
          throw new MessageOnlyException(
              "Cannot determine `jsEnvInput`: Linking result does not have a " +
              "module named `main`. Set jsEnvInput manually?\n" +
              s"Full report:\n$report")
        }

        val linkerOutputDir =
          linkerOutputDirectory(linkingResult, resolvedScoped.value.scope, scalaJSLinkerResult)
        val path = (linkerOutputDir / mainModule.jsFileName).toPath

        mainModule.moduleKind match {
          case ModuleKind.NoModule       => Input.Script(path)
          case ModuleKind.ESModule       => Input.ESModule(path)
          case ModuleKind.CommonJSModule => Input.CommonJSModule(path)
        }
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
        (scalaJSModuleInitializers in Scope(This, Zero, This, This)).value,

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
        log.info(s"Running $className.")
        log.debug(s"with JSEnv ${env.name}")

        val input = jsEnvInput.value

        /* The list of threads that are piping output to System.out and
         * System.err. This is not an AtomicReference or any other thread-safe
         * structure because:
         * - `onOutputStream` is guaranteed to be called exactly once, and
         * - `pipeOutputThreads` is only read once the run is completed
         *   (although the JSEnv interface does not explicitly specify that the
         *   call to `onOutputStream must happen before that, anything else is
         *   just plain unreasonable).
         * We only mark it as `@volatile` to ensure that there is an
         * appropriate memory barrier between writing to it and reading it back.
         */
        @volatile var pipeOutputThreads: List[Thread] = Nil

        /* #4560 Explicitly redirect out/err to System.out/System.err, instead
         * of relying on `inheritOut` and `inheritErr`, so that streams
         * installed with `System.setOut` and `System.setErr` are always taken
         * into account. sbt installs such alternative outputs when it runs in
         * server mode.
         */
        val config = RunConfig()
          .withLogger(scalaJSLoggerFactory.value(log))
          .withEnv((run / envVars).value)
          .withInheritOut(false)
          .withInheritErr(false)
          .withOnOutputStream { (out, err) =>
            pipeOutputThreads = (
              out.map(PipeOutputThread.start(_, System.out)).toList :::
              err.map(PipeOutputThread.start(_, System.err)).toList
            )
          }

        try {
          val run = env.start(input, config)

          enhanceNotInstalledException(resolvedScoped.value, log) {
            Await.result(run.future, Duration.Inf)
          }
        } finally {
          /* Wait for the pipe output threads to be done, to make sure that we
           * do not finish the `run` task before *all* output has been
           * transferred to System.out and System.err.
           * We do that in a `finally` block so that the stdout and stderr
           * streams are propagated even if the run finishes with a failure.
           * `join()` itself does not throw except if the current thread is
           * interrupted, which is not supposed to happen (if it does happen,
           * the interrupted exception will shadow any error from the run).
           */
          for (pipeOutputThread <- pipeOutputThreads)
            pipeOutputThread.join()
        }
      },

      runMain := {
        throw new MessageOnlyException("`runMain` is not supported in Scala.js")
      }
  )

  val scalaJSCompileSettings: Seq[Setting[_]] = (
      scalaJSConfigSettings
  )

  val scalaJSTestSettings: Seq[Setting[_]] = scalaJSConfigSettings ++ Seq(
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
                s"`$configName / scalaJSUseMainModuleInitializer` " +
                s"`$configName / scalaJSUseTestModuleInitializer` true")
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
        val input = jsEnvInput.value

        if (fork.value) {
          throw new MessageOnlyException(
              s"`$configName / test` tasks in a Scala.js project require " +
              s"`$configName / fork := false`.")
        }

        if (!scalaJSUseTestModuleInitializer.value) {
          throw new MessageOnlyException(
              s"You may only use `$configName / test` tasks in a Scala.js project if " +
              s"`$configName / scalaJSUseTestModuleInitializer := true`.")
        }

        if (input.isEmpty) {
          throw new MessageOnlyException(
              s"`$configName / test` got called but `$configName / jsEnvInput` is empty. " +
              "This is not allowed, since running tests requires the generated Scala.js code. " +
              s"If you want to call `$configName / test` but not have it do anything, " +
              s"set `$configName / test` := {}`.")
        }

        val frameworks = testFrameworks.value
        val env = jsEnv.value
        val frameworkNames = frameworks.map(_.implClassNames.toList).toList

        val log = streams.value.log
        val config = TestAdapter.Config()
          .withLogger(scalaJSLoggerFactory.value(log))
          .withEnv(envVars.value)

        val adapter = newTestAdapter(env, input, config)
        val frameworkAdapters = enhanceNotInstalledException(resolvedScoped.value, log) {
          adapter.loadFrameworks(frameworkNames)
        }

        frameworks.zip(frameworkAdapters).collect {
          case (tf, Some(adapter)) => (tf, adapter)
        }.toMap
      },

      // Override default to avoid triggering a test:fastLinkJS in a test:compile
      // without losing autocompletion.
      definedTestNames := {
        definedTests.map(_.map(_.name).distinct)
          .storeAs(definedTestNames).triggeredBy(loadedTestFrameworks).value
      },

      scalaJSTestHTMLArtifactDirectory := {
        val stageSuffix = scalaJSStage.value match {
          case Stage.FastOpt => "fastopt"
          case Stage.FullOpt => "opt"
        }
        val config = configuration.value.name
        ((testHtml / crossTarget).value /
            ((testHtml / moduleName).value + s"-$stageSuffix-$config-html"))
      },

      testHtml / artifactPath :=
        scalaJSTestHTMLArtifactDirectory.value / "index.html",

      testHtml := {
        val log = streams.value.log
        val output = (testHtml / artifactPath).value
        val artifactDirectory = scalaJSTestHTMLArtifactDirectory.value

        val title = name.value + " - tests"
        val input = (testHtml / jsEnvInput).value

        val frameworks = (testHtml / loadedTestFrameworks).value.toList
        val frameworkImplClassNames =
          frameworks.map(_._1.implClassNames.toList)

        val taskDefs = for (td <- (testHtml / definedTests).value) yield {
          new sbt.testing.TaskDef(td.name, td.fingerprint,
              td.explicitlySpecified, td.selectors)
        }

        IO.createDirectory(artifactDirectory)

        HTMLRunnerBuilder.write(output.toPath(), artifactDirectory.toPath(),
            title, input, frameworkImplClassNames, taskDefs.toList)

        if (input.exists(_.isInstanceOf[Input.ESModule])) {
          log.info(s"Wrote HTML test runner to $output. You must serve it " +
              "through an HTTP server (e.g. `python3 -m http.server`), since " +
              "it loads at least one ESModule.")
        } else {
          log.info(s"Wrote HTML test runner. Point your browser to ${output.toURI}")
        }

        Attributed.blank(output)
      }
  )

  private def isScala3(scalaV: String): Boolean =
    scalaV.startsWith("3.")

  private val scalaJSProjectBaseSettings = Seq(
      platformDepsCrossVersion := ScalaJSCrossVersion.binary,
      crossVersion := ScalaJSCrossVersion.binary,

      scalaJSModuleInitializers := Seq(),
      scalaJSUseMainModuleInitializer := false,
      jsEnvInput := Nil,

      /* Add core library dependencies (and compiler plugin), depending on the
       * Scala version (2.x versus 3.x).
       */
      libraryDependencies := {
        val prev = libraryDependencies.value
        val scalaOrg = scalaOrganization.value
        val scalaV = scalaVersion.value

        if (isScala3(scalaV)) {
          /* Remove scala3-library (non _sjs1) in case sbt-dotty was applied
           * before sbt-scalajs.
           */
          val filteredPrev = prev.filterNot { dep =>
            dep.organization == scalaOrg && dep.name == "scala3-library"
          }
          filteredPrev ++ Seq(
              scalaOrg %% "scala3-library_sjs1" % scalaV,
              /* scala3-library_sjs1 depends on some version of scalajs-library_2.13,
               * but we bump it to be at least scalaJSVersion.
               * (It will also depend on some version of scalajs-scalalib_2.13,
               * but we do not have to worry about that here.)
               */
              "org.scala-js" % "scalajs-library_2.13" % scalaJSVersion,
              "org.scala-js" % "scalajs-test-bridge_2.13" % scalaJSVersion % "test"
          )
        } else {
          prev ++ Seq(
              compilerPlugin("org.scala-js" % "scalajs-compiler" % scalaJSVersion cross CrossVersion.full),
              "org.scala-js" %% "scalajs-library" % scalaJSVersion,
              /* scalajs-library depends on some version of scalajs-scalalib,
               * but we want to make sure to bump it to be at least the one
               * of our own `scalaVersion` (which would have back-published in
               * the meantime).
               */
              "org.scala-js" %% "scalajs-scalalib" % s"$scalaV+$scalaJSVersion",
              "org.scala-js" %% "scalajs-test-bridge" % scalaJSVersion % "test"
          )
        }
      },

      /* Add the `-scalajs` compiler flag if this is Scala 3, and it is not
       * already present (that could happen if sbt-dotty is used and is applied
       * before sbt-scalajs).
       */
      scalacOptions := {
        val prev = scalacOptions.value
        if (isScala3(scalaVersion.value) && !prev.contains("-scalajs"))
          prev :+ "-scalajs"
        else
          prev
      }
  )

  val scalaJSProjectSettings: Seq[Setting[_]] = (
      scalaJSProjectBaseSettings ++
      inConfig(Compile)(scalaJSCompileSettings) ++
      inConfig(Test)(scalaJSTestSettings)
  )
}
