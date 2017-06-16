package org.scalajs.sbtplugin

import java.util.IllegalFormatException

import sbt._
import sbt.inc.{IncOptions, ClassfileManager}
import Keys._
import sbinary.DefaultProtocol._
import Cache.seqFormat
import complete.Parser
import complete.DefaultParsers._

import sbtcrossproject.CrossPlugin.autoImport._

import Loggers._

import org.scalajs.core.tools.io.{IO => _, _}
import org.scalajs.core.tools.linker._
import org.scalajs.core.tools.linker.standard._

import org.scalajs.jsenv._
import org.scalajs.jsenv.nodejs.NodeJSEnv

import org.scalajs.core.ir
import org.scalajs.core.ir.Utils.escapeJS
import org.scalajs.core.ir.ScalaJSVersions
import org.scalajs.core.ir.Printers.{InfoPrinter, IRTreePrinter}

import org.scalajs.testadapter.{FrameworkDetector, HTMLRunnerBuilder}

import scala.util.Try
import scala.collection.mutable

import java.io.FileNotFoundException
import java.nio.charset.Charset

/** Contains settings used by ScalaJSPlugin that should not be automatically
 *  be in the *.sbt file's scope.
 */
object ScalaJSPluginInternal {

  import ScalaJSPlugin.autoImport.{ModuleKind => _, _}

  /** The global Scala.js IR cache */
  val globalIRCache: IRFileCache = new IRFileCache()

  val scalaJSClearCacheStats = TaskKey[Unit]("scalaJSClearCacheStats",
      "Scala.js internal: Clear the global IR cache's statistics. Used to " +
      "implement cache statistics.", KeyRanks.Invisible)

  /** Dummy setting to persist a Scala.js linker. */
  val scalaJSLinker = SettingKey[ClearableLinker]("scalaJSLinker",
      "Scala.js internal: Setting to persist a linker", KeyRanks.Invisible)

  /** A tag to indicate that a task is using the value of [[scalaJSLinker]]
   *
   *  This setting's value should always be retrieved from the same scope than
   *  [[scalaJSLinker]] was retrieved from.
   */
  val usesScalaJSLinkerTag = SettingKey[Tags.Tag]("usesScalaJSLinkerTag",
      "Scala.js internal: Tag to indicate that a task uses the link or " +
      "linkUnit method of the value of scalaJSLinker", KeyRanks.Invisible)

  val scalaJSIRCacheHolder = SettingKey[globalIRCache.Cache]("scalaJSIRCacheHolder",
      "Scala.js internal: Setting to persist a cache. Do NOT use this directly. " +
      "Use scalaJSIRCache instead.", KeyRanks.Invisible)

  val scalaJSIRCache = TaskKey[globalIRCache.Cache]("scalaJSIRCache",
      "Scala.js internal: Task to access a cache.", KeyRanks.Invisible)

  /** All .sjsir files on the fullClasspath, used by scalajsp. */
  val sjsirFilesOnClasspath = TaskKey[Seq[String]]("sjsirFilesOnClasspath",
      "All .sjsir files on the fullClasspath, used by scalajsp",
      KeyRanks.Invisible)

  val scalaJSSourceFiles = AttributeKey[Seq[File]]("scalaJSSourceFiles",
      "Files used to compute this value (can be used in FileFunctions later).",
      KeyRanks.Invisible)

  val stageKeys: Map[Stage, TaskKey[Attributed[File]]] = Map(
    Stage.FastOpt -> fastOptJS,
    Stage.FullOpt -> fullOptJS
  )

  /** A JS expression that detects the global scope just like Scala.js */
  val jsGlobalExpr: String = {
    """((typeof global === "object" && global &&
         global["Object"] === Object) ? global : this)"""
  }

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
  private val DefaultParallelLinker: Boolean = {
    try {
      scala.util.Properties.isJavaAtLeast("1.8")
      true
    } catch {
      case _: NumberFormatException => false
    }
  }

  def logIRCacheStats(logger: Logger): Unit = {
    logger.debug("Global IR cache stats: " + globalIRCache.stats.logLine)
  }

  /** Patches the IncOptions so that .sjsir files are pruned as needed.
   *
   *  This complicated logic patches the ClassfileManager factory of the given
   *  IncOptions with one that is aware of .sjsir files emitted by the Scala.js
   *  compiler. This makes sure that, when a .class file must be deleted, the
   *  corresponding .sjsir file are also deleted.
   */
  def scalaJSPatchIncOptions(incOptions: IncOptions): IncOptions = {
    val inheritedNewClassfileManager = incOptions.newClassfileManager
    val newClassfileManager = () => new ClassfileManager {
      private[this] val inherited = inheritedNewClassfileManager()

      def delete(classes: Iterable[File]): Unit = {
        inherited.delete(classes flatMap { classFile =>
          val scalaJSFiles = if (classFile.getPath endsWith ".class") {
            val f = FileVirtualFile.withExtension(classFile, ".class", ".sjsir")
            if (f.exists) List(f)
            else Nil
          } else Nil
          classFile :: scalaJSFiles
        })
      }

      def generated(classes: Iterable[File]): Unit = inherited.generated(classes)
      def complete(success: Boolean): Unit = inherited.complete(success)
    }
    incOptions.withNewClassfileManager(newClassfileManager)
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

        new ClearableLinker(() => StandardLinker(config), config.batchMode)
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

            linker.link(ir, moduleInitializers,
                AtomicWritableFileVirtualJSFile(output),
                sbtLogger2ToolsLogger(log))

            logIRCacheStats(log)

            Set(output)
          } (realFiles.toSet)

          val sourceMapFile = FileVirtualJSFile(output).sourceMapFile
          Attributed.blank(output).put(scalaJSSourceMap, sourceMapFile)
        }.tag(usesLinkerTag)
      }.value,

      scalaJSLinkedFile in key := new FileVirtualJSFile(key.value.data)
  )

  private def dispatchSettingKeySettings[T](key: SettingKey[T]) = Seq(
      key := Def.settingDyn {
        val stageKey = stageKeys(scalaJSStage.value)
        Def.setting { (key in stageKey).value }
      }.value
  )

  private def dispatchTaskKeySettings[T](key: TaskKey[T]) = Seq(
      key := Def.settingDyn {
        val stageKey = stageKeys(scalaJSStage.value)
        Def.task { (key in stageKey).value }
      }.value
  )

  private def scalajspSettings: Seq[Setting[_]] = {
    case class Options(
        infos: Boolean = false
    )

    val optionsParser: Parser[Options] = {
      token(OptSpace ~> (
          (literal("-i") | "--infos") ^^^ ((_: Options).copy(infos = true))
      )).* map {
        fns => Function.chain(fns)(Options())
      }
    }

    def sjsirFileOnClasspathParser(
        relPaths: Seq[String]): Parser[String] = {
      OptSpace ~> StringBasic
        .examples(ScalajspUtils.relPathsExamples(relPaths))
    }

    def scalajspParser(state: State, relPaths: Seq[String]) =
      optionsParser ~ sjsirFileOnClasspathParser(relPaths)

    val parser = loadForParser(sjsirFilesOnClasspath) { (state, relPaths) =>
      scalajspParser(state, relPaths.getOrElse(Nil))
    }

    Seq(
        sjsirFilesOnClasspath := Def.task {
          scalaJSIR.value.data.map(_.relativePath).toSeq
        }.storeAs(sjsirFilesOnClasspath).triggeredBy(scalaJSIR).value,

        scalajsp := {
          val (options, relPath) = parser.parsed

          val vfile = scalaJSIR.value.data
              .find(_.relativePath == relPath)
              .getOrElse(throw new FileNotFoundException(relPath))

          val stdout = new java.io.PrintWriter(System.out)
          if (options.infos)
            new InfoPrinter(stdout).print(vfile.info)
          else
            new IRTreePrinter(stdout).printTopLevelTree(vfile.tree)
          stdout.flush()

          logIRCacheStats(streams.value.log)
        }
    )
  }

  /** Collect certain file types from a classpath.
   *
   *  @param cp Classpath to collect from
   *  @param filter Filter for (real) files of interest (not in jars)
   *  @param collectJar Collect elements from a jar (called for all jars)
   *  @param collectFile Collect a single file. Params are the file and the
   *      relative path of the file (to its classpath entry root).
   *  @return Collected elements attributed with physical files they originated
   *      from (key: scalaJSSourceFiles).
   */
  private def collectFromClasspath[T](cp: Def.Classpath, filter: FileFilter,
      collectJar: VirtualJarFile => Seq[T],
      collectFile: (File, String) => T): Attributed[Seq[T]] = {

    val realFiles = Seq.newBuilder[File]
    val results = Seq.newBuilder[T]

    for (cpEntry <- Attributed.data(cp) if cpEntry.exists) {
      if (cpEntry.isFile && cpEntry.getName.endsWith(".jar")) {
        realFiles += cpEntry
        val vf = new FileVirtualBinaryFile(cpEntry) with VirtualJarFile
        results ++= collectJar(vf)
      } else if (cpEntry.isDirectory) {
        for {
          (file, relPath0) <- Path.selectSubpaths(cpEntry, filter)
        } {
          val relPath = relPath0.replace(java.io.File.separatorChar, '/')
          realFiles += file
          results += collectFile(file, relPath)
        }
      } else {
        throw new IllegalArgumentException(
            "Illegal classpath entry: " + cpEntry.getPath)
      }
    }

    Attributed.blank(results.result()).put(scalaJSSourceFiles, realFiles.result())
  }

  val scalaJSConfigSettings: Seq[Setting[_]] = Seq(
      incOptions ~= scalaJSPatchIncOptions
  ) ++ (
      scalajspSettings ++
      stageKeys.flatMap((scalaJSStageSettings _).tupled) ++
      dispatchTaskKeySettings(scalaJSLinkedFile) ++
      dispatchSettingKeySettings(scalaJSLinker) ++
      dispatchSettingKeySettings(usesScalaJSLinkerTag)
  ) ++ Seq(
      /* Note: This cache only gets freed by its finalizer. Otherwise we'd need
       * to intercept reloads in sbt (see #2171).
       * Also note that it doesn't get cleared by the sbt's clean task.
       */
      scalaJSIRCacheHolder := globalIRCache.newCache,
      scalaJSIRCache := Def.task {
        scalaJSIRCacheHolder.value
      }.dependsOn(scalaJSClearCacheStats).value,

      scalaJSIR := {
        val rawIR = collectFromClasspath(fullClasspath.value, "*.sjsir",
            collectJar = Seq(_),
            collectFile = FileVirtualScalaJSIRFile.relative)

        val cache = scalaJSIRCache.value
        rawIR.map(cache.cached)
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
          .withClosureCompiler(prevConfig.outputMode == OutputMode.ECMAScript51Isolated)
      },

      console := console.dependsOn(Def.task {
        streams.value.log.warn("Scala REPL doesn't work with Scala.js. You " +
            "are running a JVM REPL. JavaScript things won't work.")
      }).value,

      scalaJSJavaSystemProperties ++= {
        val javaSysPropsPattern = "-D([^=]*)=(.*)".r
        javaOptions.value.map {
          case javaSysPropsPattern(propName, propValue) => (propName, propValue)
          case opt =>
            throw new MessageOnlyException(
                "Scala.js javaOptions can only be \"-D<key>=<value>\"," +
                " but received: " + opt)
        }.toMap
      },

      // Optionally add a JS file defining Java system properties
      jsExecutionFiles ++= {
        val javaSystemProperties = scalaJSJavaSystemProperties.value
        if (javaSystemProperties.isEmpty) {
          Nil
        } else {
          val formattedProps = javaSystemProperties.map {
            case (propName, propValue) =>
              "\"" + escapeJS(propName) + "\": \"" + escapeJS(propValue) + "\""
          }
          val code = {
            "var __ScalaJSEnv = (typeof __ScalaJSEnv === \"object\" && __ScalaJSEnv) ? __ScalaJSEnv : {};\n" +
            "__ScalaJSEnv.javaSystemProperties = {" + formattedProps.mkString(", ") + "};\n"
          }
          Seq(new MemVirtualJSFile("setJavaSystemProperties.js").withContent(code))
        }
      },

      // Crucially, add the Scala.js linked file to the JS files
      jsExecutionFiles += scalaJSLinkedFile.value
  )

  // These settings will be filtered by the stage dummy tasks
  val scalaJSRunSettings = Seq(
      scalaJSMainModuleInitializer := {
        mainClass.value.map { mainCl =>
          ModuleInitializer.mainMethodWithArgs(mainCl, "main")
        }
      },

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
        val files = jsExecutionFiles.value

        log.info("Running " + mainClass.value.getOrElse("<unknown class>"))
        log.debug(s"with JSEnv ${env.name}")

        env.jsRunner(files).run(sbtLogger2ToolsLogger(log), ConsoleJSConsole)
      },

      runMain := {
        throw new MessageOnlyException("`runMain` is not supported in Scala.js")
      }
  )

  val scalaJSCompileSettings = (
      scalaJSConfigSettings ++
      scalaJSRunSettings
  )

  val scalaJSTestFrameworkSettings = Seq(
      loadedTestFrameworks := {
        if (fork.value) {
          throw new MessageOnlyException(
              "`test` tasks in a Scala.js project require " +
              "`fork in Test := false`.")
        }

        val env = jsEnv.value match {
          case env: ComJSEnv => env

          case env =>
            throw new MessageOnlyException(
                s"You need a ComJSEnv to test (found ${env.name})")
        }

        val files = jsExecutionFiles.value

        val moduleKind = scalaJSLinkerConfig.value.moduleKind
        val moduleIdentifier = moduleKind match {
          case ModuleKind.NoModule       => None
          case ModuleKind.CommonJSModule => Some(scalaJSLinkedFile.value.path)
        }

        val frameworksAndTheirImplNames =
          testFrameworks.value.map(f => f -> f.implClassNames.toList)

        val logger = sbtLogger2ToolsLogger(streams.value.log)

        FrameworkDetector.detectFrameworks(env, files, moduleKind,
            moduleIdentifier, frameworksAndTheirImplNames, logger)
      },

      // Override default to avoid triggering a test:fastOptJS in a test:compile
      // without loosing autocompletion.
      definedTestNames := {
        definedTests.map(_.map(_.name).distinct)
          .storeAs(definedTestNames).triggeredBy(loadedTestFrameworks).value
      }
  )

  val scalaJSTestBuildSettings = (
      scalaJSConfigSettings
  ) ++ (
      Seq(fastOptJS, fullOptJS) map { packageJSTask =>
        moduleName in packageJSTask := moduleName.value + "-test"
      }
  )

  val scalaJSTestHtmlSettings = Seq(
      artifactPath in testHtml := {
        val stageSuffix = scalaJSStage.value match {
          case Stage.FastOpt => "fastopt"
          case Stage.FullOpt => "opt"
        }
        ((crossTarget in testHtml).value /
            ((moduleName in testHtml).value + s"-$stageSuffix-test.html"))
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

  val scalaJSTestSettings = (
      scalaJSTestBuildSettings ++
      scalaJSRunSettings ++
      scalaJSTestFrameworkSettings ++
      scalaJSTestHtmlSettings
  )

  val scalaJSDefaultBuildConfigs = (
      inConfig(Compile)(scalaJSConfigSettings) ++ // build settings for Compile
      inConfig(Test)(scalaJSTestBuildSettings)
  )

  val scalaJSDefaultConfigs = (
      inConfig(Compile)(scalaJSCompileSettings) ++
      inConfig(Test)(scalaJSTestSettings)
  )

  val scalaJSProjectBaseSettings = Seq(
      crossPlatform := JSPlatform,

      scalaJSLinkerConfig := {
        StandardLinker.Config()
          .withParallel(DefaultParallelLinker)
      },

      scalaJSModuleInitializers := Seq(),
      scalaJSModuleInitializers in Compile := scalaJSModuleInitializers.value,
      // Do not inherit scalaJSModuleInitializers in Test from Compile
      scalaJSModuleInitializers in Test := scalaJSModuleInitializers.value,

      scalaJSUseMainModuleInitializer := false,
      scalaJSUseMainModuleInitializer in Test := false,

      jsEnv := new NodeJSEnv(),

      jsExecutionFiles := Nil,
      jsExecutionFiles in Compile := jsExecutionFiles.value,
      // Do not inherit jsExecutionFiles in Test from Compile
      jsExecutionFiles in Test := jsExecutionFiles.value,

      clean := {
        // have clean reset incremental linker state
        val _ = clean.value
        (scalaJSLinker in (Compile, fastOptJS)).value.clear()
        (scalaJSLinker in (Test, fastOptJS)).value.clear()
        (scalaJSLinker in (Compile, fullOptJS)).value.clear()
        (scalaJSLinker in (Test, fullOptJS)).value.clear()
        ()
      },

      scalaJSJavaSystemProperties := Map.empty
  )

  val scalaJSAbstractSettings: Seq[Setting[_]] = (
      scalaJSProjectBaseSettings ++
      scalaJSDefaultConfigs
  )

  val scalaJSAbstractBuildSettings: Seq[Setting[_]] = (
      scalaJSProjectBaseSettings ++
      scalaJSDefaultBuildConfigs
  )

  val scalaJSEcosystemSettings = Seq(
      // you will need the Scala.js compiler plugin
      autoCompilerPlugins := true,
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

}
