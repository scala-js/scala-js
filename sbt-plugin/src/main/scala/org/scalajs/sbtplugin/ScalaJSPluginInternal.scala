package org.scalajs.sbtplugin

import java.util.IllegalFormatException

import sbt._
import sbt.inc.{IncOptions, ClassfileManager}
import Keys._
import sbinary.DefaultProtocol._
import Cache.seqFormat
import complete.Parser
import complete.DefaultParsers._

import Loggers._

import org.scalajs.core.tools.sem.Semantics
import org.scalajs.core.tools.io.{IO => toolsIO, _}
import org.scalajs.core.tools.jsdep._
import org.scalajs.core.tools.json._
import org.scalajs.core.tools.linker.{ClearableLinker, Linker}
import org.scalajs.core.tools.linker.frontend.LinkerFrontend
import org.scalajs.core.tools.linker.backend.{LinkerBackend, ModuleKind, OutputMode}

import org.scalajs.jsenv._
import org.scalajs.jsenv.phantomjs.PhantomJettyClassLoader

import org.scalajs.core.ir
import org.scalajs.core.ir.Utils.escapeJS
import org.scalajs.core.ir.ScalaJSVersions
import org.scalajs.core.ir.Printers.{InfoPrinter, IRTreePrinter}

import org.scalajs.testadapter.ScalaJSFramework

import scala.util.Try
import scala.collection.mutable

import java.io.FileNotFoundException
import java.nio.charset.Charset
import java.net.URLClassLoader

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

  /** Dummy setting to ensure we do not fork in Scala.js run & test. */
  val scalaJSEnsureUnforked = SettingKey[Boolean]("ensureUnforked",
      "Scala.js internal: Fails if fork is true.", KeyRanks.Invisible)

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

  /** Internal task to calculate whether a project requests the DOM
   *  (through jsDependencies or requiresDOM) */
  val scalaJSRequestsDOM = TaskKey[Boolean]("scalaJSRequestsDOM",
      "Scala.js internal: Whether a project really wants the DOM. " +
      "Calculated using requiresDOM and jsDependencies", KeyRanks.Invisible)

  /** All .sjsir files on the fullClasspath, used by scalajsp. */
  val sjsirFilesOnClasspath = TaskKey[Seq[String]]("sjsirFilesOnClasspath",
      "All .sjsir files on the fullClasspath, used by scalajsp",
      KeyRanks.Invisible)

  val scalaJSModuleIdentifier = TaskKey[Option[String]](
      "scalaJSModuleIdentifier",
      "An identifier for the module which contains the exports of Scala.js",
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

  private def packageJSDependenciesSetting(taskKey: TaskKey[File], cacheName: String,
      getLib: ResolvedJSDependency => VirtualJSFile): Setting[Task[File]] = {
    taskKey := Def.taskDyn {
      if ((skip in taskKey).value)
        Def.task((artifactPath in taskKey).value)
      else Def.task {
        val s = (streams in taskKey).value
        val deps = resolvedJSDependencies.value
        val output = (artifactPath in taskKey).value

        val realFiles = deps.get(scalaJSSourceFiles).get
        val resolvedDeps = deps.data

        FileFunction.cached(s.cacheDirectory / cacheName, FilesInfo.lastModified,
            FilesInfo.exists) { _ => // We don't need the files

          IO.createDirectory(output.getParentFile)

          val outFile = AtomicWritableFileVirtualJSFile(output)
          toolsIO.concatFiles(outFile, resolvedDeps.map(getLib))

          Set(output)
        } (realFiles.toSet)

        output
      }
    }.value
  }

  /** Settings for the production key (e.g. fastOptJS) of a given stage */
  private def scalaJSStageSettings(stage: Stage,
      key: TaskKey[Attributed[File]]): Seq[Setting[_]] = Seq(
      scalaJSLinker in key := {
        val opts = (scalaJSOptimizerOptions in key).value

        val semantics = (scalaJSSemantics in key).value
        val outputMode = (scalaJSOutputMode in key).value
        val moduleKind = scalaJSModuleKind.value // intentionally not 'in key'
        val withSourceMap = (emitSourceMaps in key).value

        val relSourceMapBase = {
          if ((relativeSourceMaps in key).value)
            Some((artifactPath in key).value.getParentFile.toURI())
          else
            None
        }

        val frontendConfig = LinkerFrontend.Config()
          .withBypassLinkingErrorsInternal(opts.bypassLinkingErrors)
          .withCheckIR(opts.checkScalaJSIR)

        val backendConfig = LinkerBackend.Config()
          .withRelativizeSourceMapBase(relSourceMapBase)
          .withCustomOutputWrapper(scalaJSOutputWrapper.value)
          .withPrettyPrint(opts.prettyPrintFullOptJS)

        val config = Linker.Config()
          .withSourceMap(withSourceMap)
          .withOptimizer(!opts.disableOptimizer)
          .withParallel(opts.parallel)
          .withClosureCompiler(opts.useClosureCompiler)
          .withFrontendConfig(frontendConfig)
          .withBackendConfig(backendConfig)

        val newLinker = { () =>
          Linker(semantics, outputMode, moduleKind, config)
        }

        new ClearableLinker(newLinker, opts.batchMode)
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
        val s = (streams in key).value
        val log = s.log
        val irInfo = (scalaJSIR in key).value
        val realFiles = irInfo.get(scalaJSSourceFiles).get
        val ir = irInfo.data
        val output = (artifactPath in key).value

        Def.task {
          FileFunction.cached(s.cacheDirectory, FilesInfo.lastModified,
              FilesInfo.exists) { _ => // We don't need the files

            val stageName = stage match {
              case Stage.FastOpt => "Fast"
              case Stage.FullOpt => "Full"
            }

            log.info(s"$stageName optimizing $output")

            IO.createDirectory(output.getParentFile)

            val linker = (scalaJSLinker in key).value
            linker.link(ir, AtomicWritableFileVirtualJSFile(output), sbtLogger2ToolsLogger(log))

            logIRCacheStats(log)

            Set(output)
          } (realFiles.toSet)

          val sourceMapFile = FileVirtualJSFile(output).sourceMapFile
          Attributed.blank(output).put(scalaJSSourceMap, sourceMapFile)
        } tag((usesScalaJSLinkerTag in key).value)
      }.value,

      key := key.dependsOn(packageJSDependencies, packageScalaJSLauncher).value,

      scalaJSLinkedFile in key := new FileVirtualJSFile(key.value.data)
  )

  private def dispatchSettingKeySettings[T](key: SettingKey[T]) = Seq(
      key := Def.settingDyn {
        val stageKey = stageKeys(scalaJSStage.value)
        Def.setting { (key in stageKey).value }
      }.value
  )

  private def dispatchTaskKeySettings[T](key: TaskKey[T]) = Seq(
      key := Def.taskDyn {
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
        sys.error("Illegal classpath entry: " + cpEntry.getPath)
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
        import IRFileCache.IRContainer

        val rawIR = collectFromClasspath(fullClasspath.value,
            "*.sjsir", collectJar = jar => IRContainer.Jar(jar) :: Nil,
            collectFile = { (file, relPath) =>
              IRContainer.File(FileVirtualScalaJSIRFile.relative(file, relPath))
            })

        val cache = scalaJSIRCache.value
        rawIR.map(cache.cached)
      },

      artifactPath in fastOptJS :=
        ((crossTarget in fastOptJS).value /
            ((moduleName in fastOptJS).value + "-fastopt.js")),

      artifactPath in fullOptJS :=
        ((crossTarget in fullOptJS).value /
            ((moduleName in fullOptJS).value + "-opt.js")),

      scalaJSSemantics in fullOptJS ~= (_.optimized),
      scalaJSOptimizerOptions in fullOptJS := {
        val prev = (scalaJSOptimizerOptions in fullOptJS).value
        val outputMode = (scalaJSOutputMode in fullOptJS).value
        prev.withUseClosureCompiler(outputMode == OutputMode.ECMAScript51Isolated)
      },

      fullOptJS := fullOptJS.dependsOn(packageMinifiedJSDependencies).value,

      artifactPath in packageScalaJSLauncher :=
        ((crossTarget in packageScalaJSLauncher).value /
            ((moduleName in packageScalaJSLauncher).value + "-launcher.js")),

      skip in packageScalaJSLauncher := {
        val value = !persistLauncher.value
        if (!value && (scalaJSModuleKind.value != ModuleKind.NoModule)) {
          throw new MessageOnlyException(
              "persistLauncher := true is not compatible with emitting " +
              "JavaScript modules")
        }
        value
      },

      packageScalaJSLauncher := Def.taskDyn {
        if ((skip in packageScalaJSLauncher).value) {
          Def.task {
            Attributed.blank((artifactPath in packageScalaJSLauncher).value)
          }
        } else {
          Def.task {
            mainClass.value map { mainCl =>
              val file = (artifactPath in packageScalaJSLauncher).value
              assert(scalaJSModuleKind.value == ModuleKind.NoModule,
                  "Cannot produce a launcher file when scalaJSModuleKind " +
                  "is different from NoModule")
              IO.write(file,
                  launcherContent(mainCl, ModuleKind.NoModule, None),
                  Charset.forName("UTF-8"))

              // Attach the name of the main class used, (ab?)using the name key
              Attributed(file)(AttributeMap.empty.put(name.key, mainCl))
            } getOrElse {
              sys.error("Cannot write launcher file, since there is no or multiple mainClasses")
            }
          }
        }
      }.value,

      artifactPath in packageJSDependencies :=
        ((crossTarget in packageJSDependencies).value /
            ((moduleName in packageJSDependencies).value + "-jsdeps.js")),

      packageJSDependenciesSetting(packageJSDependencies, "package-js-deps", _.lib),

      artifactPath in packageMinifiedJSDependencies :=
        ((crossTarget in packageMinifiedJSDependencies).value /
            ((moduleName in packageMinifiedJSDependencies).value + "-jsdeps.min.js")),

      packageJSDependenciesSetting(packageMinifiedJSDependencies,
          "package-min-js-deps", dep => dep.minifiedLib.getOrElse(dep.lib)),

      jsDependencyManifest := {
        val myModule = thisProject.value.id
        val config = configuration.value.name

        // Collect all libraries
        val jsDeps = jsDependencies.value.collect {
          case dep: JSModuleID if dep.configurations.forall(_ == config) =>
            dep.jsDep
        }

        val requiresDOM = jsDependencies.value.exists {
          case RuntimeDOMDep(configurations) =>
            configurations.forall(_ == config)
          case _ => false
        }

        /* We make the assumption here, that scalaJSSemantics has not
         * unreasonably overridden values for the fastOptJS and fullOptJS
         * tasks. Otherwise this value does not really make sense.
         */
        val compliantSemantics = scalaJSSemantics.value.compliants

        val manifest = new JSDependencyManifest(new Origin(myModule, config),
            jsDeps.toList, requiresDOM, compliantSemantics)

        // Write dependency file to class directory
        val targetDir = classDirectory.value
        IO.createDirectory(targetDir)

        val file = targetDir / JSDependencyManifest.ManifestFileName
        val vfile = WritableFileVirtualTextFile(file)

        // Prevent writing if unnecessary to not invalidate dependencies
        val needWrite = !vfile.exists || {
          Try {
            val readManifest = JSDependencyManifest.read(vfile)
            readManifest != manifest
          } getOrElse true
        }

        if (needWrite)
          JSDependencyManifest.write(manifest, vfile)

        file
      },

      products := products.dependsOn(jsDependencyManifest).value,

      console := console.dependsOn(Def.task {
        streams.value.log.warn("Scala REPL doesn't work with Scala.js. You " +
            "are running a JVM REPL. JavaScript things won't work.")
      }).value,

      scalaJSNativeLibraries := {
        collectFromClasspath(fullClasspath.value,
            "*.js", collectJar = _.jsFiles,
            collectFile = FileVirtualJSFile.relative)
      },

      jsDependencyManifests := {
        val filter = jsManifestFilter.value
        val rawManifests = collectFromClasspath(fullClasspath.value,
            new ExactFilter(JSDependencyManifest.ManifestFileName),
            collectJar = _.jsDependencyManifests,
            collectFile = { (file, _) =>
              fromJSON[JSDependencyManifest](readJSON(IO.read(file)))
            })

        rawManifests.map(manifests => filter(manifests.toTraversable))
      },

      resolvedJSDependencies := {
        val dependencyFilter = jsDependencyFilter.value
        val attLibs = scalaJSNativeLibraries.value
        val attManifests = jsDependencyManifests.value

        // Verify semantics compliance
        if (checkScalaJSSemantics.value) {
          import ComplianceRequirement._
          val requirements = mergeFromManifests(attManifests.data)

          /* We make the assumption here, that scalaJSSemantics has not
           * unreasonably overridden values for the fastOptJS and fullOptJS
           * tasks. Otherwise, this check is bogus.
           */
          checkCompliance(requirements, scalaJSSemantics.value)
        }

        // Collect originating files
        val realFiles = {
          attLibs.get(scalaJSSourceFiles).get ++
          attManifests.get(scalaJSSourceFiles).get
        }

        // Collect available JS libraries
        val availableLibs = {
          val libs = mutable.Map.empty[String, VirtualJSFile]
          for (lib <- attLibs.data)
            libs.getOrElseUpdate(lib.relativePath, lib)
          libs.toMap
        }

        // Actually resolve the dependencies
        val resolved = DependencyResolver.resolveDependencies(
            attManifests.data, availableLibs, dependencyFilter)

        Attributed.blank[Seq[ResolvedJSDependency]](resolved)
            .put(scalaJSSourceFiles, realFiles)
      },

      // Give tasks ability to check we are not forking at build reading time
      scalaJSEnsureUnforked := {
        if (fork.value)
          sys.error("Scala.js cannot be run in a forked JVM")
        else
          true
      },

      scalaJSRequestsDOM := {
        requiresDOM.?.value.getOrElse(
            jsDependencyManifests.value.data.exists(_.requiresDOM))
      },

      resolvedJSEnv := jsEnv.?.value.getOrElse {
        if (scalaJSUseRhinoInternal.value) {
          RhinoJSEnvInternal().value
        } else if (scalaJSRequestsDOM.value) {
          JSDOMNodeJSEnv().value
        } else {
          NodeJSEnv().value
        }
      },

      scalaJSJavaSystemProperties ++= {
        val javaSysPropsPattern = "-D([^=]*)=(.*)".r
        javaOptions.value.map {
          case javaSysPropsPattern(propName, propValue) => (propName, propValue)
          case opt =>
            sys.error("Scala.js javaOptions can only be \"-D<key>=<value>\"," +
                " but received: " + opt)
        }.toMap
      },

      scalaJSConfigurationLibs ++= {
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
          Seq(ResolvedJSDependency.minimal(
              new MemVirtualJSFile("setJavaSystemProperties.js").withContent(code)))
        }
      },

      loadedJSEnv := Def.taskDyn {
        val log = streams.value.log
        val libs =
          resolvedJSDependencies.value.data ++ scalaJSConfigurationLibs.value
        resolvedJSEnv.value match {
          /* Do not apply the LinkingUnitJSEnv treatment when
           * scalaJSModuleKind != NoModule, because the API of LinkingUnitJSEnv
           * is not designed to deal with modules, and would ignore that
           * setting.
           */
          case env: LinkingUnitJSEnv
              if scalaJSModuleKind.value == ModuleKind.NoModule =>
            log.debug(s"Generating LinkingUnit for JSEnv ${env.name}")
            Def.task {
              val linker = scalaJSLinker.value
              val ir = scalaJSIR.value.data
              val unit = linker.linkUnit(ir, env.symbolRequirements,
                  sbtLogger2ToolsLogger(log))

              log.debug("Loading JSEnv with LinkingUnit")
              env.loadLibs(libs).loadLinkingUnit(unit)
            } tag(usesScalaJSLinkerTag.value)
          case env =>
            Def.task {
              val file = scalaJSLinkedFile.value
              log.debug(s"Loading JSEnv with linked file ${file.path}")
              env.loadLibs(libs :+ ResolvedJSDependency.minimal(file))
            }
        }
      }.value,

      scalaJSModuleIdentifier := Def.taskDyn[Option[String]] {
        scalaJSModuleKind.value match {
          case ModuleKind.NoModule =>
            Def.task {
              None
            }

          case ModuleKind.CommonJSModule =>
            Def.task {
              Some(scalaJSLinkedFile.value.path)
            }
        }
      }.value
  )

  /** Run a class in a given environment using a given launcher */
  private def jsRun(jsEnv: JSEnv, mainCl: String,
      launcher: VirtualJSFile, log: Logger, console: JSConsole) = {

    log.info("Running " + mainCl)
    log.debug(s"with JSEnv ${jsEnv.name}")

    val runner = jsEnv.jsRunner(launcher)
    runner.run(sbtLogger2ToolsLogger(log), console)
  }

  private def launcherContent(mainCl: String, moduleKind: ModuleKind,
      moduleIdentifier: Option[String]): String = {
    val exportsNamespaceExpr =
      makeExportsNamespaceExpr(moduleKind, moduleIdentifier)
    val parts = mainCl.split('.').map(s => s"""["${escapeJS(s)}"]""").mkString
    s"$exportsNamespaceExpr$parts().main();\n"
  }

  private[sbtplugin] def makeExportsNamespaceExpr(moduleKind: ModuleKind,
      moduleIdentifier: Option[String]): String = {
    // !!! DUPLICATE code with ScalaJSFramework.optionalExportsNamespacePrefix
    moduleKind match {
      case ModuleKind.NoModule =>
        jsGlobalExpr

      case ModuleKind.CommonJSModule =>
        val moduleIdent = moduleIdentifier.getOrElse {
          throw new IllegalArgumentException(
              "The module identifier must be specified for CommonJS modules")
        }
        s"""require("${escapeJS(moduleIdent)}")"""
    }
  }

  private def memLauncher(mainCl: String, moduleKind: ModuleKind,
      moduleIdentifier: Option[String]): VirtualJSFile = {
    new MemVirtualJSFile("Generated launcher file")
      .withContent(launcherContent(mainCl, moduleKind, moduleIdentifier))
  }

  def discoverJSApps(analysis: inc.Analysis): Seq[String] = {
    import xsbt.api.{Discovered, Discovery}

    val jsApp = "scala.scalajs.js.JSApp"

    def isJSApp(discovered: Discovered) =
      discovered.isModule && discovered.baseClasses.contains(jsApp)

    Discovery(Set(jsApp), Set.empty)(Tests.allDefs(analysis)) collect {
      case (definition, discovered) if isJSApp(discovered) =>
        definition.name
    }
  }

  private val runMainParser = {
    Defaults.loadForParser(discoveredMainClasses) { (_, names) =>
      val mainClasses = names.getOrElse(Nil).toSet
      Space ~> token(NotSpace examples mainClasses)
    }
  }

  // These settings will be filtered by the stage dummy tasks
  val scalaJSRunSettings = Seq(
      mainClass in scalaJSLauncher := (mainClass in run).value,
      scalaJSLauncher := Def.taskDyn[Attributed[VirtualJSFile]] {
        if (persistLauncher.value) {
          Def.task {
            packageScalaJSLauncher.value.map(FileVirtualJSFile)
          }
        } else {
          Def.task {
            (mainClass in scalaJSLauncher).value.fold {
              sys.error("No main class detected.")
            } { mainClass =>
              val moduleKind = scalaJSModuleKind.value
              val moduleIdentifier = scalaJSModuleIdentifier.value
              val memLaunch =
                memLauncher(mainClass, moduleKind, moduleIdentifier)
              Attributed[VirtualJSFile](memLaunch)(
                  AttributeMap.empty.put(name.key, mainClass))
            }
          }
        }
      }.value,

      discoveredMainClasses := compile.map(discoverJSApps).
        storeAs(discoveredMainClasses).triggeredBy(compile).value,

      run := {
        // use assert to prevent warning about pure expr in stat pos
        assert(scalaJSEnsureUnforked.value)

        val launch = scalaJSLauncher.value
        val className = launch.get(name.key).getOrElse("<unknown class>")
        jsRun(loadedJSEnv.value, className, launch.data,
            streams.value.log, scalaJSConsole.value)
      },

      runMain := {
        // use assert to prevent warning about pure expr in stat pos
        assert(scalaJSEnsureUnforked.value)

        val mainClass = runMainParser.parsed
        val moduleKind = scalaJSModuleKind.value
        val moduleIdentifier = scalaJSModuleIdentifier.value
        jsRun(loadedJSEnv.value, mainClass,
            memLauncher(mainClass, moduleKind, moduleIdentifier),
            streams.value.log, scalaJSConsole.value)
      }
  )

  val scalaJSCompileSettings = (
      scalaJSConfigSettings ++
      scalaJSRunSettings
  )

  val scalaJSTestFrameworkSettings = Seq(
      loadedTestFrameworks := {
        // use assert to prevent warning about pure expr in stat pos
        assert(scalaJSEnsureUnforked.value)

        val console = scalaJSConsole.value
        val logger = streams.value.log
        val toolsLogger = sbtLogger2ToolsLogger(logger)
        val frameworks = testFrameworks.value

        val jsEnv = loadedJSEnv.value match {
          case jsEnv: ComJSEnv => jsEnv

          case jsEnv =>
            sys.error(s"You need a ComJSEnv to test (found ${jsEnv.name})")
        }

        val moduleKind = scalaJSModuleKind.value
        val moduleIdentifier = scalaJSModuleIdentifier.value

        val detector =
          new FrameworkDetector(jsEnv, moduleKind, moduleIdentifier)

        detector.detect(frameworks, toolsLogger) map { case (tf, name) =>
          (tf, new ScalaJSFramework(name, jsEnv, moduleKind, moduleIdentifier,
              toolsLogger, console))
        }
      },
      // Override default to avoid triggering a test:fastOptJS in a test:compile
      // without loosing autocompletion.
      definedTestNames := {
        definedTests.map(_.map(_.name).distinct)
          .storeAs(definedTestNames).triggeredBy(loadedJSEnv).value
      }
  )

  val scalaJSTestBuildSettings = (
      scalaJSConfigSettings
  ) ++ (
      Seq(fastOptJS, fullOptJS, packageScalaJSLauncher,
          packageJSDependencies) map { packageJSTask =>
        moduleName in packageJSTask := moduleName.value + "-test"
      }
  )

  private def scalaJSTestHtmlTaskSettings(
      testHtmlKey: TaskKey[Attributed[File]], sjsKey: TaskKey[Attributed[File]],
      jsdepsKey: TaskKey[File]) = {
    testHtmlKey := {
      if ((skip in jsdepsKey).value) {
        throw new MessageOnlyException(
            s"(skip in ${jsdepsKey.key}) must be false for ${testHtmlKey.key}.")
      }

      val log = streams.value.log
      val output = (artifactPath in testHtmlKey).value

      val css: java.io.File = {
        val name = "test-runner.css"
        val inputStream = getClass.getResourceAsStream(name)
        try {
          val outFile = (resourceManaged in testHtmlKey).value / name
          IO.transfer(inputStream, outFile)
          outFile
        } finally {
          inputStream.close()
        }
      }

      IO.write(output, HTMLRunnerTemplate.render(output.toURI,
          name.value + " - tests", (sjsKey in testHtmlKey).value.data.toURI,
          (jsdepsKey in testHtmlKey).value.toURI, css.toURI,
          (loadedTestFrameworks in testHtmlKey).value,
          (definedTests in testHtmlKey).value,
          (scalaJSJavaSystemProperties in testHtmlKey).value))

      log.info(s"Wrote HTML test runner. Point your browser to ${output.toURI}")

      Attributed.blank(output)
    }
  }

  val scalaJSTestHtmlSettings = Seq(
      artifactPath in testHtmlFastOpt :=
        ((crossTarget in testHtmlFastOpt).value /
            ((moduleName in testHtmlFastOpt).value + "-fastopt-test.html")),
      artifactPath in testHtmlFullOpt :=
        ((crossTarget in testHtmlFullOpt).value /
            ((moduleName in testHtmlFullOpt).value + "-opt-test.html"))
  ) ++ (
      scalaJSTestHtmlTaskSettings(testHtmlFastOpt, fastOptJS,
          packageJSDependencies) ++
      scalaJSTestHtmlTaskSettings(testHtmlFullOpt, fullOptJS,
          packageMinifiedJSDependencies)
  )

  val scalaJSTestSettings = (
      scalaJSTestBuildSettings ++
      scalaJSRunSettings ++
      scalaJSTestFrameworkSettings ++
      scalaJSTestHtmlSettings
  )

  val scalaJSDependenciesSettings = Seq(
      // add all the webjars your jsDependencies depend upon
      libraryDependencies ++= jsDependencies.value.collect {
        case JarJSModuleID(module, _) => module
      }
  )

  val scalaJSDefaultBuildConfigs = (
      inConfig(Compile)(scalaJSConfigSettings) ++ // build settings for Compile
      inConfig(Test)(scalaJSTestBuildSettings) ++
      scalaJSDependenciesSettings
  )

  val scalaJSDefaultConfigs = (
      inConfig(Compile)(scalaJSCompileSettings) ++
      inConfig(Test)(scalaJSTestSettings) ++
      scalaJSDependenciesSettings
  )

  val phantomJSJettyModules = Seq(
      "org.eclipse.jetty" % "jetty-websocket" % "8.1.16.v20140903",
      "org.eclipse.jetty" % "jetty-server" % "8.1.16.v20140903"
  )

  val scalaJSProjectBaseSettings = Seq(
      isScalaJSProject := true,

      relativeSourceMaps := false,
      persistLauncher := false,
      persistLauncher in Test := false,

      emitSourceMaps := true,

      scalaJSOutputWrapper := ("", ""),

      scalaJSOptimizerOptions := OptimizerOptions(),

      jsDependencies := Seq(),
      jsDependencyFilter := identity,
      jsManifestFilter := identity,

      scalaJSSemantics := Semantics.Defaults,
      scalaJSOutputMode := OutputMode.ECMAScript51Isolated,
      scalaJSModuleKind := ModuleKind.NoModule,
      checkScalaJSSemantics := true,

      scalaJSConsole := ConsoleJSConsole,

      clean := {
        // have clean reset incremental linker state
        val _ = clean.value
        (scalaJSLinker in (Compile, fastOptJS)).value.clear()
        (scalaJSLinker in (Test, fastOptJS)).value.clear()
        (scalaJSLinker in (Compile, fullOptJS)).value.clear()
        (scalaJSLinker in (Test, fullOptJS)).value.clear()
        ()
      },

      /* Depend on jetty artifacts in dummy configuration to be able to inject
       * them into the PhantomJS runner if necessary.
       * See scalaJSPhantomJSClassLoader
       */
      ivyConfigurations += config("phantom-js-jetty").hide,
      libraryDependencies ++= phantomJSJettyModules.map(_ % "phantom-js-jetty"),
      scalaJSPhantomJSClassLoader := {
        val report = update.value
        val jars = report.select(configurationFilter("phantom-js-jetty"))

        val jettyLoader =
          new URLClassLoader(jars.map(_.toURI.toURL).toArray, null)

        new PhantomJettyClassLoader(jettyLoader, getClass.getClassLoader)
      },
      scalaJSJavaSystemProperties := Map.empty,
      scalaJSConfigurationLibs := Nil
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
