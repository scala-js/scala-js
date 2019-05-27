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

import java.util.IllegalFormatException
import java.util.concurrent.atomic.AtomicReference

import sbt._
import Keys._
import complete.Parser
import complete.DefaultParsers._

import Loggers._
import SBTCompat._
import SBTCompat.formatImplicits._
import SBTCompat.formatImplicits.seqFormat

import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._

import org.scalajs.core.tools.sem.Semantics
import org.scalajs.core.tools.io.{IO => toolsIO, _}
import org.scalajs.core.tools.jsdep._
import org.scalajs.core.tools.json._
import org.scalajs.core.tools.linker._
import org.scalajs.core.tools.linker.standard._

import org.scalajs.jsenv._
import org.scalajs.jsenv.phantomjs.PhantomJettyClassLoader

import org.scalajs.core.ir
import org.scalajs.core.ir.Utils.escapeJS
import org.scalajs.core.ir.ScalaJSVersions
import org.scalajs.core.ir.Printers.{InfoPrinter, IRTreePrinter}

import org.scalajs.testadapter.TestAdapter

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

  @tailrec
  final private def registerResource[T <: AnyRef](
      l: AtomicReference[List[T]], r: T): r.type = {
    val prev = l.get()
    if (l.compareAndSet(prev, r :: prev)) r
    else registerResource(l, r)
  }

  private val allocatedIRCaches =
    new AtomicReference[List[globalIRCache.Cache]](Nil)

  /** Allocates a new IR cache linked to the [[globalIRCache]].
   *
   *  The allocated IR cache will automatically be freed when the build is
   *  unloaded.
   */
  private def newIRCache: globalIRCache.Cache =
    registerResource(allocatedIRCaches, globalIRCache.newCache)

  private[sbtplugin] def freeAllIRCaches(): Unit =
    allocatedIRCaches.getAndSet(Nil).foreach(_.free())

  private val createdTestAdapters =
    new AtomicReference[List[TestAdapter]](Nil)

  private def newTestAdapter(jsEnv: ComJSEnv, config: TestAdapter.Config): TestAdapter =
    registerResource(createdTestAdapters, new TestAdapter(jsEnv, config))

  private[sbtplugin] def closeAllTestAdapters(): Unit =
    createdTestAdapters.getAndSet(Nil).foreach(_.close())

  /** Non-deprecated alias of `scalaJSClearCacheStats` for internal use. */
  private[sbtplugin] val scalaJSClearCacheStatsInternal = TaskKey[Unit](
      "scalaJSClearCacheStats",
      "Scala.js internal: Clear the global IR cache's statistics. Used to " +
      "implement cache statistics.", KeyRanks.Invisible)

  @deprecated("Not used anymore.", "0.6.20")
  val scalaJSClearCacheStats = scalaJSClearCacheStatsInternal

  /** Dummy setting to ensure we do not fork in Scala.js run & test. */
  val scalaJSEnsureUnforked = SettingKey[Boolean]("ensureUnforked",
      "Scala.js internal: Fails if fork is true.", KeyRanks.Invisible)

  val scalaJSLinker: SettingKey[ClearableLinker] =
    ScalaJSPlugin.autoImport.scalaJSLinker

  val usesScalaJSLinkerTag: SettingKey[Tags.Tag] =
    ScalaJSPlugin.autoImport.usesScalaJSLinkerTag

  /** Non-deprecated alias of `scalaJSIRCacheHolder` for internal use. */
  private[sbtplugin] val scalaJSIRCacheHolderInternal = SettingKey[globalIRCache.Cache](
      "scalaJSIRCacheHolder",
      "Scala.js internal: Setting to persist a cache. Do NOT use this directly. " +
      "Use scalaJSIRCache instead.", KeyRanks.Invisible)

  @deprecated("Use scalaJSIRCache instead", "0.6.20")
  val scalaJSIRCacheHolder = scalaJSIRCacheHolderInternal

  val scalaJSIRCache: TaskKey[globalIRCache.Cache] =
    ScalaJSPlugin.autoImport.scalaJSIRCache

  /** Non-deprecated alias of `scalaJSRequestsDOM` for internal use. */
  private[sbtplugin] val scalaJSRequestsDOMInternal = TaskKey[Boolean](
      "scalaJSRequestsDOM",
      "Scala.js internal: Whether a project really wants the DOM. " +
      "Calculated using requiresDOM and jsDependencies", KeyRanks.Invisible)

  /** Internal task to calculate whether a project requests the DOM
   *  (through jsDependencies or requiresDOM) */
  @deprecated(
      "`scalaJSRequestsDOM` will always be false in new builds, because " +
      "`jsDependencies += RuntimeDOM` and `requiresDOM := true` are " +
      "deprecated. A better alternative to reading `scalaJSRequestsDOM` is " +
      "to detect whether `resolvedJSEnv` is a DOM-enabled JS env, or to use " +
      "your own setting key.",
      "0.6.20")
  val scalaJSRequestsDOM = scalaJSRequestsDOMInternal

  /** All .sjsir files on the fullClasspath, used by scalajsp. */
  val sjsirFilesOnClasspath: TaskKey[Seq[String]] =
    ScalaJSPlugin.autoImport.sjsirFilesOnClasspath

  /** Internal task to map discovered main classes to whether they are in the
   *  "new" style (true, standard main method) or the "old" style (false,
   *  `js.JSApp` or `main(): Unit` method).
   */
  val scalaJSDiscoveredMainClasses = TaskKey[Map[String, Boolean]](
      "scalaJSDiscoveredMainClasses",
      "Discovered main classes and whether they use the \"new\" style",
      KeyRanks.Invisible)

  val scalaJSModuleIdentifier = TaskKey[Option[String]](
      "scalaJSModuleIdentifier",
      "An identifier for the module which contains the exports of Scala.js",
      KeyRanks.Invisible)

  val scalaJSSourceFiles: AttributeKey[Seq[File]] =
    ScalaJSPlugin.autoImport.scalaJSSourceFiles

  val stageKeys: Map[Stage, TaskKey[Attributed[File]]] =
    ScalaJSPlugin.stageKeys

  /** A JS expression that detects the global scope just like Scala.js */
  val jsGlobalExpr: String = {
    """((typeof global === "object" && global &&
         global["Object"] === Object) ? global : this)"""
  }

  def logIRCacheStats(logger: Logger): Unit =
    ScalaJSPlugin.logIRCacheStats(logger)

  /** Patches the IncOptions so that .sjsir files are pruned as needed. */
  def scalaJSPatchIncOptions(incOptions: IncOptions): IncOptions =
    SBTCompat.scalaJSPatchIncOptions(incOptions)

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

      scalaJSLinkerConfig in key := {
        val opts = (scalaJSOptimizerOptions in key).value

        val semantics = (scalaJSSemantics in key).value
        val outputMode = (scalaJSOutputMode in key).value
        val moduleKind = scalaJSModuleKind.value // intentionally not 'in key'
        val withSourceMap = (emitSourceMaps in key).value

        /* For `relativizeSourceMapBase`, preserve the one in the new config
         * if it is set, otherwise fall back on the old config.
         */
        val oldConfigRelSourceMapBase = {
          if ((relativeSourceMaps in key).value)
            Some((artifactPath in key).value.toURI())
          else
            None
        }
        val newConfigRelSourceMapBase =
          (scalaJSLinkerConfig in key).value.relativizeSourceMapBase
        val relSourceMapBase =
          newConfigRelSourceMapBase.orElse(oldConfigRelSourceMapBase)

        StandardLinker.Config()
          .withSemantics(semantics)
          .withModuleKind(moduleKind)
          .withESFeatures(outputMode)
          .withBypassLinkingErrorsInternal(opts.bypassLinkingErrors)
          .withCheckIR(opts.checkScalaJSIR)
          .withOptimizer(!opts.disableOptimizer)
          .withParallel(opts.parallel)
          .withSourceMap(withSourceMap)
          .withRelativizeSourceMapBase(relSourceMapBase)
          .withClosureCompiler(opts.useClosureCompiler)
          .withCustomOutputWrapperInternal(scalaJSOutputWrapperInternal.value)
          .withPrettyPrint(opts.prettyPrintFullOptJS)
          .withBatchMode(opts.batchMode)
      },

      scalaJSLinker in key := {
        val config = (scalaJSLinkerConfig in key).value

        if (config.moduleKind != scalaJSModuleKind.value) {
          val projectID = thisProject.value.id
          val configName = configuration.value.name
          val keyName = key.key.label
          sLog.value.warn(
              s"The module kind in `scalaJSLinkerConfig in ($projectID, " +
              s"$configName, $keyName)` is different than the value of " +
              s"`scalaJSModuleKind in ($projectID, $configName)`. " +
              "Some things will go wrong.")
        }

        new ClearableLinker(() => StandardLinker(config), config.batchMode)
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
        }.tag(usesLinkerTag, ScalaJSTags.Link)
      }.value,

      key := key.dependsOn(packageJSDependencies, packageScalaJSLauncherInternal).value,

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

  private def ensureOldStyleMainClass(mainClass: String,
      scalaJSDiscoveredMainClassesValue: Map[String, Boolean]): Unit = {
    val newStyle = scalaJSDiscoveredMainClassesValue.getOrElse(mainClass, false)
    if (newStyle) {
      throw new MessageOnlyException(
          s"The main class $mainClass uses the new style with an " +
          "Array[String] argument, but scalaJSUseMainModuleInitializer is " +
          "false. Did you forget to specify `scalaJSUseMainModuleInitializer " +
          ":= true` in your project settings?")
    }
  }

  val scalaJSConfigSettings: Seq[Setting[_]] = Seq(
      incOptions ~= scalaJSPatchIncOptions
  ) ++ (
      scalajspSettings ++
      stageKeys.flatMap((scalaJSStageSettings _).tupled) ++
      dispatchTaskKeySettings(scalaJSLinkedFile) ++
      dispatchSettingKeySettings(scalaJSLinker) ++
      dispatchSettingKeySettings(usesScalaJSLinkerTag)
  ) ++ (
      Seq(fastOptJS, fullOptJS, packageScalaJSLauncherInternal,
          packageJSDependencies, packageMinifiedJSDependencies).map { key =>
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
      scalaJSIRCacheHolderInternal := newIRCache,
      scalaJSIRCache := scalaJSIRCacheHolderInternal.value,

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
        val moduleKind = (scalaJSModuleKind in fullOptJS).value
        val useClosure = {
          outputMode != OutputMode.ECMAScript51Global &&
          moduleKind != ModuleKind.ESModule
        }
        prev.withUseClosureCompiler(useClosure)
      },

      fullOptJS := fullOptJS.dependsOn(packageMinifiedJSDependencies).value,

      artifactPath in packageScalaJSLauncherInternal :=
        ((crossTarget in packageScalaJSLauncherInternal).value /
            ((moduleName in packageScalaJSLauncherInternal).value + "-launcher.js")),

      skip in packageScalaJSLauncherInternal := {
        // @sbtUnchecked because of https://github.com/sbt/sbt/issues/3299
        val value = !persistLauncherInternal.value
        if (!value) {
          if (scalaJSUseMainModuleInitializer.value: @sbtUnchecked) {
            throw new MessageOnlyException(
                "persistLauncher := true is not compatible with using a main " +
                "module initializer (scalaJSUseMainModuleInitializer := " +
                "true), nor is it necessary, since fastOptJS/fullOptJS " +
                "includes the call to the main method")
          } else if ((scalaJSModuleKind.value: @sbtUnchecked) != ModuleKind.NoModule) {
            throw new MessageOnlyException(
                "persistLauncher := true is not compatible with emitting " +
                "JavaScript modules")
          }
        }
        value
      },

      packageScalaJSLauncherInternal := Def.taskDyn {
        if ((skip in packageScalaJSLauncherInternal).value) {
          Def.task {
            Attributed.blank((artifactPath in packageScalaJSLauncherInternal).value)
          }
        } else {
          Def.task {
            val scalaJSDiscoveredMainClassesValue =
              scalaJSDiscoveredMainClasses.value
            mainClass.value map { mainCl =>
              ensureOldStyleMainClass(mainCl, scalaJSDiscoveredMainClassesValue)

              val file = (artifactPath in packageScalaJSLauncherInternal).value
              assert(scalaJSModuleKind.value == ModuleKind.NoModule,
                  "Cannot produce a launcher file when scalaJSModuleKind " +
                  "is different from NoModule")
              IO.write(file,
                  launcherContent(mainCl, ModuleKind.NoModule, None),
                  Charset.forName("UTF-8"))

              // Attach the name of the main class used, (ab?)using the name key
              Attributed(file)(AttributeMap.empty.put(name.key, mainCl))
            } getOrElse {
              throw new MessageOnlyException(
                  "Cannot write launcher file, since there is no or multiple " +
                  "mainClasses")
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
            jsDeps.toList, requiresDOM, compliantSemantics, internal = ())

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
          // @sbtUnchecked because of https://github.com/sbt/sbt/issues/3299
          checkCompliance(requirements, scalaJSSemantics.value: @sbtUnchecked)
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
          throw new MessageOnlyException("Scala.js cannot be run in a forked JVM")
        else
          true
      },

      scalaJSRequestsDOMInternal := {
        requiresDOMInternal.?.value.getOrElse(
            jsDependencyManifests.value.data.exists(_.requiresDOMInternal))
      },

      resolvedJSEnv := {
        val useRhino = scalaJSUseRhinoInternal.value
        val rhinoJSEnv = RhinoJSEnvInternal().value
        val requestsDOM = scalaJSRequestsDOMInternal.value

        jsEnv.?.value.getOrElse {
          if (useRhino) {
            rhinoJSEnv
          } else if (requestsDOM) {
            new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv()
          } else {
            new org.scalajs.jsenv.nodejs.NodeJSEnv()
          }
        }
      },

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
              val moduleInitializers = scalaJSModuleInitializers.value
              val unit = linker.linkUnit(ir, moduleInitializers,
                  env.symbolRequirements, sbtLogger2ToolsLogger(log))

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

      scalaJSModuleIdentifier := Def.settingDyn[Task[Option[String]]] {
        scalaJSModuleKind.value match {
          case ModuleKind.NoModule =>
            Def.task {
              None
            }

          case ModuleKind.ESModule =>
            Def.task {
              Some(scalaJSLinkedFile.value.toURI.toASCIIString)
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
    moduleKind match {
      case ModuleKind.NoModule =>
        jsGlobalExpr

      case ModuleKind.ESModule =>
        throw new MessageOnlyException(
            "Using a launcher file is not compatible with ES modules")

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

  @deprecated("js.JSApps are going away, and this method with them.", "0.6.18")
  def discoverJSApps(analysis: CompileAnalysis): Seq[String] = {
    discoverScalaJSMainClasses(analysis).collect {
      case (name, false) => name
    }.toList
  }

  private def discoverScalaJSMainClasses(
      analysis: CompileAnalysis): Map[String, Boolean] = {
    import xsbt.api.{Discovered, Discovery}

    val jsApp = "scala.scalajs.js.JSApp"

    def isJSApp(discovered: Discovered) =
      discovered.isModule && discovered.baseClasses.contains(jsApp)

    Map(Discovery(Set(jsApp), Set.empty)(Tests.allDefs(analysis)).collect {
      // Old-style first, so that in case of ambiguity, we keep backward compat
      case (definition, discovered) if isJSApp(discovered) =>
        definition.name -> false
      case (definition, discovered) if discovered.hasMain =>
        definition.name -> true
    }: _*)
  }

  private val runMainParser = {
    Defaults.loadForParser(discoveredMainClasses) { (_, names) =>
      val mainClasses = names.getOrElse(Nil).toSet
      Space ~> token(NotSpace examples mainClasses)
    }
  }

  // These settings will be filtered by the stage dummy tasks
  val scalaJSRunSettings = Seq(
      scalaJSDiscoveredMainClasses := {
        discoverScalaJSMainClasses(compile.value)
      },

      discoveredMainClasses := {
        scalaJSDiscoveredMainClasses.map(_.keys.toList.sorted: Seq[String])
          .storeAs(discoveredMainClasses).triggeredBy(compile).value
      },

      scalaJSMainModuleInitializer := {
        val allDiscoveredMainClasses = scalaJSDiscoveredMainClasses.value
        mainClass.value.map { mainCl =>
          val newStyleMain = allDiscoveredMainClasses.getOrElse(mainCl, false)
          if (newStyleMain)
            ModuleInitializer.mainMethodWithArgs(mainCl, "main")
          else
            ModuleInitializer.mainMethod(mainCl, "main")
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

      mainClass in scalaJSLauncherInternal := (mainClass in run).value,
      scalaJSLauncherInternal := Def.settingDyn[Task[Attributed[VirtualJSFile]]] {
        if (persistLauncherInternal.value) {
          Def.task {
            packageScalaJSLauncherInternal.value.map(FileVirtualJSFile)
          }
        } else if (scalaJSUseMainModuleInitializer.value) {
          Def.task {
            val base = Attributed.blank[VirtualJSFile](
                new MemVirtualJSFile("No-op generated launcher file"))
            mainClass.value.fold {
              base
            } { mainClass =>
              base.put(name.key, mainClass)
            }
          }
        } else {
          Def.task {
            val moduleKind = scalaJSModuleKind.value
            val moduleIdentifier = scalaJSModuleIdentifier.value
            val scalaJSDiscoveredMainClassesValue =
              scalaJSDiscoveredMainClasses.value

            (mainClass in scalaJSLauncherInternal).value.fold {
              throw new MessageOnlyException("No main class detected.")
            } { mainClass =>
              ensureOldStyleMainClass(mainClass, scalaJSDiscoveredMainClassesValue)
              val memLaunch =
                memLauncher(mainClass, moduleKind, moduleIdentifier)
              Attributed[VirtualJSFile](memLaunch)(
                  AttributeMap.empty.put(name.key, mainClass))
            }
          }
        }
      }.value,

      run := {
        // use assert to prevent warning about pure expr in stat pos
        assert(scalaJSEnsureUnforked.value)

        val launch = scalaJSLauncherInternal.value
        val className = launch.get(name.key).getOrElse("<unknown class>")
        jsRun(loadedJSEnv.value, className, launch.data,
            streams.value.log, scalaJSConsole.value)
      },

      runMain := {
        // use assert to prevent warning about pure expr in stat pos
        assert(scalaJSEnsureUnforked.value)

        val mainClass = runMainParser.parsed

        if (scalaJSUseMainModuleInitializer.value) {
          throw new MessageOnlyException(
              "`runMain` is not supported when using a main module " +
              "initializer (scalaJSUseMainModuleInitializer := true) since " +
              "the (unique) entry point is burned in the fastOptJS/fullOptJS.")
        }

        val scalaJSDiscoveredMainClassesValue =
          scalaJSDiscoveredMainClasses.value
        val newStyleMain = scalaJSDiscoveredMainClassesValue.getOrElse(
            mainClass, false)
        if (newStyleMain) {
          throw new MessageOnlyException(
              "`runMain` is not supported when using a main class with an " +
              "Array[String] argument. It is only supported for objects that " +
              "extend js.JSApp.")
        }

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
        val frameworks = testFrameworks.value

        val jsEnv = loadedJSEnv.value match {
          case jsEnv: ComJSEnv => jsEnv

          case jsEnv =>
            throw new MessageOnlyException(
                s"You need a ComJSEnv to test (found ${jsEnv.name})")
        }

        val moduleKind = scalaJSModuleKind.value
        val moduleIdentifier = scalaJSModuleIdentifier.value
        val frameworkNames = frameworks.map(_.implClassNames.toList).toList

        val config = TestAdapter.Config()
          .withLogger(sbtLogger2ToolsLogger(logger))
          .withJSConsole(console)
          .withModuleSettings(moduleKind, moduleIdentifier)

        val adapter = newTestAdapter(jsEnv, config)
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
      }
  )

  val scalaJSTestBuildSettings = (
      scalaJSConfigSettings
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
      artifactPath in testHtmlFastOpt := {
        val config = configuration.value.name
        ((crossTarget in testHtmlFastOpt).value /
            ((moduleName in testHtmlFastOpt).value + s"-fastopt-$config.html"))
      },
      artifactPath in testHtmlFullOpt := {
        val config = configuration.value.name
        ((crossTarget in testHtmlFullOpt).value /
            ((moduleName in testHtmlFullOpt).value + s"-opt-$config.html"))
      }
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
  ) ++ Seq(
      /* Always default to false for scalaJSUseMainModuleInitializer and
       * persistLauncher in testing configurations, even if it is true in the
       * Global configuration scope.
       */
      scalaJSUseMainModuleInitializer := false,
      persistLauncherInternal := false
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

  /* As of sbt 1, a `config()` must be assigned to a `val` starting with an
   * uppercase letter, which will become the "id" of the configuration.
   */
  val PhantomJSJetty = config("phantom-js-jetty").hide

  val scalaJSProjectBaseSettings = Seq(
      platformDepsCrossVersion := ScalaJSCrossVersion.binary,
      isScalaJSProject := true,

      /* We first define scalaJSLinkerConfig in the project scope, with all
       * the defaults. Later, we derive all the old config options in the
       * project scope from scalaJSLinkerConfig.
       *
       * At the end of the day, in the fully qualified scope
       * (project, config, linkKey), we re-derive scalaJSLinkerConfig from all
       * the old config keys.
       *
       * This effectively gives meaning to scalaJSLinkerConfig in the project
       * scope and in the fully qualified scope, but not in-between. Changes
       * to `scalaJSLinkerConfig in (project, config)` will not have any
       * effect.
       *
       * This is a compromise to ensure backward compatibility of using the old
       * options in all cases, and a reasonable way to use the new options
       * for typical use cases.
       *
       * `relativeSourceMaps`/`scalaJSLinkerConfig.relativizeSourceMapBase` is
       * an exception. We cannot derive `relativizeSourceMapBase` only from
       * `relativeSourceMaps`, and deriving `relativeSourceMaps` from
       * `relativizeSourceMapBase` would lose information. Instead, we keep
       * `relativeSourceMaps` to its default `false` in the project scope,
       * irrespective of `scalaJSLinkerConfig`. And in the fully qualified
       * scope, *if* `relativeSourceMaps` is true, we set
       * `relativeSourceMapBase`, otherwise we leave it untouched. This
       * provides the same compatibility/usability features.
       */
      scalaJSLinkerConfig := {
        StandardLinker.Config()
          .withParallel(OptimizerOptions.DefaultParallel)
      },

      relativeSourceMaps := false,
      persistLauncherInternal := false,

      emitSourceMaps := scalaJSLinkerConfig.value.sourceMap,

      scalaJSOutputWrapperInternal :=
        scalaJSLinkerConfig.value.customOutputWrapper,

      scalaJSOptimizerOptions := {
        val config = scalaJSLinkerConfig.value
        OptimizerOptions()
          .withBypassLinkingErrorsInternal(config.bypassLinkingErrors)
          .withParallel(config.parallel)
          .withBatchMode(config.batchMode)
          .withDisableOptimizer(!config.optimizer)
          .withPrettyPrintFullOptJS(config.prettyPrint)
          .withCheckScalaJSIR(config.checkIR)
          .withUseClosureCompiler(config.closureCompiler)
      },

      jsDependencies := Seq(),
      jsDependencyFilter := identity,
      jsManifestFilter := identity,

      scalaJSSemantics := scalaJSLinkerConfig.value.semantics,
      scalaJSOutputMode := scalaJSLinkerConfig.value.esFeatures,
      scalaJSModuleKind := scalaJSLinkerConfig.value.moduleKind,
      checkScalaJSSemantics := true,

      scalaJSModuleInitializers := Seq(),
      scalaJSUseMainModuleInitializer := false,

      scalaJSConsole := ConsoleJSConsole,

      /* Depend on jetty artifacts in dummy configuration to be able to inject
       * them into the PhantomJS runner if necessary.
       * See scalaJSPhantomJSClassLoader
       */
      ivyConfigurations += PhantomJSJetty,
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
          // as well as the test-bridge in the Test configuration
          "org.scala-js" %% "scalajs-test-bridge" % scalaJSVersion % "test"
      ),

      // and you will want to be cross-compiled on the Scala.js binary version
      crossVersion := ScalaJSCrossVersion.binary
  )

}
