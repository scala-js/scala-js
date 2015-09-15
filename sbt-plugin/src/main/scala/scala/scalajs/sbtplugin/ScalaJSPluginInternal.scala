package org.scalajs.sbtplugin

import sbt._
import sbt.inc.{IncOptions, ClassfileManager}
import Keys._
import sbinary.DefaultProtocol._
import Cache.seqFormat
import complete.Parser
import complete.DefaultParsers._

import Implicits._

import org.scalajs.core.tools.sem.Semantics
import org.scalajs.core.tools.javascript.OutputMode
import org.scalajs.core.tools.io.{IO => toolsIO, _}
import org.scalajs.core.tools.classpath._
import org.scalajs.core.tools.classpath.builder._
import org.scalajs.core.tools.jsdep._
import org.scalajs.core.tools.optimizer._
import org.scalajs.core.tools.corelib.CoreJSLibs

import org.scalajs.jsenv._
import org.scalajs.jsenv.rhino.RhinoJSEnv
import org.scalajs.jsenv.nodejs.NodeJSEnv
import org.scalajs.jsenv.phantomjs.{PhantomJSEnv, PhantomJettyClassLoader}

import org.scalajs.core.ir
import org.scalajs.core.ir.Utils.escapeJS
import org.scalajs.core.ir.ScalaJSVersions
import org.scalajs.core.ir.Printers.{InfoPrinter, IRTreePrinter}

import org.scalajs.testadapter.ScalaJSFramework

import scala.util.Try

import java.nio.charset.Charset
import java.net.URLClassLoader

/** Contains settings used by ScalaJSPlugin that should not be automatically
 *  be in the *.sbt file's scope.
 */
object ScalaJSPluginInternal {

  import ScalaJSPlugin.autoImport._

  /** Dummy setting to ensure we do not fork in Scala.js run & test. */
  val scalaJSEnsureUnforked = SettingKey[Boolean]("ensureUnforked",
      "Scala.js internal: Fails if fork is true.", KeyRanks.Invisible)

  /** Dummy setting to persist a Scala.js linker. */
  val scalaJSLinker = SettingKey[BaseLinker]("scalaJSLinker",
      "Scala.js internal: Setting to persist a linker", KeyRanks.Invisible)

  /** Dummy setting to persist Scala.js optimizer */
  val scalaJSOptimizer = SettingKey[ScalaJSOptimizer]("scalaJSOptimizer",
      "Scala.js internal: Setting to persist the optimizer", KeyRanks.Invisible)

  /** Internal task to compute the LinkingUnitClasspath for Rhino. */
  val scalaJSLinkingUnitClasspath = TaskKey[LinkingUnitClasspath]("scalaJSLinkingUnitClasspath",
      "Resolved classpath represented as a LinkedUnit", KeyRanks.Invisible)

  /** Internal task to calculate whether a project requests the DOM
   *  (through jsDependencies or requiresDOM) */
  val scalaJSRequestsDOM = TaskKey[Boolean]("scalaJSRequestsDOM",
      "Scala.js internal: Whether a project really wants the DOM. " +
      "Calculated using requiresDOM and jsDependencies", KeyRanks.Invisible)

  /** Default post link environment */
  val scalaJSDefaultPostLinkJSEnv = TaskKey[JSEnv]("scalaJSDefaultPostLinkJSEnv",
      "Scala.js internal: Default for postLinkJSEnv", KeyRanks.Invisible)

  /** Lookup key for CompleteClasspath in attribute maps */
  val scalaJSCompleteClasspath =
      AttributeKey[CompleteClasspath]("scalaJSCompleteClasspath")

  /** All .sjsir files on the fullClasspath, used by scalajsp. */
  val sjsirFilesOnClasspath = TaskKey[List[String]]("sjsirFilesOnClasspath",
      "All .sjsir files on the fullClasspath, used by scalajsp",
      KeyRanks.Invisible)

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
    taskKey <<= Def.taskDyn {
      if ((skip in taskKey).value)
        Def.task((artifactPath in taskKey).value)
      else Def.task {
        val cp = scalaJSPreLinkClasspath.value
        val output = (artifactPath in taskKey).value
        val taskCache = WritableFileVirtualTextFile(
            streams.value.cacheDirectory / cacheName)

        IO.createDirectory(output.getParentFile)

        val outFile = AtomicWritableFileVirtualJSFile(output)
        CacheUtils.cached(cp.version, outFile, Some(taskCache)) {
          toolsIO.concatFiles(outFile, cp.jsLibs.map(getLib))
        }

        output
      }
    }
  }

  private def scalaJSOptimizerSetting(key: TaskKey[_]): Setting[_] = (
      scalaJSOptimizer in key := {
        val semantics = (scalaJSSemantics in key).value
        val outputMode = (scalaJSOutputMode in key).value
        if ((scalaJSOptimizerOptions in key).value.parallel)
          new ScalaJSOptimizer(semantics, outputMode, ParIncOptimizer.factory)
        else
          new ScalaJSOptimizer(semantics, outputMode, IncOptimizer.factory)
      }
  )

  private def scalajspSettings: Seq[Setting[_]] = {
    case class Options(
        infos: Boolean = false,
        showReflProxy: Boolean = false
    )

    val optionsParser: Parser[Options] = {
      token(OptSpace ~> (
          (literal("-i") | "--infos") ^^^ ((_: Options).copy(infos = true))
        | (literal("-p") | "--reflProxies") ^^^ ((_: Options).copy(showReflProxy = true))
      )).* map {
        fns => Function.chain(fns)(Options())
      }
    }

    def sjsirFileOnClasspathParser(
        relPaths: List[String]): Parser[String] = {
      OptSpace ~> StringBasic
        .examples(ScalajspUtils.relPathsExamples(relPaths))
    }

    def scalajspParser(state: State, relPaths: List[String]) =
      optionsParser ~ sjsirFileOnClasspathParser(relPaths)

    val parser = loadForParser(sjsirFilesOnClasspath) { (state, relPaths) =>
      scalajspParser(state, relPaths.getOrElse(Nil))
    }

    Seq(
        sjsirFilesOnClasspath <<= Def.task {
          val cp = Attributed.data(fullClasspath.value)
          ScalajspUtils.listSjsirFilesOnClasspath(cp)
        } storeAs(sjsirFilesOnClasspath) triggeredBy(fullClasspath),

        scalajsp := {
          val (options, relPath) = parser.parsed

          val cp = Attributed.data(fullClasspath.value)
          val vfile = ScalajspUtils.loadIRFile(cp, relPath)

          val stdout = new java.io.PrintWriter(System.out)
          if (options.infos) {
            new InfoPrinter(stdout).printClassInfo(vfile.info)
          } else {
            val (info, tree) = vfile.infoAndTree
            val outTree = {
              if (options.showReflProxy) tree
              else filterOutReflProxies(tree)
            }
            new IRTreePrinter(stdout).printTopLevelTree(outTree)
          }
          stdout.flush()
        }
    )
  }

  // !!! CODE DUPLICATION with Scalajsp.filterOutReflProxies
  private def filterOutReflProxies(tree: ir.Trees.ClassDef): ir.Trees.ClassDef = {
    import ir.Trees._
    import ir.Definitions.isReflProxyName
    val newDefs = tree.defs.filter {
      case MethodDef(_, Ident(name, _), _, _, _) => !isReflProxyName(name)
      case _ => true
    }
    tree.copy(defs = newDefs)(tree.optimizerHints)(tree.pos)
  }

  val scalaJSConfigSettings: Seq[Setting[_]] = Seq(
      incOptions ~= scalaJSPatchIncOptions
  ) ++ (
      scalajspSettings
  ) ++ Seq(

      scalaJSPreLinkClasspath := {
        val cp = fullClasspath.value
        val pcp = PartialClasspathBuilder.build(Attributed.data(cp).toList)
        val ccp = pcp.resolve(jsDependencyFilter.value, jsManifestFilter.value)

        if (checkScalaJSSemantics.value)
          ccp.checkCompliance(scalaJSSemantics.value)

        ccp
      },

      scalaJSLinker in scalaJSLinkingUnitClasspath := {
        val semantics = scalaJSSemantics.value
        val outputMode = scalaJSOutputMode.value
        new BaseLinker(semantics, outputMode, considerPositions = true)
      },

      scalaJSLinkingUnitClasspath := {
        // TODO wire this through the top-level linker

        val s = streams.value
        val cp = scalaJSPreLinkClasspath.value
        val opts = scalaJSOptimizerOptions.value

        val linker = (scalaJSLinker in scalaJSLinkingUnitClasspath).value


        val linkingUnit = linker.link(
            cp.scalaJSIR,
            s.log,
            reachOptimizerSymbols = true, // better be safe than sorry here
            bypassLinkingErrors = opts.bypassLinkingErrors,
            checkIR = opts.checkScalaJSIR)
        new LinkingUnitClasspath(cp.jsLibs, linkingUnit, cp.requiresDOM,
            cp.version)
      },

      artifactPath in fastOptJS :=
        ((crossTarget in fastOptJS).value /
            ((moduleName in fastOptJS).value + "-fastopt.js")),

      scalaJSOptimizerSetting(fastOptJS),

      fastOptJS := {
        val s = streams.value
        val output = (artifactPath in fastOptJS).value
        val taskCache =
          WritableFileVirtualTextFile(s.cacheDirectory / "fastopt-js")

        IO.createDirectory(output.getParentFile)

        val relSourceMapBase =
          if ((relativeSourceMaps in fastOptJS).value)
            Some(output.getParentFile.toURI())
          else None

        val opts = (scalaJSOptimizerOptions in fastOptJS).value

        import ScalaJSOptimizer._
        val outCP = (scalaJSOptimizer in fastOptJS).value.optimizeCP(
            (scalaJSPreLinkClasspath in fastOptJS).value,
            Config(AtomicWritableFileVirtualJSFile(output))
              .withCache(Some(taskCache))
              .withWantSourceMap((emitSourceMaps in fastOptJS).value)
              .withRelativizeSourceMapBase(relSourceMapBase)
              .withBypassLinkingErrors(opts.bypassLinkingErrors)
              .withCheckIR(opts.checkScalaJSIR)
              .withDisableOptimizer(opts.disableOptimizer)
              .withBatchMode(opts.batchMode)
              .withCustomOutputWrapper(scalaJSOutputWrapper.value),
            s.log)

         Attributed.blank(output).put(scalaJSCompleteClasspath, outCP)
      },
      fastOptJS <<=
        fastOptJS.dependsOn(packageJSDependencies, packageScalaJSLauncher),

      artifactPath in fullOptJS :=
        ((crossTarget in fullOptJS).value /
            ((moduleName in fullOptJS).value + "-opt.js")),

      scalaJSSemantics in fullOptJS :=
        (scalaJSSemantics in fastOptJS).value.optimized,

      scalaJSOptimizerSetting(fullOptJS),

      fullOptJS := {
        val s = streams.value
        val output = (artifactPath in fullOptJS).value
        val taskCache =
          WritableFileVirtualTextFile(s.cacheDirectory / "fullopt-js")

        IO.createDirectory(output.getParentFile)

        val relSourceMapBase =
          if ((relativeSourceMaps in fullOptJS).value)
            Some(output.getParentFile.toURI())
          else None

        val opts = (scalaJSOptimizerOptions in fullOptJS).value

        import ScalaJSClosureOptimizer._
        val outCP = new ScalaJSClosureOptimizer().optimizeCP(
            (scalaJSOptimizer in fullOptJS).value,
            (scalaJSPreLinkClasspath in fullOptJS).value,
            Config(AtomicWritableFileVirtualJSFile(output))
              .withCache(Some(taskCache))
              .withWantSourceMap((emitSourceMaps in fullOptJS).value)
              .withRelativizeSourceMapBase(relSourceMapBase)
              .withBypassLinkingErrors(opts.bypassLinkingErrors)
              .withCheckIR(opts.checkScalaJSIR)
              .withDisableOptimizer(opts.disableOptimizer)
              .withBatchMode(opts.batchMode)
              .withCustomOutputWrapper(scalaJSOutputWrapper.value)
              .withPrettyPrint(opts.prettyPrintFullOptJS),
            s.log)

        Attributed.blank(output).put(scalaJSCompleteClasspath, outCP)
      },
      fullOptJS <<=
        fullOptJS.dependsOn(packageJSDependencies, packageMinifiedJSDependencies,
            packageScalaJSLauncher),

      artifactPath in packageScalaJSLauncher :=
        ((crossTarget in packageScalaJSLauncher).value /
            ((moduleName in packageScalaJSLauncher).value + "-launcher.js")),

      skip in packageScalaJSLauncher := !persistLauncher.value,

      packageScalaJSLauncher <<= Def.taskDyn {
        if ((skip in packageScalaJSLauncher).value)
          Def.task(Attributed.blank((artifactPath in packageScalaJSLauncher).value))
        else Def.task {
          mainClass.value map { mainCl =>
            val file = (artifactPath in packageScalaJSLauncher).value
            IO.write(file, launcherContent(mainCl), Charset.forName("UTF-8"))

            // Attach the name of the main class used, (ab?)using the name key
            Attributed(file)(AttributeMap.empty.put(name.key, mainCl))
          } getOrElse {
            sys.error("Cannot write launcher file, since there is no or multiple mainClasses")
          }
        }
      },

      artifactPath in packageJSDependencies :=
        ((crossTarget in packageJSDependencies).value /
            ((moduleName in packageJSDependencies).value + "-jsdeps.js")),

      packageJSDependenciesSetting(packageJSDependencies, "package-js-deps", _.lib),

      artifactPath in packageMinifiedJSDependencies :=
        ((crossTarget in packageJSDependencies).value /
            ((moduleName in packageJSDependencies).value + "-jsdeps.min.js")),

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

      products <<= products.dependsOn(jsDependencyManifest),

      console <<= console.dependsOn(Def.task(
          streams.value.log.warn("Scala REPL doesn't work with Scala.js. You " +
              "are running a JVM REPL. JavaScript things won't work.")
      )),

      // Give tasks ability to check we are not forking at build reading time
      scalaJSEnsureUnforked := {
        if (fork.value)
          sys.error("Scala.js cannot be run in a forked JVM")
        else
          true
      },

      scalaJSRequestsDOM :=
        requiresDOM.?.value.getOrElse(scalaJSExecClasspath.value.requiresDOM),

      // Default jsEnv
      jsEnv <<= Def.taskDyn {
        scalaJSStage.value match {
          case Stage.PreLink =>
            Def.task {
              preLinkJSEnv.?.value.getOrElse {
                new RhinoJSEnv(scalaJSSemantics.value,
                    withDOM = scalaJSRequestsDOM.value)
              }
            }
          case Stage.FastOpt | Stage.FullOpt =>
            Def.task(scalaJSDefaultPostLinkJSEnv.value)
        }
      },

      // Wire jsEnv and sources for other stages
      scalaJSDefaultPostLinkJSEnv := postLinkJSEnv.?.value.getOrElse {
        if (scalaJSRequestsDOM.value)
          new PhantomJSEnv(jettyClassLoader = scalaJSPhantomJSClassLoader.value)
        else
          new NodeJSEnv
      },

      scalaJSExecClasspath <<= Def.taskDyn {
        scalaJSStage.value match {
          case Stage.PreLink =>
            Def.task { scalaJSLinkingUnitClasspath.value }
          case Stage.FastOpt =>
            Def.task { fastOptJS.value.get(scalaJSCompleteClasspath).get }
          case Stage.FullOpt =>
            Def.task { fullOptJS.value.get(scalaJSCompleteClasspath).get }
        }
      }
  )

  /** Run a class in a given environment using a given launcher */
  private def jsRun(env: JSEnv, cp: CompleteClasspath, mainCl: String,
      launcher: VirtualJSFile, jsConsole: JSConsole, log: Logger) = {

    log.info("Running " + mainCl)
    log.debug(s"with JSEnv of type ${env.getClass()}")
    log.debug(s"with classpath of type ${cp.getClass}")

    // Actually run code
    env.jsRunner(cp, launcher, log, jsConsole).run()
  }

  private def launcherContent(mainCl: String) = {
    val parts = mainCl.split('.').map(s => s"""["${escapeJS(s)}"]""").mkString
    s"${CoreJSLibs.jsGlobalExpr}$parts().main();\n"
  }

  private def memLauncher(mainCl: String) = {
    new MemVirtualJSFile("Generated launcher file")
      .withContent(launcherContent(mainCl))
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
      scalaJSLauncher <<= Def.taskDyn {
        if (persistLauncher.value)
          Def.task(packageScalaJSLauncher.value.map(FileVirtualJSFile))
        else Def.task {
          (mainClass in scalaJSLauncher).value map { mainClass =>
            val memLaunch = memLauncher(mainClass)
            Attributed[VirtualJSFile](memLaunch)(
                AttributeMap.empty.put(name.key, mainClass))
          } getOrElse {
            sys.error("No main class detected.")
          }
        }
      },

      discoveredMainClasses <<= compile.map(discoverJSApps).
        storeAs(discoveredMainClasses).triggeredBy(compile),

      run <<= Def.inputTask {
        // use assert to prevent warning about pure expr in stat pos
        assert(scalaJSEnsureUnforked.value)

        val launch = scalaJSLauncher.value
        val className = launch.get(name.key).getOrElse("<unknown class>")
        jsRun(jsEnv.value, scalaJSExecClasspath.value, className,
            launch.data, scalaJSConsole.value, streams.value.log)
      },

      runMain := {
        // use assert to prevent warning about pure expr in stat pos
        assert(scalaJSEnsureUnforked.value)

        val mainClass = runMainParser.parsed
        jsRun(jsEnv.value, scalaJSExecClasspath.value, mainClass,
            memLauncher(mainClass), scalaJSConsole.value, streams.value.log)
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

        val env = jsEnv.value match {
          case env: ComJSEnv => env
          case _ =>
            sys.error("You need a ComJSEnv to test")
        }

        val classpath = scalaJSExecClasspath.value
        val detector = new FrameworkDetector(env, classpath)
        val console = scalaJSConsole.value
        val logger = streams.value.log

        detector.detect(testFrameworks.value) map { case (tf, name) =>
          (tf, new ScalaJSFramework(name, env, classpath, logger, console))
        }
      },
      // Override default to avoid triggering a test:fastOptJS in a test:compile
      // without loosing autocompetion.
      definedTestNames <<= definedTests map (_.map(_.name).distinct)
        storeAs definedTestNames triggeredBy scalaJSExecClasspath
  )

  val scalaJSTestBuildSettings = (
      scalaJSConfigSettings
  ) ++ (
      Seq(fastOptJS, fullOptJS, packageScalaJSLauncher,
          packageJSDependencies) map { packageJSTask =>
        moduleName in packageJSTask := moduleName.value + "-test"
      }
  )

  val scalaJSTestSettings = (
      scalaJSTestBuildSettings ++
      scalaJSTestFrameworkSettings
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
      relativeSourceMaps   := false,
      persistLauncher      := false,

      emitSourceMaps := true,

      scalaJSOutputWrapper := ("", ""),

      scalaJSOptimizerOptions := OptimizerOptions(),

      jsDependencies := Seq(),
      jsDependencyFilter := identity,
      jsManifestFilter := identity,

      scalaJSSemantics := Semantics.Defaults,
      scalaJSOutputMode := OutputMode.ECMAScript51Isolated,
      checkScalaJSSemantics := true,

      scalaJSConsole := ConsoleJSConsole,

      clean <<= clean.dependsOn(Def.task {
        // have clean reset incremental optimizer state
        (scalaJSOptimizer in (Compile, fastOptJS)).value.clean()
        (scalaJSOptimizer in (Test, fastOptJS)).value.clean()
        (scalaJSOptimizer in (Compile, fullOptJS)).value.clean()
        (scalaJSOptimizer in (Test, fullOptJS)).value.clean()
      }),

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
      }
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

      // and of course the Scala.js library
      libraryDependencies += "org.scala-js" %% "scalajs-library" % scalaJSVersion,

      // and you will want to be cross-compiled on the Scala.js binary version
      crossVersion := ScalaJSCrossVersion.binary
  )

}
