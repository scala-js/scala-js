package scala.scalajs.sbtplugin

import sbt._
import sbt.inc.{IncOptions, ClassfileManager}
import Keys._

import Implicits._
import JSUtils._

import scala.scalajs.tools.sem.Semantics
import scala.scalajs.tools.io.{IO => toolsIO, _}
import scala.scalajs.tools.classpath._
import scala.scalajs.tools.classpath.builder._
import scala.scalajs.tools.jsdep._
import scala.scalajs.tools.optimizer.{
  ScalaJSOptimizer,
  ScalaJSClosureOptimizer,
  IncOptimizer,
  ParIncOptimizer
}
import scala.scalajs.tools.corelib.CoreJSLibs

import scala.scalajs.tools.env._
import scala.scalajs.sbtplugin.env.rhino.RhinoJSEnv
import scala.scalajs.sbtplugin.env.nodejs.NodeJSEnv
import scala.scalajs.sbtplugin.env.phantomjs.{PhantomJSEnv, PhantomJettyClassLoader}

import scala.scalajs.ir.ScalaJSVersions

import scala.scalajs.sbtplugin.testing.{TestFramework, JSClasspathLoader}

import scala.util.Try

import java.nio.charset.Charset
import java.net.URLClassLoader

/** Contains settings used by ScalaJSPlugin that should not be automatically
 *  be in the *.sbt file's scope.
 */
object ScalaJSPluginInternal {

  import ScalaJSPlugin.ScalaJSKeys._
  import ScalaJSPlugin.scalaJSVersion

  /** Dummy setting to ensure we do not fork in Scala.js run & test. */
  val scalaJSEnsureUnforked = SettingKey[Boolean]("ensureUnforked",
      "Scala.js internal: Fails if fork is true.", KeyRanks.Invisible)

  /** Dummy setting to persist Scala.js optimizer */
  val scalaJSOptimizer = SettingKey[ScalaJSOptimizer]("scalaJSOptimizer",
      "Scala.js internal: Setting to persist the optimizer", KeyRanks.Invisible)

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
    incOptions.copy(newClassfileManager = newClassfileManager)
  }

  private def scalaJSOptimizerSetting(key: TaskKey[_]): Setting[_] = (
      scalaJSOptimizer in key := {
        val semantics = (scalaJSSemantics in key).value
        if ((scalaJSOptimizerOptions in key).value.parallel)
          new ScalaJSOptimizer(semantics, new ParIncOptimizer(_))
        else
          new ScalaJSOptimizer(semantics, new IncOptimizer(_))
      }
  )

  val scalaJSConfigSettings: Seq[Setting[_]] = Seq(
      incOptions ~= scalaJSPatchIncOptions
  ) ++ Seq(

      scalaJSPreLinkClasspath := {
        val semantics = scalaJSSemantics.value
        val cp = fullClasspath.value
        val pcp = PartialClasspathBuilder.build(Attributed.data(cp).toList)
        pcp.resolve(jsDependencyFilter.value)
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
            Inputs(input = (scalaJSPreLinkClasspath in fastOptJS).value),
            OutputConfig(
                output = WritableFileVirtualJSFile(output),
                cache = Some(taskCache),
                wantSourceMap = (emitSourceMaps in fastOptJS).value,
                relativizeSourceMapBase = relSourceMapBase,
                checkIR = opts.checkScalaJSIR,
                disableOptimizer = opts.disableOptimizer,
                batchMode = opts.batchMode),
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

        val semantics = (scalaJSSemantics in fullOptJS).value

        import ScalaJSClosureOptimizer._
        val outCP = new ScalaJSClosureOptimizer(semantics).optimizeCP(
            (scalaJSOptimizer in fullOptJS).value,
            Inputs(ScalaJSOptimizer.Inputs(
                input = (scalaJSPreLinkClasspath in fullOptJS).value)),
            OutputConfig(
                output = WritableFileVirtualJSFile(output),
                cache = Some(taskCache),
                wantSourceMap = (emitSourceMaps in fullOptJS).value,
                relativizeSourceMapBase = relSourceMapBase,
                checkIR = opts.checkScalaJSIR,
                disableOptimizer = opts.disableOptimizer,
                batchMode = opts.batchMode,
                prettyPrint = opts.prettyPrintFullOptJS),
            s.log)

        Attributed.blank(output).put(scalaJSCompleteClasspath, outCP)
      },

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

      packageJSDependencies <<= Def.taskDyn {
        if ((skip in packageJSDependencies).value)
          Def.task((artifactPath in packageJSDependencies).value)
        else Def.task {
          val cp = scalaJSPreLinkClasspath.value
          val output = (artifactPath in packageJSDependencies).value
          val taskCache = WritableFileVirtualJSFile(
              streams.value.cacheDirectory / "package-js-deps")

          IO.createDirectory(output.getParentFile)

          val outFile = WritableFileVirtualTextFile(output)
          CacheUtils.cached(cp.version, outFile, Some(taskCache)) {
            toolsIO.concatFiles(outFile, cp.jsLibs.map(_.lib))
          }

          output
        }
      },

      jsDependencyManifest := {
        val myModule = thisProject.value.id
        val config = configuration.value.name

        // Collect all libraries
        val jsDeps = jsDependencies.value.collect {
          case dep: JSModuleID if dep.configurations.forall(_ == config) =>
            dep.jsDep
        }

        val requiresDOM = jsDependencies.value.exists {
          case RuntimeDOM(configurations) =>
            configurations.forall(_ == config)
          case _ => false
        }

        val manifest = new JSDependencyManifest(new Origin(myModule, config),
            jsDeps.toList, requiresDOM)

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
      jsEnv := preLinkJSEnv.?.value.getOrElse {
        new RhinoJSEnv(scalaJSSemantics.value,
            withDOM = scalaJSRequestsDOM.value)
      },

      // Wire jsEnv and sources for other stages
      scalaJSDefaultPostLinkJSEnv := postLinkJSEnv.?.value.getOrElse {
        if (scalaJSRequestsDOM.value)
          new PhantomJSEnv(jettyClassLoader = scalaJSPhantomJSClassLoader.value)
        else
          new NodeJSEnv
      },

      jsEnv in fastOptStage <<= scalaJSDefaultPostLinkJSEnv,
      jsEnv in fullOptStage <<= scalaJSDefaultPostLinkJSEnv,

      // Define execution classpaths
      scalaJSExecClasspath                 := scalaJSPreLinkClasspath.value,
      scalaJSExecClasspath in fastOptStage := fastOptJS.value.get(scalaJSCompleteClasspath).get,
      scalaJSExecClasspath in fullOptStage := fullOptJS.value.get(scalaJSCompleteClasspath).get,

      // Dummy task need dummy tags (used for concurrency restrictions)
      tags in fastOptStage := Seq(),
      tags in fullOptStage := Seq()
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
    // If we are running in Node.js, we need to bracket select on
    // global rather than this
    """((typeof global === "object" && global &&
         global["Object"] === Object) ? global : this)""" +
    s"${dot2bracket(mainCl)}().main();\n"
  }

  private def memLauncher(mainCl: String) = {
    new MemVirtualJSFile("Generated launcher file")
      .withContent(launcherContent(mainCl))
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

      /* We do currently not discover objects containing a
       *
       *   def main(args: Array[String]): Unit
       *
       * Support will be added again, as soon as we can run them
       * reliably (e.g. without implicitly requiring that an exported
       *
       *   def main(): Unit
       *
       * exists alongside.
       */
      discoveredMainClasses := {
        import xsbt.api.{Discovered, Discovery}

        val jsApp = "scala.scalajs.js.JSApp"

        def isJSApp(discovered: Discovered) =
          discovered.isModule && discovered.baseClasses.contains(jsApp)

        Discovery(Set(jsApp), Set.empty)(Tests.allDefs(compile.value)) collect {
          case (definition, discovered) if isJSApp(discovered) =>
            definition.name
        }
      },

      run <<= Def.inputTask {
        // use assert to prevent warning about pure expr in stat pos
        assert(scalaJSEnsureUnforked.value)

        val launch = scalaJSLauncher.value
        val className = launch.get(name.key).getOrElse("<unknown class>")
        jsRun(jsEnv.value, scalaJSExecClasspath.value, className,
            launch.data, scalaJSConsole.value, streams.value.log)
      },

      runMain <<= {
        // Implicits for parsing
        import sbinary.DefaultProtocol.StringFormat
        import Cache.seqFormat

        val parser = Defaults.loadForParser(discoveredMainClasses)((s, names) =>
          Defaults.runMainParser(s, names getOrElse Nil))

        Def.inputTask {
          // use assert to prevent warning about pure expr in stat pos
          assert(scalaJSEnsureUnforked.value)

          val mainCl = parser.parsed._1
          jsRun(jsEnv.value, scalaJSExecClasspath.value, mainCl,
              memLauncher(mainCl), scalaJSConsole.value, streams.value.log)
        }
      }
  )

  val scalaJSCompileSettings = (
      scalaJSConfigSettings ++
      scalaJSRunSettings ++

      // Staged runners
      inTask(fastOptStage)(scalaJSRunSettings) ++
      inTask(fullOptStage)(scalaJSRunSettings)
  )

  val scalaJSTestFrameworkSettings = Seq(
      // Copied from Defaults, but scoped. We need a JVM loader in
      // loadedTestFrameworks to find out whether the framework exists.
      testLoader in loadedTestFrameworks := {
        TestFramework.createTestLoader(
            Attributed.data(fullClasspath.value),
            scalaInstance.value,
            IO.createUniqueDirectory(taskTemporaryDirectory.value))
      },

      loadedTestFrameworks := {
        // use assert to prevent warning about pure expr in stat pos
        assert(scalaJSEnsureUnforked.value)

        val loader = (testLoader in loadedTestFrameworks).value
        val isTestFrameworkDefined = try {
          Class.forName(scalaJSTestFramework.value, false, loader)
          true
        } catch {
          case _: ClassNotFoundException => false
        }
        if (isTestFrameworkDefined) {
          loadedTestFrameworks.value.updated(
              sbt.TestFramework(classOf[TestFramework].getName),
              new TestFramework(
                  environment = jsEnv.value,
                  jsConsole = scalaJSConsole.value,
                  testFramework = scalaJSTestFramework.value)
          )
        } else {
          loadedTestFrameworks.value
        }
      },

      // Pseudo loader to pass classpath to test framework
      testLoader := JSClasspathLoader(scalaJSExecClasspath.value)
  )

  /** Transformer to force keys (which are not in exclude list) to be
   *  scoped in a given task if they weren't scoped to the Global task
   */
  class ForceTaskScope[A](task: TaskKey[A],
      excl: Set[AttributeKey[_]]) extends (ScopedKey ~> ScopedKey) {
    def apply[B](sc: ScopedKey[B]) = {
      if (!excl.contains(sc.key) && sc.scope.task != Global) {
        val scope = sc.scope.copy(task = Select(task.key))
        sc.copy(scope = scope)
      } else sc
    }
  }

  private def filterTask(
      settings: Seq[Def.Setting[_]],
      task: TaskKey[_],
      keys: Set[AttributeKey[_]],
      excl: Set[AttributeKey[_]]) = {
    val f = new ForceTaskScope(task, excl)

    for {
      setting <- settings if keys.contains(setting.key.key)
    } yield setting mapKey f mapReferenced f
  }

  def stagedTestSettings[A](task: TaskKey[A]) = {
    // Re-filter general settings
    val hackedTestTasks = filterTask(Defaults.testTasks, task,
        keys = Set(executeTests.key, testListeners.key,
            testOptions.key, test.key, testOnly.key, testExecution.key),
        excl = Set(testFilter.key, testGrouping.key))

    // Re-filter settings specific to testQuick
    val hackedTestQuickTasks = filterTask(Defaults.testTasks, task,
        keys = Set(testFilter.key, testQuick.key),
        excl = Set(testGrouping.key))

    hackedTestTasks ++ hackedTestQuickTasks ++
    inTask(task)(scalaJSTestFrameworkSettings)
  }

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
      scalaJSTestFrameworkSettings ++

      // Add staged tests
      stagedTestSettings(fastOptStage) ++
      stagedTestSettings(fullOptStage)
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

      skip in packageJSDependencies := true,

      scalaJSTestFramework := "org.scalajs.jasminetest.JasmineTestFramework",

      emitSourceMaps := true,

      scalaJSOptimizerOptions := OptimizerOptions(),

      jsDependencies := Seq(),
      jsDependencyFilter := identity,

      scalaJSSemantics := Semantics.Defaults,

      scalaJSConsole := ConsoleJSConsole,

      clean <<= clean.dependsOn(Def.task {
        // have clean reset incremental optimizer state
        (scalaJSOptimizer in (Compile, fastOptJS)).value.clean()
        (scalaJSOptimizer in (Test, fastOptJS)).value.clean()
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

  val scalaJSReleasesResolver = Resolver.url("scala-js-releases",
      url("http://dl.bintray.com/content/scala-js/scala-js-releases"))(
      Resolver.ivyStylePatterns)
  val scalaJSSnapshotsResolver = Resolver.url("scala-js-snapshots",
      url("http://repo.scala-js.org/repo/snapshots/"))(
      Resolver.ivyStylePatterns)

  val scalaJSEcosystemSettings = Seq(
      // the resolver to find the compiler and library (and others)
      resolvers ++= Seq(scalaJSReleasesResolver, scalaJSSnapshotsResolver),

      // you will need the Scala.js compiler plugin
      autoCompilerPlugins := true,
      addCompilerPlugin(
          "org.scala-lang.modules.scalajs" % "scalajs-compiler" % scalaJSVersion cross CrossVersion.full),

      // and of course the Scala.js library
      libraryDependencies += "org.scala-lang.modules.scalajs" %% "scalajs-library" % scalaJSVersion,

      // and you will want to be cross-compiled on the Scala.js binary version
      crossVersion := ScalaJSCrossVersion.binary
  )

}
