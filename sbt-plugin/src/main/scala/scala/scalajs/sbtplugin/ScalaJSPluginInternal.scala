package scala.scalajs.sbtplugin

import sbt._
import sbt.inc.{IncOptions, ClassfileManager}
import Keys._

import Implicits._
import JSUtils._

import scala.scalajs.tools.io.{IO => _, _}
import scala.scalajs.tools.classpath._
import scala.scalajs.tools.classpath.builder._
import scala.scalajs.tools.packager._
import scala.scalajs.tools.jsdep._
import scala.scalajs.tools.optimizer.{ScalaJSOptimizer, ScalaJSClosureOptimizer}
import scala.scalajs.tools.corelib.CoreJSLibs

import scala.scalajs.tools.env._
import scala.scalajs.sbtplugin.env.rhino.RhinoJSEnv
import scala.scalajs.sbtplugin.env.nodejs.NodeJSEnv
import scala.scalajs.sbtplugin.env.phantomjs.PhantomJSEnv

import scala.scalajs.ir.ScalaJSVersions

import scala.scalajs.sbtplugin.testing.{TestFramework, JSClasspathLoader}

import scala.util.Try

import java.nio.charset.Charset

/** Contains settings used by ScalaJSPlugin that should not be automatically
 *  be in the *.sbt file's scope.
 */
object ScalaJSPluginInternal {

  import ScalaJSPlugin.ScalaJSKeys._
  import ScalaJSPlugin.scalaJSVersion

  /** Dummy setting to ensure we do not fork in Scala.js run & test. */
  val ensureUnforked = SettingKey.local[Boolean]

  /** Dummy setting to persist Scala.js optimizer */
  val scalaJSOptimizer = SettingKey.local[ScalaJSOptimizer]

  private def isJarWithPrefix(prefixes: String*)(item: File): Boolean = {
    item.name.endsWith(".jar") && prefixes.exists(item.name.startsWith)
  }

  val isScalaJSCompilerJar = isJarWithPrefix(
      "scala-library", "scala-compiler", "scala-reflect", "scalajs-compiler",
      "scala-parser-combinators", "scala-xml") _

  private def filterClasspath(cp: Seq[Attributed[File]]): Seq[File] = {
    for {
      entry <- cp
      f = entry.data
      if !isScalaJSCompilerJar(f)
    } yield f
  }

  private def filesToWatchForChanges(classpath: Seq[File]): Set[File] = {
    val seq = classpath flatMap { f =>
      if (f.isFile) List(f)
      else (f ** (("*.sjsir": NameFilter) | "*.js" |
          JSDependencyManifest.ManifestFileName)).get
    }
    seq.toSet
  }

  def packageClasspathJSTasks(classpathKey: TaskKey[Classpath],
      packageJSKey: TaskKey[PartialClasspath],
      outputSuffix: String): Seq[Setting[_]] = Seq(

      artifactPath in packageJSKey :=
        ((crossTarget in packageJSKey).value /
            ((moduleName in packageJSKey).value + outputSuffix + ".js")),

      packageJSKey := {
        val s = streams.value
        val classpathDirs =
          filterClasspath((classpathKey in packageJSKey).value).toList
        val output = (artifactPath in packageJSKey).value
        val taskCache = WritableFileVirtualTextFile(
            s.cacheDirectory / ("package-js" + outputSuffix))
        val classpath = PartialClasspathBuilder.build(classpathDirs.toList)

        IO.createDirectory(output.getParentFile)

        val relSourceMapBase =
          if (relativeSourceMaps.value) Some(output.getParentFile.toURI())
          else None

        import ScalaJSPackager._
        (new ScalaJSPackager).packageCP(classpath,
            OutputConfig(
                output = WritableFileVirtualJSFile(output),
                cache = Some(taskCache),
                wantSourceMap = (emitSourceMaps in packageJSKey).value,
                relativizeSourceMapBase = relSourceMapBase),
            s.log)
      }
  )

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
      val inherited = inheritedNewClassfileManager()

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

  val scalaJSConfigSettings: Seq[Setting[_]] = Seq(
      incOptions ~= scalaJSPatchIncOptions
  ) ++ (
      packageClasspathJSTasks(externalDependencyClasspath,
          packageExternalDepsJS, "-pack-extdeps") ++
      packageClasspathJSTasks(internalDependencyClasspath,
          packageInternalDepsJS, "-pack-intdeps") ++
      packageClasspathJSTasks(exportedProducts,
          packageExportedProductsJS, "-pack-app")
  ) ++ Seq(

      preLinkClasspath := {
        val cp = fullClasspath.value
        val pcp = PartialClasspathBuilder.buildIR(Attributed.data(cp).toList)
        pcp.resolve(jsDependencyFilter.value)
      },

      // The artifactPath of packageJS is the location of the corejslibs.js
      artifactPath in packageJS :=
        ((crossTarget in packageJS).value / "corejslibs.js"),

      packageJS := {
        // If it doesn't exist, we need to write the corejslibs so they are
        // available to HTML files
        val envFile = (artifactPath in packageJS).value
        if (!envFile.exists) {
          IO.createDirectory(envFile.getParentFile)
          for (lib <- CoreJSLibs.libs)
            IO.append(envFile, lib.content, Charset.forName("UTF-8"))
        }

        val cps = List(
            packageExternalDepsJS.value,
            packageInternalDepsJS.value,
            packageExportedProductsJS.value)

        cps.reduceLeft(_ append _).resolve(jsDependencyFilter.value)
      },
      packageJS <<=
        packageJS.dependsOn(packageJSDependencies, packageLauncher),

      artifactPath in fastOptJS :=
        ((crossTarget in fastOptJS).value /
            ((moduleName in fastOptJS).value + "-fastopt.js")),

      scalaJSOptimizer in fastOptJS :=
        new ScalaJSOptimizer,

      fastOptJS := {
        val s = streams.value
        val output = (artifactPath in fastOptJS).value
        val taskCache =
          WritableFileVirtualTextFile(s.cacheDirectory / "fastopt-js")

        IO.createDirectory(output.getParentFile)

        val relSourceMapBase =
          if (relativeSourceMaps.value) Some(output.getParentFile.toURI())
          else None

        import ScalaJSOptimizer._
        (scalaJSOptimizer in fastOptJS).value.optimizeCP(
            Inputs(input = (preLinkClasspath in fastOptJS).value),
            OutputConfig(
                output = WritableFileVirtualJSFile(output),
                cache = Some(taskCache),
                wantSourceMap = (emitSourceMaps in fastOptJS).value,
                relativizeSourceMapBase = relSourceMapBase,
                checkIR = checkScalaJSIR.value),
            s.log)
      },
      fastOptJS <<=
        fastOptJS.dependsOn(packageJSDependencies, packageLauncher),

      artifactPath in fullOptJS :=
        ((crossTarget in fullOptJS).value /
            ((moduleName in fullOptJS).value + "-opt.js")),

      fullOptJS := {
        val s = streams.value
        val output = (artifactPath in fullOptJS).value
        val taskCache =
          WritableFileVirtualTextFile(s.cacheDirectory / "fullopt-js")

        IO.createDirectory(output.getParentFile)

        val relSourceMapBase =
          if (relativeSourceMaps.value) Some(output.getParentFile.toURI())
          else None

        import ScalaJSClosureOptimizer._
        (new ScalaJSClosureOptimizer).optimizeCP(
            Inputs(input = (fastOptJS in fullOptJS).value),
            OutputConfig(
                output = WritableFileVirtualJSFile(output),
                cache = Some(taskCache),
                wantSourceMap = (emitSourceMaps in fullOptJS).value,
                prettyPrint = fullOptJSPrettyPrint.value,
                relSourceMapBase),
            s.log)
      },

      artifactPath in packageLauncher :=
        ((crossTarget in packageLauncher).value /
            ((moduleName in packageLauncher).value + "-launcher.js")),

      skip in packageLauncher := !persistLauncher.value,

      packageLauncher <<= Def.taskDyn {
        if ((skip in packageLauncher).value)
          Def.task(Attributed.blank((artifactPath in packageLauncher).value))
        else Def.task {
          mainClass.value map { mainCl =>
            val file = (artifactPath in packageLauncher).value
            IO.write(file, launcherContent(mainCl), Charset.forName("UTF-8"))

            // Attach the name of the main class used, (ab?)using the name key
            Attributed(file)(AttributeMap.empty.put(name.key, mainCl))
          } getOrElse {
            error("Cannot write launcher file, since there is no or multiple mainClasses")
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
          val cp = preLinkClasspath.value
          val output = (artifactPath in packageJSDependencies).value

          import ScalaJSPackager._
          (new ScalaJSPackager).packageJS(cp.jsLibs,
               OutputConfig(WritableFileVirtualJSFile(output)),
               streams.value.log,
               strictMode = false)

          output
        }
      },

      compile <<= compile.dependsOn(Def.task {
        val myModule = thisProject.value.id
        val config = configuration.value.name

        // Collect all libraries
        val jsDeps = for {
          dep <- jsDependencies.value
          if dep.configurations.forall(_ == config)
        } yield dep.jsDep

        val manifest = JSDependencyManifest(
            Origin(myModule, config), jsDeps.toList)

        // Write dependency file to class directory
        val targetDir = classDirectory.value
        IO.createDirectory(targetDir)

        val file = targetDir / JSDependencyManifest.ManifestFileName

        // Prevent writing if unnecessary to not invalidate dependencies
        val needWrite = !file.exists || {
          Try {
            val vfile = new FileVirtualTextFile(file)
            val readManifest = JSDependencyManifest.read(vfile)
            readManifest != manifest
          } getOrElse true
        }

        if (needWrite)
          JSDependencyManifest.write(manifest, file)
      }),

      console <<= console.dependsOn(Def.task(
          streams.value.log.warn("Scala REPL doesn't work with Scala.js. You " +
              "are running a JVM REPL. JavaScript things won't work.")
      )),

      // Give tasks ability to check we are not forking at build reading time
      ensureUnforked := {
        if (fork.value)
          error("Scala.js cannot be run in a forked JVM")
        else
          true
      },

      // Default jsEnv
      jsEnv := preLinkJSEnv.value,

      // Wire jsEnv and sources for other stages
      jsEnv in packageStage := postLinkJSEnv.value,
      jsEnv in fastOptStage := postLinkJSEnv.value,
      jsEnv in fullOptStage := postLinkJSEnv.value,

      // Define execution classpaths
      execClasspath                 := preLinkClasspath.value,
      execClasspath in packageStage := packageJS.value,
      execClasspath in fastOptStage := fastOptJS.value,
      execClasspath in fullOptStage := fullOptJS.value,

      // Dummy task need dummy tags (used for concurrency restrictions)
      tags in packageStage := Seq(),
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
    env.runJS(cp, launcher, log, jsConsole)
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
      mainClass in launcher := (mainClass in run).value,
      launcher <<= Def.taskDyn {
        if (persistLauncher.value)
          Def.task(packageLauncher.value.map(FileVirtualJSFile))
        else Def.task {
          (mainClass in launcher).value map { mainClass =>
            val memLaunch = memLauncher(mainClass)
            Attributed[VirtualJSFile](memLaunch)(
                AttributeMap.empty.put(name.key, mainClass))
          } getOrElse {
            error("No main class detected.")
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
        assert(ensureUnforked.value)

        val launch = launcher.value
        val className = launch.get(name.key).getOrElse("<unknown class>")
        jsRun(jsEnv.value, execClasspath.value, className,
            launch.data, jsConsole.value, streams.value.log)
      },

      runMain <<= {
        // Implicits for parsing
        import sbinary.DefaultProtocol.StringFormat
        import Cache.seqFormat

        val parser = Defaults.loadForParser(discoveredMainClasses)((s, names) =>
          Defaults.runMainParser(s, names getOrElse Nil))

        Def.inputTask {
          // use assert to prevent warning about pure expr in stat pos
          assert(ensureUnforked.value)

          val mainCl = parser.parsed._1
          jsRun(jsEnv.value, execClasspath.value, mainCl,
              memLauncher(mainCl), jsConsole.value, streams.value.log)
        }
      }
  )

  val scalaJSCompileSettings = (
      scalaJSConfigSettings ++
      scalaJSRunSettings ++

      // Staged runners
      inTask(packageStage)(scalaJSRunSettings) ++
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
        assert(ensureUnforked.value)

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
                  jsConsole = jsConsole.value,
                  testFramework = scalaJSTestFramework.value)
          )
        } else {
          loadedTestFrameworks.value
        }
      },

      // Pseudo loader to pass classpath to test framework
      testLoader := JSClasspathLoader(execClasspath.value)
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
      Seq(packageExternalDepsJS, packageInternalDepsJS,
          packageExportedProductsJS,
          fastOptJS, fullOptJS, packageLauncher,
          packageJSDependencies) map { packageJSTask =>
        moduleName in packageJSTask := moduleName.value + "-test"
      }
  )

  val scalaJSTestSettings = (
      scalaJSTestBuildSettings ++
      scalaJSTestFrameworkSettings ++

      // Add staged tests
      stagedTestSettings(packageStage) ++
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

  val scalaJSProjectBaseSettings = Seq(
      relativeSourceMaps   := false,
      fullOptJSPrettyPrint := false,
      requiresDOM          := false,
      persistLauncher      := false,

      skip in packageJSDependencies := true,

      scalaJSTestFramework := "scala.scalajs.test.JasmineTestFramework",

      preLinkJSEnv  := new RhinoJSEnv(withDOM = requiresDOM.value),
      postLinkJSEnv := {
        if (requiresDOM.value) new PhantomJSEnv
        else new NodeJSEnv
      },

      emitSourceMaps := true,
      emitSourceMaps in packageExternalDepsJS := false,

      checkScalaJSIR := false,

      jsDependencies := Seq(),
      jsDependencyFilter := identity,

      jsConsole := ConsoleJSConsole,

      clean <<= clean.dependsOn(Def.task {
        // have clean reset incremental optimizer state
        (scalaJSOptimizer in (Compile, fastOptJS)).value.clean()
        (scalaJSOptimizer in (Test, fastOptJS)).value.clean()
      })
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
