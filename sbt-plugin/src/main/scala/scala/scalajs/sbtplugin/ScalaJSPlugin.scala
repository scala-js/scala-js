/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.sbtplugin

import sbt._
import sbt.inc.{ IncOptions, ClassfileManager }
import Keys._

import Implicits._

import scala.scalajs.tools.io.{IO => _, _}
import scala.scalajs.tools.classpath._
import scala.scalajs.tools.packager._
import scala.scalajs.tools.jsdep._
import scala.scalajs.tools.optimizer.{ScalaJSOptimizer, ScalaJSClosureOptimizer}

import scala.scalajs.tools.env._
import scala.scalajs.sbtplugin.env.rhino.RhinoJSEnv
import scala.scalajs.sbtplugin.env.nodejs.NodeJSEnv
import scala.scalajs.sbtplugin.env.phantomjs.PhantomJSEnv

import scala.scalajs.ir.ScalaJSVersions

import scala.scalajs.sbtplugin.testing.TestFramework

import scala.util.Try

object ScalaJSPlugin extends Plugin with impl.DependencyBuilders {
  val scalaJSVersion = ScalaJSVersions.current
  val scalaJSIsSnapshotVersion = ScalaJSVersions.currentIsSnapshot
  val scalaJSScalaVersion = "2.11.0"
  val scalaJSBinaryVersion =
    ScalaJSCrossVersion.binaryScalaJSVersion(scalaJSVersion)

  object ScalaJSKeys {
    val packageJS = taskKey[Seq[File]](
        "Package all the compiled .js files")
    val fastOptJS = taskKey[File](
        "Package and fast optimize all the compiled .js files in one file")
    val fullOptJS = taskKey[File](
        "Package and fully optimize all the compiled .js files in one file")

    val packageExternalDepsJS = taskKey[Seq[File]](
        "Package the .js files of external dependencies")
    val packageInternalDepsJS = taskKey[Seq[File]](
        "Package the .js files of internal dependencies")
    val packageExportedProductsJS = taskKey[Seq[File]](
        "Package the .js files of the project")

    val fullOptJSPrettyPrint = settingKey[Boolean](
        "Pretty-print the output of fullOptJS")

    val loggingConsole = taskKey[Option[JSConsole]](
        "The logging console used by the Scala.js jvm environment")

    val preLinkJSEnv = settingKey[JSEnv](
        "The jsEnv used to execute before linking (packaging / optimizing) Scala.js files")
    val postLinkJSEnv = settingKey[JSEnv](
        "The jsEnv used to execute after linking (packaging / optimizing) Scala.js files")

    val jsEnv = settingKey[JSEnv](
        "A JVM-like environment where Scala.js files can be run and tested")

    val requiresDOM = settingKey[Boolean]("Whether this projects needs the DOM")

    val scalaJSSetupRunner = settingKey[Boolean](
        "Configure the run task to run the main object with the Scala.js environment")

    val scalaJSTestFramework = settingKey[String](
        "The Scala.js class that is used as a test framework, for example a class that wraps Jasmine")

    val relativeSourceMaps = settingKey[Boolean](
        "Make the referenced paths on source maps relative to target path")

    val checkScalaJSIR = settingKey[Boolean](
        "Perform expensive checks of the sanity of the Scala.js IR")

    val emitSourceMaps = settingKey[Boolean](
        "Whether package and optimize stages should emit source maps at all")

    val scalaJSOptimizer = settingKey[ScalaJSOptimizer](
        "Scala.js optimizer")

    val jsDependencies = settingKey[Seq[JSModuleID]](
        "JavaScript libraries this project depends upon")

    // Task keys to re-wire sources and run with other VM
    val packageStage = taskKey[Unit]("Run stuff after packageJS")
    val fastOptStage = taskKey[Unit]("Run stuff after fastOptJS")
    val fullOptStage = taskKey[Unit]("Run stuff after fullOptJS")
  }

  import ScalaJSKeys._

  private def isJarWithPrefix(prefixes: String*)(item: File): Boolean = {
    item.name.endsWith(".jar") && prefixes.exists(item.name.startsWith)
  }

  val isScalaJSCompilerJar = isJarWithPrefix(
      "scala-library", "scala-compiler", "scala-reflect", "scalajs-compiler",
      "scala-parser-combinators", "scala-xml") _

  private def jsClasspath(cp: Seq[Attributed[File]]): Seq[File] = {
    for {
      entry <- cp
      f = entry.data
      if !isScalaJSCompilerJar(f)
    } yield f
  }

  private def filesToWatchForChanges(classpath: Seq[File]): Set[File] = {
    val seq = classpath flatMap { f =>
      if (f.isFile) List(f)
      else (f ** (("*.sjsir": NameFilter) | "*.js")).get
    }
    seq.toSet
  }

  def packageClasspathJSTasks(classpathKey: TaskKey[Classpath],
      packageJSKey: TaskKey[Seq[File]],
      outputSuffix: String,
      packOrder: Int): Seq[Setting[_]] = Seq(

      artifactPath in packageJSKey :=
        ((crossTarget in packageJSKey).value /
            ((moduleName in packageJSKey).value + outputSuffix + ".js")),

      packageJSKey := {
        val s = streams.value
        val classpath = jsClasspath((classpathKey in packageJSKey).value)
        val output = (artifactPath in packageJSKey).value
        val taskCacheDir = s.cacheDirectory / "package-js"

        IO.createDirectory(output.getParentFile)

        val outputWriter = new FileVirtualJSFileWriter(output)

        try {
          if (classpath.isEmpty) {
            import ScalaJSPackedClasspath.packOrderLine

            outputWriter.contentWriter.write(packOrderLine(packOrder) + "\n")
            outputWriter.contentWriter.write("/* dummy empty file */\n")
          } else {
            FileFunction.cached(taskCacheDir,
                FilesInfo.lastModified, FilesInfo.exists) { dependencies =>
              s.log.info("Packaging %s ..." format output)
              import ScalaJSPackager._
              val classpathEntries = ScalaJSClasspath.fromClasspath(classpath)
              val packager = new ScalaJSPackager
              val relSourceMapBase =
                if (relativeSourceMaps.value) Some(output.getParentFile.toURI())
                else None

              try {
                packager.packageScalaJS(
                    Inputs(classpath = classpathEntries),
                    OutputConfig(
                        name = output.name,
                        writer = outputWriter,
                        packOrder = packOrder,
                        addCoreJSLibs = packageJSKey == packageExternalDepsJS,
                        wantSourceMap = (emitSourceMaps in packageJSKey).value,
                        relativizeSourceMapBase = relSourceMapBase),
                    s.log)
              } finally {
                outputWriter.close()
              }

              Set(output)
            } (filesToWatchForChanges(classpath))
          }
        } finally {
          outputWriter.close()
        }

        Seq(output)
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
          packageExternalDepsJS, "-extdeps", 0) ++
      packageClasspathJSTasks(internalDependencyClasspath,
          packageInternalDepsJS, "-intdeps", 1) ++
      packageClasspathJSTasks(exportedProducts,
          packageExportedProductsJS, "", 2)
  ) ++ Seq(

      packageJS := (
          packageExternalDepsJS.value ++
          packageInternalDepsJS.value ++
          packageExportedProductsJS.value
      ),

      artifactPath in fastOptJS :=
        ((crossTarget in fastOptJS).value /
            ((moduleName in fastOptJS).value + "-fastopt.js")),

      scalaJSOptimizer in fastOptJS :=
        new ScalaJSOptimizer,

      fastOptJS := {
        val s = streams.value
        val classpath = jsClasspath((fullClasspath in fastOptJS).value)
        val output = (artifactPath in fastOptJS).value
        val taskCacheDir = s.cacheDirectory / "fastopt-js"

        IO.createDirectory(output.getParentFile)

        FileFunction.cached(taskCacheDir,
            FilesInfo.lastModified, FilesInfo.exists) { dependencies =>
          s.log.info("Fast optimizing %s ..." format output)
          import ScalaJSOptimizer._
          val classpathEntries = ScalaJSClasspath.fromClasspath(classpath)
          val optimizer = (scalaJSOptimizer in fastOptJS).value
          val outputWriter = new FileVirtualJSFileWriter(output)
          val relSourceMapBase =
              if (relativeSourceMaps.value) Some(output.getParentFile.toURI())
              else None

          try {
            optimizer.optimize(
                Inputs(classpath = classpathEntries),
                OutputConfig(
                    name = output.name,
                    writer = outputWriter,
                    wantSourceMap = (emitSourceMaps in fastOptJS).value,
                    relativizeSourceMapBase = relSourceMapBase,
                    checkIR = checkScalaJSIR.value),
                s.log)
          } finally {
            outputWriter.close()
          }

          Set(output)
        } (filesToWatchForChanges(classpath))

        output
      },

      sources in fullOptJS := Seq(fastOptJS.value),

      artifactPath in fullOptJS :=
        ((crossTarget in fullOptJS).value /
            ((moduleName in fullOptJS).value + "-opt.js")),

      fullOptJS := {
        val s = streams.value
        val inputs = (sources in fullOptJS).value
        val output = (artifactPath in fullOptJS).value
        val taskCacheDir = s.cacheDirectory / "fullopt-js"

        IO.createDirectory(output.getParentFile)

        FileFunction.cached(taskCacheDir,
            FilesInfo.lastModified, FilesInfo.exists) { dependencies =>
          s.log.info("Optimizing %s ..." format output)
          import ScalaJSClosureOptimizer._
          val optimizer = new ScalaJSClosureOptimizer
          val outputWriter = new FileVirtualJSFileWriter(output)
          val relSourceMapBase =
              if (relativeSourceMaps.value) Some(output.getParentFile.toURI())
              else None

          try {
            optimizer.optimize(
                Inputs(sources = inputs map FileVirtualJSFile),
                OutputConfig(
                    name = output.name,
                    writer = outputWriter,
                    wantSourceMap = (emitSourceMaps in fullOptJS).value,
                    prettyPrint = fullOptJSPrettyPrint.value,
                    relSourceMapBase),
                s.log)
          } finally {
            outputWriter.close()
          }

          Set(output)
        } (inputs.toSet)

        output
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

        if (needWrite) {
          val writer = new FileVirtualTextFileWriter(file)

          try { JSDependencyManifest.write(manifest, writer) }
          finally { writer.close() }
        }
      }),

      console <<= console.dependsOn(Def.task(
          streams.value.log.warn("Scala REPL doesn't work with Scala.js. You " +
              "are running a JVM REPL. JavaScript things won't work.")
      )),

      // Default jsEnv
      jsEnv := preLinkJSEnv.value,

      // Wire jsEnv and sources for other stages
      jsEnv in packageStage := postLinkJSEnv.value,
      jsEnv in fastOptStage := postLinkJSEnv.value,
      jsEnv in fullOptStage := postLinkJSEnv.value,

      exportedProducts in packageStage :=
        Attributed.blankSeq(packageJS.value) ++ exportedProducts.value,
      exportedProducts in fastOptStage :=
        Attributed.blank(fastOptJS.value) +: exportedProducts.value,
      exportedProducts in fullOptStage :=
        Attributed.blank(fullOptJS.value) +: exportedProducts.value

  )

  val scalaJSStageSettings = Seq(
    fullClasspath <<= Classpaths.concatDistinct(exportedProducts,
        dependencyClasspath in (This, This, Global)),

    // Dummy task needs dummy tag (used for concurrency restrictions)
    tags := Seq()
  )

  // These settings will be filtered by the stage dummy tasks
  val scalaJSRunSettings = Seq(
      scalaJSSetupRunner := true,
      runner <<= Def.taskDyn {
        if (scalaJSSetupRunner.value)
          Def.task(new ScalaJSEnvRun(jsEnv.value))
        else
          runner
      },

      // Although these are the defaults, we add them again since they
      // will be filtered
      run <<= Defaults.runTask(fullClasspath, mainClass, runner),
      runMain <<= Defaults.runMainTask(fullClasspath, runner)
  )

  val scalaJSCompileSettings = (
      scalaJSConfigSettings ++
      scalaJSRunSettings ++

      // Staged runners
      inTask(packageStage)(scalaJSStageSettings ++ scalaJSRunSettings) ++
      inTask(fastOptStage)(scalaJSStageSettings ++ scalaJSRunSettings) ++
      inTask(fullOptStage)(scalaJSStageSettings ++ scalaJSRunSettings)
  )

  val scalaJSTestFrameworkSettings = Seq(
      scalaJSTestFramework := "scala.scalajs.test.JasmineTestFramework",

      loadedTestFrameworks := {
        // Hard scope the loader since we cannot test presence in other stages
        val loader = (testLoader in test).value
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
                  testFramework = scalaJSTestFramework.value)
          )
        } else {
          loadedTestFrameworks.value
        }
      }
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
        keys = Set(testLoader.key, executeTests.key, testListeners.key,
            testOptions.key, test.key, testOnly.key, testExecution.key),
        excl = Set(testFilter.key, testGrouping.key))

    // Re-filter settings specific to testQuick
    val hackedTestQuickTasks = filterTask(Defaults.testTasks, task,
        keys = Set(testFilter.key, testQuick.key),
        excl = Set(testGrouping.key))

    hackedTestTasks ++ hackedTestQuickTasks ++
    inTask(task)(scalaJSStageSettings ++ scalaJSTestFrameworkSettings)
  }

  val scalaJSTestSettings = (
      scalaJSConfigSettings ++
      scalaJSTestFrameworkSettings ++

      // Add staged tests
      stagedTestSettings(packageStage) ++
      stagedTestSettings(fastOptStage) ++
      stagedTestSettings(fullOptStage)
  ) ++ (
      Seq(packageExternalDepsJS, packageInternalDepsJS,
          packageExportedProductsJS,
          fastOptJS, fullOptJS) map { packageJSTask =>
        moduleName in packageJSTask := moduleName.value + "-test"
      }
  )

  val scalaJSDefaultConfigs = (
      inConfig(Compile)(scalaJSCompileSettings) ++
      inConfig(Test)(scalaJSTestSettings)
  ) ++ Seq(
      // add all the webjars your jsDependencies depend upon
      libraryDependencies ++= jsDependencies.value.collect {
        case JarJSModuleID(module, _) => module
      },
      // have clean reset incremental optimizer state
      clean <<= clean.dependsOn(Def.task {
        scalaJSOptimizer.in(Compile, fastOptJS).value.clean()
        scalaJSOptimizer.in(Test,    fastOptJS).value.clean()
      })
  )

  val scalaJSProjectBaseSettings = Seq(
      relativeSourceMaps   := false,
      fullOptJSPrettyPrint := false,
      requiresDOM          := false,

      preLinkJSEnv  := new RhinoJSEnv(withDOM = requiresDOM.value),
      postLinkJSEnv := {
        if (requiresDOM.value) new PhantomJSEnv
        else new NodeJSEnv
      },

      emitSourceMaps := true,
      emitSourceMaps in packageExternalDepsJS := false,

      checkScalaJSIR := false,

      jsDependencies := Seq(),

      loggingConsole := Some(new LoggerJSConsole(streams.value.log))
  )

  val scalaJSAbstractSettings: Seq[Setting[_]] = (
      scalaJSProjectBaseSettings ++
      scalaJSDefaultConfigs
  )

  val scalaJSReleasesResolver = Resolver.url("scala-js-releases",
      url("http://dl.bintray.com/content/scala-js/scala-js-releases"))(
      Resolver.ivyStylePatterns)
  val scalaJSSnapshotsResolver = Resolver.url("scala-js-snapshots",
      url("http://repo.scala-js.org/repo/snapshots/"))(
      Resolver.ivyStylePatterns)

  val scalaJSSettings: Seq[Setting[_]] = scalaJSAbstractSettings ++ Seq(
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
