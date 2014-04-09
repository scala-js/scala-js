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
import scala.scalajs.tools.optimizer.{ScalaJSOptimizer, ScalaJSClosureOptimizer}

import scala.scalajs.tools.env._
import scala.scalajs.sbtplugin.env.rhino.RhinoJSEnv
import scala.scalajs.sbtplugin.env.nodejs.NodeJSEnv

import scala.scalajs.sbtplugin.testing.TestFramework

object ScalaJSPlugin extends Plugin {
  val scalaJSVersion = "0.5.0-SNAPSHOT"
  val scalaJSIsSnapshotVersion = scalaJSVersion endsWith "-SNAPSHOT"
  val scalaJSScalaVersion = "2.10.2"

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

    val scalaJSSetupRunner = settingKey[Boolean](
        "Configure the run task to run the main object with the Scala.js environment")

    val scalaJSTestFramework = settingKey[String](
        "The Scala.js class that is used as a test framework, for example a class that wraps Jasmine")

    val relativeSourceMaps = settingKey[Boolean](
        "Make the referenced paths on source maps relative to target path")

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
      else (f ** "*.js").get
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

        val outputWriter = new FileVirtualScalaJSPackfileWriter(output)

        try {
          if (classpath.isEmpty) {
            import ScalaJSPackedClasspath.{ writePackInfo, PackInfoData }

            outputWriter.contentWriter.write("/* dummy empty file */\n")
            writePackInfo(outputWriter, PackInfoData(packOrder))
          } else {
            FileFunction.cached(taskCacheDir,
                FilesInfo.lastModified, FilesInfo.exists) { dependencies =>
              s.log.info("Packaging %s ..." format output)
              import ScalaJSPackager._
              val classpathEntries =
                ScalaJSClasspath.partialFromClasspath(classpath)
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
                        wantSourceMap = (packageJSKey != packageExternalDepsJS),
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

  /** Patches the IncOptions so that .js and .js.map files are pruned as needed.
   *
   *  This complicated logic patches the ClassfileManager factory of the given
   *  IncOptions with one that is aware of .js, .js.map and .sjsinfo files
   *  emitted by the Scala.js compiler. This makes sure that, when a .class
   *  file must be deleted, the corresponding .js, .js.map and .sjsinfo files
   *  are also deleted.
   */
  def scalaJSPatchIncOptions(incOptions: IncOptions): IncOptions = {
    val inheritedNewClassfileManager = incOptions.newClassfileManager
    val newClassfileManager = () => new ClassfileManager {
      val inherited = inheritedNewClassfileManager()

      def delete(classes: Iterable[File]): Unit = {
        inherited.delete(classes flatMap { classFile =>
          val scalaJSFiles = if (classFile.getPath endsWith ".class") {
            for {
              ext <- List(".js", ".js.map", ".sjsinfo")
              f = FileVirtualFile.withExtension(classFile, ".class", ext)
              if f.exists
            } yield f
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
          val optimizer = new ScalaJSOptimizer
          val outputWriter = new FileVirtualScalaJSPackfileWriter(output)

          try {
            optimizer.optimize(
                Inputs(classpath = classpathEntries),
                OutputConfig(
                    name = output.name,
                    writer = outputWriter,
                    // TODO configure source map once we support it
                    wantSourceMap = false),
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
          val outputWriter = new FileVirtualScalaJSPackfileWriter(output)

          try {
            optimizer.optimize(
                Inputs(sources = inputs map FileVirtualJSFile),
                OutputConfig(
                    name = output.name,
                    writer = outputWriter,
                    // TODO configure source map once we support it
                    wantSourceMap = false,
                    prettyPrint = fullOptJSPrettyPrint.value),
                s.log)
          } finally {
            outputWriter.close()
          }

          Set(output)
        } (inputs.toSet)

        output
      },

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

      exportedProducts in packageStage := Attributed.blankSeq( packageJS.value),
      exportedProducts in fastOptStage := Seq(Attributed.blank(fastOptJS.value)),
      exportedProducts in fullOptStage := Seq(Attributed.blank(fullOptJS.value))

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
   *  scoped in a given task.
   */
  class ForceTaskScope[A](task: TaskKey[A],
      excl: Set[AttributeKey[_]]) extends (ScopedKey ~> ScopedKey) {
    def apply[B](sc: ScopedKey[B]) = if (!excl.contains(sc.key)) {
      val scope = sc.scope.copy(task = Select(task.key))
      sc.copy(scope = scope)
    } else sc
  }

  def stagedTestSettings[A](task: TaskKey[A]) = {
    val keys = Set[AttributeKey[_]](
        testLoader.key, executeTests.key, test.key, testOnly.key, testQuick.key)
    val excl = Set[AttributeKey[_]](
        testExecution.key, testFilter.key, testGrouping.key)

    val f = new ForceTaskScope(task, excl)
    val hackedTestTasks = for {
      setting <- Defaults.testTasks
      if keys.contains(setting.key.key)
    } yield setting mapKey f mapReferenced f

    hackedTestTasks ++
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

  def defaultLoggingConsole =
      loggingConsole := Some(new LoggerJSConsole(streams.value.log))

  val scalaJSDefaultConfigs = (
      inConfig(Compile)(scalaJSCompileSettings) ++
      inConfig(Test)(scalaJSTestSettings)
  )

  val scalaJSProjectBaseSettings = Seq(
      relativeSourceMaps := false,
      fullOptJSPrettyPrint := false,

      preLinkJSEnv  := new RhinoJSEnv,
      postLinkJSEnv := new NodeJSEnv,

      defaultLoggingConsole
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
      libraryDependencies += "org.scala-lang.modules.scalajs" %% "scalajs-library" % scalaJSVersion
  )
}
