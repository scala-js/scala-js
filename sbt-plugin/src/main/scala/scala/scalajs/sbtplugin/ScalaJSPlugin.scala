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

import java.io.{ BufferedWriter, FileWriter }
import java.nio.charset.Charset

import scala.collection.mutable

import SourceMapCat.catJSFilesAndTheirSourceMaps
import Utils._
import Implicits._

import scala.scalajs.tools.io.{IO => _, _}
import scala.scalajs.tools.classpath._
import scala.scalajs.tools.packager._
import scala.scalajs.tools.optimizer.{ScalaJSOptimizer, ScalaJSClosureOptimizer}

import environment.{Console, LoggerConsole, RhinoBasedScalaJSEnvironment}
import environment.rhino.{CodeBlock, Utilities}

import scala.scalajs.sbtplugin.testing.TestFramework

object ScalaJSPlugin extends Plugin {
  val scalaJSVersion = "0.4.3-SNAPSHOT"
  val scalaJSIsSnapshotVersion = scalaJSVersion endsWith "-SNAPSHOT"
  val scalaJSScalaVersion = "2.10.2"

  object ScalaJSKeys {
    val packageJS = taskKey[Seq[File]]("Package all the compiled .js files")
    val preoptimizeJS = taskKey[File]("Package and pre-optimize all the compiled .js files in one file")
    val optimizeJS = taskKey[File]("Package and optimize all the compiled .js files in one file")

    val packageExternalDepsJS = taskKey[Seq[File]]("Package the .js files of external dependencies")
    val packageInternalDepsJS = taskKey[Seq[File]]("Package the .js files of internal dependencies")
    val packageExportedProductsJS = taskKey[Seq[File]]("Package the .js files the project")

    val optimizeJSPrettyPrint = settingKey[Boolean](
        "Pretty-print the output of optimizeJS")
    val optimizeJSExterns = taskKey[Seq[File]](
        "Extern files to use with optimizeJS")

    val loggingConsole = taskKey[Option[Console]](
        "The logging console used by the Scala.js jvm environment")
    val scalaJSEnvironment = taskKey[ScalaJSEnvironment](
        "A JVM-like environment where Scala.js files can be run and tested")

    val scalaJSSetupRunner = settingKey[Boolean](
        "Configure the run task to run the main object with the Scala.js environment")

    val scalaJSTestBridgeClass = settingKey[String](
        "The Scala.js class that delegates test calls to the given test framework")
    val scalaJSTestFramework = settingKey[String](
        "The Scala.js class that is used as a test framework, for example a class that wraps Jasmine")

    val relativeSourceMaps = settingKey[Boolean](
        "Make the referenced paths on source maps relative to target path")
    val emitSourceMaps = settingKey[Boolean](
        "Whether package and optimize stages should emit source maps at all")
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
      outputSuffix: String): Seq[Setting[_]] = Seq(

      artifactPath in packageJSKey :=
        ((crossTarget in packageJSKey).value /
            ((moduleName in packageJSKey).value + outputSuffix + ".js")),

      packageJSKey := {
        val s = streams.value
        val classpath = jsClasspath((classpathKey in packageJSKey).value)
        val output = (artifactPath in packageJSKey).value
        val taskCacheDir = s.cacheDirectory / "package-js"

        IO.createDirectory(output.getParentFile)

        if (classpath.isEmpty) {
          if (!output.isFile || output.length != 0)
            IO.writeLines(output, Nil)
        } else {
          FileFunction.cached(taskCacheDir,
              FilesInfo.lastModified, FilesInfo.exists) { dependencies =>
            s.log.info("Packaging %s ..." format output)
            import ScalaJSPackager._
            val classpathEntries =
              ScalaJSClasspathEntries.readEntriesInClasspathPartial(classpath)
            val packager = new ScalaJSPackager
            val relSourceMapBase =
              if (relativeSourceMaps.value) Some(output.getParentFile.toURI())
              else None

            val outputWriter = new FileVirtualJSFileWriter(output)

            try {
              packager.packageScalaJS(
                  Inputs(classpath = classpathEntries),
                  OutputConfig(
                      name = output.name,
                      writer = outputWriter,
                      wantSourceMap = (emitSourceMaps in packageJSKey).value,
                      relativizeSourceMapBase = relSourceMapBase),
                  s.log)
            } finally {
              outputWriter.close()
            }

            Set(output)
          } (filesToWatchForChanges(classpath))
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
              f = changeExt(classFile, ".class", ext)
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

  val scalaJSEnvironmentTask = Def.task[ScalaJSEnvironment] {
    val logger = streams.value.log
    val console = loggingConsole.value

    val classpath =
      ScalaJSClasspathEntries.readEntriesInClasspath(
          jsClasspath((fullClasspath in scalaJSEnvironment).value))

    new RhinoBasedScalaJSEnvironment(classpath, console, logger.trace)
  }

  val scalaJSEnvironmentSettings = Seq(
      scalaJSEnvironment <<= scalaJSEnvironmentTask
  )

  val scalaJSConfigSettings: Seq[Setting[_]] = Seq(
      incOptions ~= scalaJSPatchIncOptions
  ) ++ (
      packageClasspathJSTasks(externalDependencyClasspath,
          packageExternalDepsJS, "-extdeps") ++
      packageClasspathJSTasks(internalDependencyClasspath,
          packageInternalDepsJS, "-intdeps") ++
      packageClasspathJSTasks(exportedProducts,
          packageExportedProductsJS, "")
  ) ++ (
      scalaJSEnvironmentSettings
  ) ++ Seq(
      packageJS := (
          packageExternalDepsJS.value ++
          packageInternalDepsJS.value ++
          packageExportedProductsJS.value
      ),

      artifactPath in preoptimizeJS :=
        ((crossTarget in preoptimizeJS).value /
            ((moduleName in preoptimizeJS).value + "-preopt.js")),

      preoptimizeJS := {
        val s = streams.value
        val classpath = jsClasspath((fullClasspath in preoptimizeJS).value)
        val output = (artifactPath in preoptimizeJS).value
        val taskCacheDir = s.cacheDirectory / "preoptimize-js"

        IO.createDirectory(output.getParentFile)

        FileFunction.cached(taskCacheDir,
            FilesInfo.lastModified, FilesInfo.exists) { dependencies =>
          s.log.info("Preoptimizing %s ..." format output)
          import ScalaJSOptimizer._
          val classpathEntries =
            ScalaJSClasspathEntries.readEntriesInClasspath(classpath)
          val optimizer = new ScalaJSOptimizer
          val outputWriter = new FileVirtualJSFileWriter(output)

          try {
            optimizer.optimize(
                Inputs(classpath = classpathEntries),
                OutputConfig(
                    name = output.name,
                    writer = outputWriter,
                    wantSourceMap = (emitSourceMaps in preoptimizeJS).value),
                s.log)
          } finally {
            outputWriter.close()
          }

          Set(output)
        } (filesToWatchForChanges(classpath))

        output
      },

      sources in optimizeJS := Seq(preoptimizeJS.value),

      artifactPath in optimizeJS :=
        ((crossTarget in optimizeJS).value /
            ((moduleName in optimizeJS).value + "-opt.js")),

      optimizeJS := {
        val s = streams.value
        val inputs = (sources in optimizeJS).value
        val output = (artifactPath in optimizeJS).value
        val taskCacheDir = s.cacheDirectory / "optimize-js"

        IO.createDirectory(output.getParentFile)

        FileFunction.cached(taskCacheDir,
            FilesInfo.lastModified, FilesInfo.exists) { dependencies =>
          s.log.info("Optimizing %s ..." format output)
          import ScalaJSClosureOptimizer._
          val optimizer = new ScalaJSClosureOptimizer
          val outputWriter = new FileVirtualJSFileWriter(output)

          try {
            optimizer.optimize(
                Inputs(
                    sources = inputs map FileVirtualJSFile,
                    additionalExterns =
                      optimizeJSExterns.value map FileVirtualJSFile),
                OutputConfig(
                    name = output.name,
                    writer = outputWriter,
                    wantSourceMap = (emitSourceMaps in optimizeJS).value,
                    prettyPrint = optimizeJSPrettyPrint.value),
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
      ))

  )

  lazy val scalaJSRunnerTask = Def.task[ScalaRun] {
    new ScalaJSEnvRun(scalaJSEnvironment.value)
  }

  val scalaJSRunSettings = Seq(
      scalaJSSetupRunner := true,
      runner in run <<= Def.taskDyn {
        if (scalaJSSetupRunner.value)
          scalaJSRunnerTask
        else
          runner in run
      }
  )

  val scalaJSCompileSettings = (
      scalaJSConfigSettings ++
      scalaJSRunSettings
  )

  val scalaJSTestFrameworkSettings = Seq(
      scalaJSTestFramework := "scala.scalajs.test.JasmineTestFramework",
      scalaJSTestBridgeClass := "scala.scalajs.test.JasmineTestBridge",

      loadedTestFrameworks := {
        val loader = testLoader.value
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
                  environment = scalaJSEnvironment.value,
                  testRunnerClass = scalaJSTestBridgeClass.value,
                  testFramework = scalaJSTestFramework.value)
          )
        } else {
          loadedTestFrameworks.value
        }
      }
  )

  val scalaJSTestSettings = (
      scalaJSConfigSettings ++
      scalaJSTestFrameworkSettings
  ) ++ (
      Seq(packageExternalDepsJS, packageInternalDepsJS,
          packageExportedProductsJS,
          preoptimizeJS, optimizeJS) map { packageJSTask =>
        moduleName in packageJSTask := moduleName.value + "-test"
      }
  )

  def defaultLoggingConsole =
      loggingConsole := Some(new LoggerConsole(streams.value.log))

  val scalaJSDefaultConfigs = (
      inConfig(Compile)(scalaJSCompileSettings) ++
      inConfig(Test)(scalaJSTestSettings)
  )

  val scalaJSProjectBaseSettings = Seq(
      relativeSourceMaps := false,
      emitSourceMaps := true,
      emitSourceMaps in packageExternalDepsJS := false,
      optimizeJSPrettyPrint := false,
      optimizeJSExterns := Seq(),

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
      addCompilerPlugin("org.scala-lang.modules.scalajs" %% "scalajs-compiler" % scalaJSVersion),

      // and of course the Scala.js library
      libraryDependencies += "org.scala-lang.modules.scalajs" %% "scalajs-library" % scalaJSVersion
  )
}
