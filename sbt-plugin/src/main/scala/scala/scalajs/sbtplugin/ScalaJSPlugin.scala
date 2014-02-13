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

import scala.collection.mutable

import SourceMapCat.catJSFilesAndTheirSourceMaps

import com.google.javascript.jscomp.{
  SourceFile => ClosureSource,
  Compiler => ClosureCompiler,
  CompilerOptions => ClosureOptions,
  _
}
import scala.collection.JavaConverters._

import environment.{Console, LoggerConsole, RhinoBasedScalaJSEnvironment}
import environment.rhino.{CodeBlock, Utilities}

import scala.scalajs.sbtplugin.testing.TestFramework

object ScalaJSPlugin extends Plugin {
  val scalaJSVersion = "0.4-SNAPSHOT"
  val scalaJSIsSnapshotVersion = scalaJSVersion endsWith "-SNAPSHOT"
  val scalaJSScalaVersion = "2.10.2"

  object ScalaJSKeys {
    val packageJS = taskKey[Seq[File]]("Package all the compiled .js files")
    val optimizeJS = taskKey[File]("Package and optimize all the compiled .js files in one file")

    val packageExternalDepsJS = taskKey[Seq[File]]("Package the .js files of external dependencies")
    val packageInternalDepsJS = taskKey[Seq[File]]("Package the .js files of internal dependencies")
    val packageExportedProductsJS = taskKey[Seq[File]]("Package the .js files the project")

    val excludeDefaultScalaLibrary = settingKey[Boolean](
        "Exclude the default Scala library from the classpath sent to Scala.js")

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
  }

  import ScalaJSKeys._

  private def isJarWithPrefix(prefixes: String*)(item: File): Boolean = {
    item.name.endsWith(".jar") && prefixes.exists(item.name.startsWith)
  }

  val isScalaJSCompilerJar = isJarWithPrefix(
      "scala-library", "scala-compiler", "scala-reflect", "scalajs-compiler",
      "scala-parser-combinators", "scala-xml") _

  def sortScalaJSOutputFiles(files: Seq[File]): Seq[File] = {
    files sortWith { (lhs, rhs) =>
      val corejslibName = "scalajs-corejslib.js"
      val lhsName = lhs.name
      val rhsName = rhs.name

      if (rhsName == corejslibName) false
      else if (lhsName == corejslibName) true
      else lhsName.compareTo(rhsName) < 0
    }
  }

  private val ScalaJSExterns = """
    /** @constructor */
    function Object() {}
    Object.protoype.toString = function() {};
    /** @constructor */
    function Array() {}
    Array.prototype.length = 0;
    /** @constructor */
    function Function() {}
    Function.prototype.constructor = function() {};
    Function.prototype.call = function() {};
    Function.prototype.apply = function() {};
    var global = {};
    """

  def packageClasspathJSTasks(classpathKey: TaskKey[Classpath],
      packageJSKey: TaskKey[Seq[File]],
      outputSuffix: String): Seq[Setting[_]] = Seq(

      classpathKey in packageJSKey := {
        val s = streams.value
        val originalClasspath = classpathKey.value

        val taskCacheDir = s.cacheDirectory / "package-js"
        IO.createDirectory(taskCacheDir)

        val taskExtractDir = taskCacheDir / "extracted-jars"
        IO.createDirectory(taskExtractDir)

        def fileID(file: File) =
          file.name + "-" + Integer.toString(file.getPath.##, 16)

        // List cp directories, and jars to extract and where

        val cpDirectories = new mutable.ListBuffer[Attributed[File]]
        val jars = mutable.Set.empty[File]

        for (cpEntry <- originalClasspath) {
          val cpFile = cpEntry.data

          if (cpFile.isDirectory) {
            cpDirectories += cpEntry
          } else if (cpFile.isFile && !isScalaJSCompilerJar(cpFile)) {
            val extractDir = taskExtractDir / fileID(cpFile)
            jars += cpFile
            cpDirectories += Attributed.blank(extractDir)
          }
        }

        // Extract jars

        val cachedExtractJars = FileFunction.cached(taskCacheDir / "extract-jars")(
            FilesInfo.lastModified, FilesInfo.exists) { (inReport, outReport) =>

          val usefulFilesFilter = ("*.js": NameFilter) | ("*.js.map")

          for (jar <- inReport.modified -- inReport.removed) {
            s.log.info("Extracting %s ..." format jar)
            val extractDir = taskExtractDir / fileID(jar)
            if (extractDir.exists)
              IO.delete(extractDir)

            IO.createDirectory(extractDir)
            IO.unzip(jar, extractDir, filter = usefulFilesFilter,
                preserveLastModified = true)
          }

          for (jar <- inReport.removed) {
            val extractDir = taskExtractDir / fileID(jar)
            if (extractDir.exists)
              IO.delete(extractDir)
          }

          (taskExtractDir ** usefulFilesFilter).get.toSet
        }

        cachedExtractJars(jars.toSet)

        cpDirectories
      },

      unmanagedSources in packageJSKey := Seq(),

      managedSources in packageJSKey := {
        // List input files (files in earlier dirs shadow files in later dirs)
        val cp = (classpathKey in packageJSKey).value

        val existingPaths = mutable.Set.empty[String]
        val inputs = new mutable.ListBuffer[File]

        for (dir <- cp.map(_.data)) {
          for (file <- (dir ** "*.js").get) {
            val path = IO.relativize(dir, file).get
            if (!existingPaths.contains(path)) {
              inputs += file
              existingPaths += path
            }
          }
        }

        // Return a sorted list of the inputs

        sortScalaJSOutputFiles(inputs.result())
      },

      sources in packageJSKey := {
        ((managedSources in packageJSKey).value ++
            (unmanagedSources in packageJSKey).value)
      },

      moduleName in packageJSKey := moduleName.value,

      artifactPath in packageJSKey :=
        ((crossTarget in packageJSKey).value /
            ((moduleName in packageJSKey).value + outputSuffix + ".js")),

      packageJSKey := {
        val s = streams.value
        val inputs = (sources in packageJSKey).value
        val output = (artifactPath in packageJSKey).value
        val taskCacheDir = s.cacheDirectory / "package-js"

        IO.createDirectory(new File(output.getParent))

        if (inputs.isEmpty) {
          if (!output.isFile || output.length != 0)
            IO.writeLines(output, Nil)
        } else {
          FileFunction.cached(taskCacheDir / "package",
              FilesInfo.lastModified, FilesInfo.exists) { dependencies =>
            s.log.info("Packaging %s ..." format output)
            catJSFilesAndTheirSourceMaps(inputs, output, relativeSourceMaps.value)
            Set(output)
          } (inputs.toSet)
        }

        Seq(output)
      }
  )

  /** Patches the IncOptions so that .js and .js.map files are pruned as needed.
   *
   *  This complicated logic patches the ClassfileManager factory of the given
   *  IncOptions with one that is aware of .js and .js.map files emitted by the
   *  Scala.js compiler. This makes sure that, when a .class file must be
   *  deleted, the corresponding .js and .js.map files are also deleted.
   */
  def scalaJSPatchIncOptions(incOptions: IncOptions): IncOptions = {
    val inheritedNewClassfileManager = incOptions.newClassfileManager
    val newClassfileManager = () => new ClassfileManager {
      val inherited = inheritedNewClassfileManager()

      def delete(classes: Iterable[File]): Unit = {
        inherited.delete(classes flatMap { classFile =>
          var jsFiles: List[File] = Nil
          val classFileName = classFile.getName
          if (classFileName endsWith ".class") {
            val parent = classFile.getParentFile
            val baseName = classFileName.substring(0, classFileName.length-6)
            val siblings = parent.listFiles()
            if (siblings ne null) {
              for (file <- siblings) {
                val name = file.getName
                if (name.length > 5 &&
                    name.charAt(4) == '-' &&
                    name.substring(0, 4).forall(Character.isDigit)) {
                  val nameWithoutPrefix = name.substring(5)
                  if (nameWithoutPrefix == baseName + ".js" ||
                      nameWithoutPrefix == baseName + ".js.map") {
                    jsFiles ::= file
                  }
                }
              }
            }
          }
          classFile :: jsFiles
        })
      }

      def generated(classes: Iterable[File]): Unit = inherited.generated(classes)
      def complete(success: Boolean): Unit = inherited.complete(success)
    }
    incOptions.copy(newClassfileManager = newClassfileManager)
  }

  val scalaJSEnvironmentTask = Def.task[ScalaJSEnvironment] {
    val inputs = (sources in scalaJSEnvironment).value
    val classpath = (fullClasspath in scalaJSEnvironment).value.map(_.data)
    val logger = streams.value.log
    val console = loggingConsole.value

    new RhinoBasedScalaJSEnvironment(inputs, classpath, console, logger.trace)
  }

  val scalaJSEnvironmentSettings = Seq(
      sources in scalaJSEnvironment := (
          (sources in packageExternalDepsJS).value ++
          (sources in packageInternalDepsJS).value ++
          (sources in packageExportedProductsJS).value
      ),

      fullClasspath in scalaJSEnvironment := (
          (externalDependencyClasspath in packageExternalDepsJS).value ++
          (internalDependencyClasspath in packageInternalDepsJS).value ++
          (exportedProducts in packageExportedProductsJS).value
      ),

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
      managedSources in packageJS := Seq(),
      unmanagedSources in packageJS := Seq(),
      sources in packageJS := {
        ((managedSources in packageJS).value ++
            (unmanagedSources in packageJS).value)
      },

      sources in packageExportedProductsJS ++=
        (sources in packageJS).value,

      packageJS := (
          packageExternalDepsJS.value ++
          packageInternalDepsJS.value ++
          packageExportedProductsJS.value
      ),

      managedSources in optimizeJS := packageJS.value,
      unmanagedSources in optimizeJS := Seq(),
      sources in optimizeJS := {
        ((managedSources in optimizeJS).value ++
            (unmanagedSources in optimizeJS).value)
      },

      moduleName in optimizeJS := moduleName.value,

      artifactPath in optimizeJS :=
        ((crossTarget in optimizeJS).value /
            ((moduleName in optimizeJS).value + "-opt.js")),

      optimizeJS := {
        val s = streams.value
        val logger = s.log
        val cacheDir = s.cacheDirectory
        val allJSFiles = (sources in optimizeJS).value
        val output = (artifactPath in optimizeJS).value

        val cachedOptimizeJS = FileFunction.cached(cacheDir / "optimize-js",
            FilesInfo.lastModified, FilesInfo.exists) { dependencies =>

          logger.info("Optimizing %s ..." format output)

          val closureSources = allJSFiles map ClosureSource.fromFile
          val closureExterns = (
              ClosureSource.fromCode("ScalaJSExterns.js", ScalaJSExterns) +:
              optimizeJSExterns.value.map(ClosureSource.fromFile(_)))

          IO.createDirectory(new File(output.getParent))

          val options = new ClosureOptions
          options.prettyPrint = optimizeJSPrettyPrint.value
          CompilationLevel.ADVANCED_OPTIMIZATIONS.setOptionsForCompilationLevel(options)
          options.setLanguageIn(ClosureOptions.LanguageMode.ECMASCRIPT5)

          val compiler = new ClosureCompiler
          val result = compiler.compile(
              closureExterns.asJava, closureSources.asJava, options)

          val errors = result.errors.toList
          val warnings = result.warnings.toList

          if (!errors.isEmpty) {
            logger.error(errors.length + " Closure errors:")
            errors.foreach(err => logger.error(err.toString))

            IO.write(output, "")
          } else {
            if (!warnings.isEmpty) {
              logger.warn(warnings.length + " Closure warnings:")
              warnings.foreach(err => logger.warn(err.toString))
            }

            IO.write(output,
                "(function(){'use strict';" +
                compiler.toSource +
                "}).call(this);\n")
          }

          Set(output)
        }

        cachedOptimizeJS(allJSFiles.toSet)

        output
      }
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
      scalaJSTestBridgeClass := "scala.scalajs.test.TestBridge",

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
          packageExportedProductsJS) map { packageJSTask =>
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
      excludeDefaultScalaLibrary := false,

      relativeSourceMaps := false,
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

      // you had better use the same version of Scala as Scala.js
      scalaVersion := scalaJSScalaVersion,

      // you will need the Scala.js compiler plugin
      autoCompilerPlugins := true,
      addCompilerPlugin("org.scala-lang.modules.scalajs" %% "scalajs-compiler" % scalaJSVersion),

      // and of course the Scala.js library
      libraryDependencies += "org.scala-lang.modules.scalajs" %% "scalajs-library" % scalaJSVersion
  )
}
