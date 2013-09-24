package ch.epfl.lamp.sbtscalajs

import sbt._
import Keys._

import scala.collection.mutable

import SourceMapCat.catJSFilesAndTheirSourceMaps
import RhinoBasedRun._

import com.google.javascript.jscomp.{
  SourceFile => ClosureSource,
  Compiler => ClosureCompiler,
  CompilerOptions => ClosureOptions,
  _
}
import scala.collection.JavaConversions._

import org.mozilla.{ javascript => rhino }

object ScalaJSPlugin extends Plugin {
  object ScalaJSKeys {
    val packageJS = taskKey[File]("Package the compiled .js files in one file")
    val optimizeJS = taskKey[File]("Package and optimize the compiled .js files in one file")

    val excludeDefaultScalaLibrary = settingKey[Boolean](
        "Exclude the default Scala library from the classpath sent to Scala.js")

    val optimizeJSPrettyPrint = settingKey[Boolean](
        "Pretty-print the output of optimizeJS")
    val optimizeJSExterns = taskKey[Seq[File]](
        "Extern files to use with optimizeJS")
  }

  import ScalaJSKeys._

  private def isJarWithPrefix(prefixes: String*)(item: File): Boolean = {
    item.name.endsWith(".jar") && prefixes.exists(item.name.startsWith)
  }

  private val isScalaLibraryJar = isJarWithPrefix("scala-library") _

  val isScalaJSCompilerJar = isJarWithPrefix(
      "scala-library", "scala-compiler", "scala-reflect", "scalajs-compiler") _

  private val isWindows =
    System.getProperty("os.name").toLowerCase().indexOf("win") >= 0

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
    """

  val scalaJSConfigSettings: Seq[Setting[_]] = inTask(compile)(
      Defaults.runnerTask
  ) ++ Seq(
      fork in compile := isWindows, // not forking does not seem to work on Win
      trapExit in compile := true,
      javaOptions in compile += "-Xmx512M",

      compile := {
        val inputs = (compileInputs in compile).value
        import inputs.config._

        val s = streams.value
        val logger = s.log
        val cacheDir = s.cacheDirectory

        // Discover classpaths

        def cpToString(cp: Seq[File]) =
          cp.map(_.getAbsolutePath).mkString(java.io.File.pathSeparator)

        val (compilerCp, cp0) = classpath.partition(isScalaJSCompilerJar)
        val cp =
          if (excludeDefaultScalaLibrary.value) cp0
          else cp0 ++ compilerCp.filter(isScalaLibraryJar)

        val cpStr = cpToString(cp)

        // List all my dependencies (recompile if any of these changes)

        val isClassOrJstypeFile = ("*.class": NameFilter) | "*.jstype"
        val allMyDependencies = classpath filterNot (_ == classesDirectory) flatMap { cpFile =>
          if (cpFile.isDirectory) (cpFile ** isClassOrJstypeFile).get
          else Seq(cpFile)
        }

        // Compile

        val cachedCompile = FileFunction.cached(cacheDir / "compile-js",
            FilesInfo.lastModified, FilesInfo.exists) { dependencies =>

          logger.info(
              "Compiling %d Scala.js sources to %s..." format (
              sources.size, classesDirectory))

          if (classesDirectory.exists)
            IO.delete(classesDirectory)
          IO.createDirectory(classesDirectory)

          val sourcesArgs = sources.map(_.getAbsolutePath()).toList

          /* run.run() below in doCompileJS() will emit a call to its
           * logger.info("Running scala.tools.nsc.scalajs.Main [...]")
           * which we do not want to see. We use this patched logger to
           * filter out that particular message.
           */
          val patchedLogger = new Logger {
            def log(level: Level.Value, message: => String) = {
              val msg = message
              if (level != Level.Info ||
                  !msg.startsWith("Running scala.tools.nsc.scalajs.Main"))
                logger.log(level, msg)
            }
            def success(message: => String) = logger.success(message)
            def trace(t: => Throwable) = logger.trace(t)
          }

          def doCompileJS(sourcesArgs: List[String]): Unit = {
            val run = (runner in compile).value
            run.run("scala.tools.nsc.scalajs.Main", compilerCp,
                "-cp" :: cpStr ::
                "-d" :: classesDirectory.getAbsolutePath() ::
                options ++:
                sourcesArgs,
                patchedLogger) foreach sys.error
          }

          /* Crude way of overcoming the Windows limitation on command line
           * length.
           */
          if ((fork in compile).value && isWindows &&
              (sourcesArgs.map(_.length).sum > 1536)) {
            IO.withTemporaryFile("sourcesargs", ".txt") { sourceListFile =>
              IO.writeLines(sourceListFile, sourcesArgs)
              doCompileJS(List("@"+sourceListFile.getAbsolutePath()))
            }
          } else {
            doCompileJS(sourcesArgs)
          }

          // Output is all files in classesDirectory
          (classesDirectory ** AllPassFilter).get.toSet
        }

        cachedCompile((sources ++ allMyDependencies).toSet)

        // We do not have dependency analysis for Scala.js code
        sbt.inc.Analysis.Empty
      },

      fullClasspath in packageJS := {
        val s = streams.value
        val originalFullClasspath = fullClasspath.value

        val taskCacheDir = s.cacheDirectory / "package-js"
        IO.createDirectory(taskCacheDir)

        val taskExtractDir = taskCacheDir / "extracted-jars"
        IO.createDirectory(taskExtractDir)

        def fileID(file: File) =
          file.name + "-" + Integer.toString(file.getPath.##, 16)

        // List cp directories, and jars to extract and where

        val cpDirectories = new mutable.ListBuffer[Attributed[File]]
        val jars = mutable.Set.empty[File]

        for (cpEntry <- originalFullClasspath) {
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

      unmanagedSources in packageJS := Seq(),

      managedSources in packageJS := {
        // List input files (files in earlier dirs shadow files in later dirs)
        val cp = (fullClasspath in packageJS).value

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

      sources in packageJS := {
        ((managedSources in packageJS).value ++
            (unmanagedSources in packageJS).value)
      },

      packageJS := {
        val s = streams.value
        val inputs = (sources in packageJS).value
        val output = crossTarget.value / (moduleName.value + ".js")
        val taskCacheDir = s.cacheDirectory / "package-js"

        val cachedPackage = FileFunction.cached(taskCacheDir / "package",
            FilesInfo.lastModified, FilesInfo.exists) { dependencies =>
          s.log.info("Packaging %s ..." format output)
          catJSFilesAndTheirSourceMaps(inputs, output)
          Set(output)
        }

        cachedPackage(inputs.toSet)

        output
      },

      // TODO Leverage the sbt-js plugin from Untyped

      unmanagedSources in optimizeJS := Seq(),

      managedSources in optimizeJS := Seq(),

      sources in optimizeJS := {
        ((sources in packageJS).value ++
            (managedSources in optimizeJS).value ++
            (unmanagedSources in optimizeJS).value)
      },

      optimizeJS := {
        val s = streams.value
        val logger = s.log
        val cacheDir = s.cacheDirectory
        val allJSFiles = (sources in optimizeJS).value
        val output = crossTarget.value / (moduleName.value + "-opt.js")

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
              closureExterns, closureSources, options)

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

            IO.write(output, compiler.toSource)
          }

          Set(output)
        }

        cachedOptimizeJS(allJSFiles.toSet)

        output
      }
  )

  def scalaJSRunJavaScriptTask(streams: TaskKey[TaskStreams],
      sources: TaskKey[Seq[File]], classpath: TaskKey[Classpath]) = Def.task {
    val s = streams.value
    s.log.info("Running ...")
    scalaJSRunJavaScript(s.log, sources.value, true,
        classpath.value.map(_.data))
  }

  val scalaJSRunSettings = Seq(
      sources in run := (sources in packageJS).value,
      fullClasspath in run := (fullClasspath in packageJS).value,

      run := {
        scalaJSRunJavaScriptTask(streams, sources in run,
            fullClasspath in run).value
      }
  )

  val scalaJSCompileSettings = scalaJSConfigSettings ++ scalaJSRunSettings

  val scalaJSTestSettings = scalaJSConfigSettings ++ Seq(
      sources in test := (sources in packageJS).value,
      fullClasspath in test := (fullClasspath in packageJS).value,

      test := {
        scalaJSRunJavaScriptTask(streams, sources in test,
            fullClasspath in test).value
      }
  )

  val scalaJSDefaultConfigs = (
      inConfig(Compile)(scalaJSCompileSettings) ++
      inConfig(Test)(scalaJSTestSettings)
  )

  val scalaJSProjectBaseSettings = Seq(
      excludeDefaultScalaLibrary := false,

      optimizeJSPrettyPrint := false,
      optimizeJSExterns := Seq()
  )

  val scalaJSAbstractSettings: Seq[Setting[_]] = (
      scalaJSProjectBaseSettings ++
      scalaJSDefaultConfigs
  )

  val scalaJSSettings: Seq[Setting[_]] = scalaJSAbstractSettings ++ Seq(
      // you had better use the same version of Scala as Scala.js
      scalaVersion := "2.10.2",

      // you will need the Scala.js compiler on the classpath
      libraryDependencies += "ch.epfl.lamp" %% "scalajs-compiler" % "0.1-SNAPSHOT",

      // and of course the Scala.js library
      libraryDependencies += "ch.epfl.lamp" %% "scalajs-library" % "0.1-SNAPSHOT"
  )
}
