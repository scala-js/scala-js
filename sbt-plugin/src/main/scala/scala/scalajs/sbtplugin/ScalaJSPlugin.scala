/* Scala.js sbt plugin
 * Copyright 2013 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

package scala.scalajs.sbtplugin

import sbt._
import inc.{ IncOptions, ClassfileManager }
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
  val scalaJSVersion = "0.2-SNAPSHOT"
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
  }

  import ScalaJSKeys._

  private def isJarWithPrefix(prefixes: String*)(item: File): Boolean = {
    item.name.endsWith(".jar") && prefixes.exists(item.name.startsWith)
  }

  private val isScalaLibraryJar = isJarWithPrefix(
      "scala-library", "scala-reflect") _

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
    var global = {};
    """

  /** A proxy for a Logger that looks like a Mozilla console object */
  private class LoggingConsole(logger: Logger) {
    def log(x: Any): Unit = logger.info(x.toString)
    def info(x: Any): Unit = logger.info(x.toString)
    def warn(x: Any): Unit = logger.warn(x.toString)
    def error(x: Any): Unit = logger.error(x.toString)
  }

  val scalaJSExternalCompileConfigSettings: Seq[Setting[_]] = inTask(compile)(
      Defaults.runnerTask
  ) ++ Seq(
      fork in compile := true,
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

        val allMyDependencies = classpath filterNot (_ == classesDirectory) flatMap { cpFile =>
          if (cpFile.isDirectory) (cpFile ** "*.class").get
          else Seq(cpFile)
        }

        // Compile

        val cachedCompile = FileFunction.cached(cacheDir / "compile",
            FilesInfo.lastModified, FilesInfo.exists) { dependencies =>

          logger.info(
              "Compiling %d Scala sources to %s..." format (
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
                  !msg.startsWith("Running scala.tools.nsc.Main"))
                logger.log(level, msg)
            }
            def success(message: => String) = logger.success(message)
            def trace(t: => Throwable) = logger.trace(t)
          }

          def doCompile(sourcesArgs: List[String]): Unit = {
            val run = (runner in compile).value
            run.run("scala.tools.nsc.Main", compilerCp,
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
              doCompile(List("@"+sourceListFile.getAbsolutePath()))
            }
          } else {
            doCompile(sourcesArgs)
          }

          // Output is all files in classesDirectory
          (classesDirectory ** AllPassFilter).get.toSet
        }

        cachedCompile((sources ++ allMyDependencies).toSet)

        // We do not have dependency analysis when compiling externally
        sbt.inc.Analysis.Empty
      }
  )

  val scalaJSExternalCompileSettings = (
      inConfig(Compile)(scalaJSExternalCompileConfigSettings) ++
      inConfig(Test)(scalaJSExternalCompileConfigSettings)
  ) ++ Seq(
      libraryDependencies +=
        "org.scala-lang" % "scala-compiler" % scalaVersion.value
  )

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

      packageJSKey := {
        val s = streams.value
        val inputs = (sources in packageJSKey).value
        val output = crossTarget.value / (moduleName.value + outputSuffix + ".js")
        val taskCacheDir = s.cacheDirectory / "package-js"

        IO.createDirectory(crossTarget.value)

        if (inputs.isEmpty) {
          if (!output.isFile || output.length != 0)
            IO.writeLines(output, Nil)
        } else {
          FileFunction.cached(taskCacheDir / "package",
              FilesInfo.lastModified, FilesInfo.exists) { dependencies =>
            s.log.info("Packaging %s ..." format output)
            catJSFilesAndTheirSourceMaps(inputs, output)
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

  val scalaJSConfigSettings: Seq[Setting[_]] = Seq(
      incOptions ~= scalaJSPatchIncOptions
  ) ++ (
      packageClasspathJSTasks(externalDependencyClasspath,
          packageExternalDepsJS, "-extdeps") ++
      packageClasspathJSTasks(internalDependencyClasspath,
          packageInternalDepsJS, "-intdeps") ++
      packageClasspathJSTasks(exportedProducts,
          packageExportedProductsJS, "")
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
    val console = new LoggingConsole(s.log)
    scalaJSRunJavaScript(sources.value, s.log.trace, Some(console), true,
        classpath.value.map(_.data))
  }

  def scalaJSRunInputsSettings(scoped: Scoped) = Seq(
      sources in scoped := (
          (sources in packageExternalDepsJS).value ++
          (sources in packageInternalDepsJS).value ++
          (sources in packageExportedProductsJS).value
      ),

      fullClasspath in scoped := (
          (externalDependencyClasspath in packageExternalDepsJS).value ++
          (internalDependencyClasspath in packageInternalDepsJS).value ++
          (exportedProducts in packageExportedProductsJS).value
      )
  )

  val scalaJSRunSettings = scalaJSRunInputsSettings(run) ++ Seq(
      run := {
        scalaJSRunJavaScriptTask(streams, sources in run,
            fullClasspath in run).value
      }
  )

  val scalaJSCompileSettings = scalaJSConfigSettings ++ scalaJSRunSettings

  val scalaJSTestSettings = scalaJSConfigSettings ++ scalaJSRunInputsSettings(test) ++ Seq(
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

  val scalaJSReleasesResolver = Resolver.url("scala-js-releases",
      url("http://repo.scala-js.org/repo/releases/"))(Resolver.ivyStylePatterns)
  val scalaJSSnapshotsResolver = Resolver.url("scala-js-snapshots",
      url("http://repo.scala-js.org/repo/snapshots/"))(Resolver.ivyStylePatterns)

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
