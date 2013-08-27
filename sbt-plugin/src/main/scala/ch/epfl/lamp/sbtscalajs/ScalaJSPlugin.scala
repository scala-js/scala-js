package ch.epfl.lamp.sbtscalajs

import sbt._
import Keys._

import SourceMapCat.catJSFilesAndTheirSourceMaps

import com.google.javascript.jscomp.{
  SourceFile => ClosureSource,
  Compiler => ClosureCompiler,
  CompilerOptions => ClosureOptions,
  _
}
import scala.collection.JavaConversions._

object ScalaJSPlugin extends Plugin {
  object ScalaJSKeys {
    val packageJS = TaskKey[File]("package-js")
    val optimizeJS = TaskKey[File]("optimize-js")

    val excludeDefaultScalaLibrary = SettingKey[Boolean]("exclude-default-scala-library")

    val optimizeJSPrettyPrint = SettingKey[Boolean]("optimize-js-pretty-print")
    val optimizeJSExterns = TaskKey[Seq[File]]("optimize-js-externs")
  }

  import ScalaJSKeys._

  def isScalaJSCompilerJar(item: File): Boolean = {
    val compilerModuleNames =
      Seq("scala-library", "scala-compiler", "scala-reflect",
          "scalajs-compiler")
    val name = item.getName
    name.endsWith(".jar") && compilerModuleNames.exists(name.startsWith)
  }

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
    var Math = {
      ceil: function() {},
      floor: function() {}
    }
    """

  val baseScalaJSSettings: Seq[Setting[_]] = Seq(
      // you had better use the same version of Scala as Scala.js
      scalaVersion := "2.10.1",

      excludeDefaultScalaLibrary := false,

      compile in Compile <<= (
          javaHome, streams, compileInputs in Compile,
          excludeDefaultScalaLibrary in Compile
      ) map { (javaHome, s, inputs, excludeDefaultScalaLibrary) =>
        import inputs.config._

        val logger = s.log

        logger.info(
            "Compiling %d Scala.js sources to %s..." format (
            sources.size, classesDirectory))

        def cpToString(cp: Seq[File]) =
          cp.map(_.getAbsolutePath).mkString(java.io.File.pathSeparator)

        val (compilerCp, cp) = classpath.partition(isScalaJSCompilerJar)
        val compilerCpStr = cpToString(compilerCp)
        val cpStr = cpToString(cp)

        val javaClasspath =
          if (excludeDefaultScalaLibrary) List("-cp", compilerCpStr)
          else List("-Xbootclasspath/a:" + compilerCpStr)

        def doCompileJS(sourcesArgs: List[String]) = {
          Run.executeTrapExit({
            classesDirectory.mkdir()

            Fork.java(javaHome,
                javaClasspath :::
                "-Xmx512M" ::
                "scala.tools.nsc.scalajs.Main" ::
                "-cp" :: cpStr ::
                "-d" :: classesDirectory.getAbsolutePath() ::
                options ++:
                sourcesArgs,
                logger)
          }, logger)
        }

        val sourcesArgs = sources.map(_.getAbsolutePath()).toList

        /* Crude way of overcoming the Windows limitation on command line
         * length.
         */
        if ((System.getProperty("os.name").toLowerCase().indexOf("win") >= 0) &&
            (sourcesArgs.map(_.length).sum > 1536)) {
          IO.withTemporaryFile("sourcesargs", ".txt") { sourceListFile =>
            IO.writeLines(sourceListFile, sourcesArgs)
            doCompileJS(List("@"+sourceListFile.getAbsolutePath()))
          }
        } else {
          doCompileJS(sourcesArgs)
        }

        // We do not have dependency analysis for Scala.js code
        sbt.inc.Analysis.Empty
      },

      packageJS in Compile <<= (
          streams, fullClasspath in Compile,
          crossTarget in Compile, moduleName
      ) map { (s, fullCp, target, modName) =>
        val output = target / (modName + ".js")
        s.log.info("Packaging %s..." format (output))

        val tempDir = target / "package-js"
        IO.createDirectory(tempDir)

        val usefulFilesFilter = ("*.js": NameFilter) | ("*.js.map")

        for (cpFile <- fullCp.map(_.data).reverse) {
          if (cpFile.isDirectory) {
            s.log.info("Copying %s..." format cpFile)
            for (f <- (cpFile ** usefulFilesFilter).get) {
              val relative = file(IO.relativize(cpFile, f).get)
              val dest = IO.resolve(tempDir, relative)
              IO.copyFile(f, dest, preserveLastModified = true)
            }
          } else if (cpFile.isFile && !isScalaJSCompilerJar(cpFile)) {
            s.log.info("Extracting %s..." format cpFile)
            IO.unzip(cpFile, tempDir, filter = usefulFilesFilter,
                preserveLastModified = true)
          }
        }

        s.log.info("Packaging...")
        val allJSFiles = (tempDir ** "*.js").get
        val sortedJSFiles = sortScalaJSOutputFiles(allJSFiles)
        catJSFilesAndTheirSourceMaps(sortedJSFiles, output)
        output
      },

      // TODO Leverage the sbt-js plugin from Untyped

      unmanagedSources in (Compile, optimizeJS) := Seq(),

      managedSources in (Compile, optimizeJS) := Seq(),

      sources in (Compile, optimizeJS) <<= (
          packageJS in Compile,
          managedSources in (Compile, optimizeJS),
          unmanagedSources in (Compile, optimizeJS)
      ) map { (pack, managed, unmanaged) =>
        (pack +: managed) ++ unmanaged
      },

      optimizeJSPrettyPrint := false,

      optimizeJSExterns := Seq(),

      optimizeJS in Compile <<= (
          streams, sources in (Compile, optimizeJS),
          optimizeJSPrettyPrint in Compile, optimizeJSExterns in Compile,
          crossTarget in Compile, moduleName
      ) map { (s, allJSFiles, prettyPrint, externs, target, modName) =>
        val logger = s.log

        logger.info("Running Closure with files:")
        for (file <- allJSFiles)
          logger.info("  "+file)

        val closureSources = allJSFiles map ClosureSource.fromFile
        val closureExterns = (
            ClosureSource.fromCode("ScalaJSExterns.js", ScalaJSExterns) +:
            externs.map(ClosureSource.fromFile(_)))
        val output = target / (modName + "-opt.js")

        IO.createDirectory(new File(output.getParent))

        val options = new ClosureOptions
        options.prettyPrint = prettyPrint
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

        output
      }
  )

  val scalaJSSettings: Seq[Setting[_]] = baseScalaJSSettings ++ Seq(
      // you had better use the same version of Scala as Scala.js
      scalaVersion := "2.10.1",

      // you will need the Scala.js compiler on the classpath
      libraryDependencies += "ch.epfl.lamp" %% "scalajs-compiler" % "0.1-SNAPSHOT",

      // and of course the Scala.js library
      libraryDependencies += "ch.epfl.lamp" %% "scalajs-library" % "0.1-SNAPSHOT"
  )
}
