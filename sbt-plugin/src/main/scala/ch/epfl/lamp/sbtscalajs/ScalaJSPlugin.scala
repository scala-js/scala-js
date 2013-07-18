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
  }

  import ScalaJSKeys._

  def sortScalaJSOutputFiles(files: Seq[File]): Seq[File] = {
    files.sortBy(_.name)
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

      compile in Compile <<= (
          javaHome, streams, compileInputs in Compile
      ) map { (javaHome, s, inputs) =>
        import inputs.config._

        val logger = s.log

        logger.info(
            "Compiling %d Scala.js sources to %s..." format (
            sources.size, classesDirectory))

        def isCompilerJar(item: File): Boolean = {
          val compilerModuleNames =
            Seq("scala-library", "scala-compiler", "scala-reflect",
                "scalajs-compiler")
          val name = item.getName
          name.endsWith(".jar") && compilerModuleNames.exists(name.startsWith)
        }

        def cpToString(cp: Seq[File]) =
          cp.map(_.getAbsolutePath).mkString(java.io.File.pathSeparator)

        val (compilerCp, cp) = classpath.partition(isCompilerJar)
        val compilerCpStr = cpToString(compilerCp)
        val cpStr = cpToString(cp)

        def doCompileJS(sourcesArgs: List[String]) = {
          Run.executeTrapExit({
            classesDirectory.mkdir()

            Fork.java(javaHome,
                ("-Xbootclasspath/a:" + compilerCpStr) ::
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
          compile in Compile, classDirectory in Compile,
          crossTarget in Compile, moduleName
      ) map { (compilationResult, classDir, target, modName) =>
        val allJSFiles = (classDir ** "*.js").get
        val sortedJSFiles = sortScalaJSOutputFiles(allJSFiles)
        val output = target / (modName + ".js")
        catJSFilesAndTheirSourceMaps(sortedJSFiles, output)
        output
      },

      // TODO Leverage the sbt-js plugin from Untyped

      unmanagedSources in (Compile, optimizeJS) := Seq(),

      managedSources in (Compile, optimizeJS) <<= (
          packageJS in Compile
      ) map { Seq(_) },

      sources in (Compile, optimizeJS) <<= (
          managedSources in (Compile, optimizeJS),
          unmanagedSources in (Compile, optimizeJS)
      ) map { (managed, unmanaged) =>
        val all = managed ++ unmanaged
        val (runtime, others) = all.partition(_.getName == "scalajs-runtime.js")
        runtime ++ others
      },

      optimizeJS in Compile <<= (
          streams, sources in (Compile, optimizeJS),
          crossTarget in Compile, moduleName
      ) map { (s, allJSFiles, target, modName) =>
        val logger = s.log

        logger.info("Running Closure with files:")
        for (file <- allJSFiles)
          logger.info("  "+file)

        val closureSources = allJSFiles map ClosureSource.fromFile
        val closureExterns = List(
            ClosureSource.fromCode("ScalaJSExterns.js", ScalaJSExterns))
        val output = target / (modName + "-opt.js")

        IO.createDirectory(new File(output.getParent))

        val options = new ClosureOptions
        options.prettyPrint = true
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
