package ch.epfl.lamp.sbtscalajs

import sbt._
import Keys._

import SourceMapCat.catJSFilesAndTheirSourceMaps

object ScalaJSPlugin extends Plugin {
  object ScalaJSKeys {
    val packageJS = TaskKey[File]("package-js")
  }

  import ScalaJSKeys._

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
        val output = target / (modName + ".js")
        catJSFilesAndTheirSourceMaps(allJSFiles, output)
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
