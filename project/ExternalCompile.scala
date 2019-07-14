package build

import sbt._
import Keys._

import org.scalajs.sbtplugin.ScalaJSPlugin

object ExternalCompile {

  private val isWindows =
    System.getProperty("os.name").toLowerCase().indexOf("win") >= 0

  val scalaJSExternalCompileConfigSettings: Seq[Setting[_]] = inTask(compile)(
      Defaults.runnerTask
  ) ++ Seq(
      fork in compile := true,
      trapExit in compile := true,
      javaOptions in compile += "-Xmx512M",

      javaOptions in compile ++= {
        val scalaExtDirs = System.getProperty("scala.ext.dirs")
        if (scalaExtDirs != null && (fork in compile).value)
          Seq("-Dscala.ext.dirs=" + scalaExtDirs)
        else
          Nil
      },

      compile := {
        val inputs = (compileInputs in compile).value
        import inputs.config._

        val s = streams.value
        val logger = s.log
        val cacheDir = s.cacheDirectory

        // Discover classpaths

        def cpToString(cp: Seq[File]) =
          cp.map(_.getAbsolutePath).mkString(java.io.File.pathSeparator)

        val compilerCp = inputs.compilers.scalac.scalaInstance.allJars
        val cpStr = cpToString(classpath)

        // List all my dependencies (recompile if any of these changes)

        val allMyDependencies = classpath filterNot (_ == classesDirectory) flatMap { cpFile =>
          if (cpFile.isDirectory) (cpFile ** "*.class").get
          else Seq(cpFile)
        }

        // Compile

        val outputDirectory = cacheDir / "compile-out"
        val cachedCompile = FileFunction.cached(cacheDir / "compile-cache",
            FilesInfo.lastModified, FilesInfo.exists) { dependencies =>

          logger.info(
              "Compiling %d Scala sources to %s..." format (
              sources.size, classesDirectory))

          if (outputDirectory.exists)
            IO.delete(outputDirectory)
          IO.createDirectory(outputDirectory)

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
            val optErrorMsg = run.run("scala.tools.nsc.Main", compilerCp,
                "-cp" :: cpStr ::
                "-d" :: outputDirectory.getAbsolutePath() ::
                options ++:
                sourcesArgs,
                patchedLogger)
            optErrorMsg.foreach(errorMsg => throw new Exception(errorMsg))
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

          // Copy to classes directory.
          val mappings = (outputDirectory ** AllPassFilter)
            .pair(Path.rebase(outputDirectory, classesDirectory))
          Sync.sync(s.cacheStoreFactory.make("compile-copy"))(mappings)

          mappings.unzip._2.toSet
        }

        cachedCompile((sources ++ allMyDependencies).toSet)

        // We do not have dependency analysis when compiling externally
        sbt.inc.Analysis.Empty
      }
  )

  val scalaJSExternalCompileSettings = (
      inConfig(Compile)(scalaJSExternalCompileConfigSettings) ++
      inConfig(Test)(scalaJSExternalCompileConfigSettings)
  )

}
