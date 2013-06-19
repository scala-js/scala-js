import sbt._
import Keys._
import Process.cat

import SourceMapCat.catJSFilesAndTheirSourceMaps

object ScalaJSBuild extends Build {

  val scalajsScalaVersion = "2.10.1"

  val packageJS = TaskKey[File]("package-js")

  val defaultSettings = Defaults.defaultSettings ++ Seq(
      scalaVersion := scalajsScalaVersion,
      scalacOptions ++= Seq(
          "-deprecation",
          "-unchecked",
          "-feature",
          "-encoding", "utf8"
      ),
      organization := "ch.epfl.lamp",
      version := "0.1-SNAPSHOT",

      normalizedName ~= { _.replace("scala.js", "scalajs") }
  )

  lazy val root = Project(
      id = "scalajs",
      base = file("."),
      settings = defaultSettings ++ Seq(
          name := "Scala.js",
          publishArtifact in Compile := false,
          packageJS in Compile <<= (
              target,
              packageJS in (corejslib, Compile),
              packageJS in (javalib, Compile),
              packageJS in (scalalib, Compile),
              packageJS in (libraryAux, Compile),
              packageJS in (library, Compile)
          ) map { (target, corejslib, javalib, scalalib, libraryAux, library) =>
            val allJSFiles =
              Seq(corejslib, javalib, scalalib, libraryAux, library)
            val output = target / ("scalajs-runtime.js")
            target.mkdir()
            catJSFilesAndTheirSourceMaps(allJSFiles, output)
            output
          }
      )
  ).aggregate(
      compiler, library
  )

  lazy val compiler = Project(
      id = "scalajs-compiler",
      base = file("compiler"),
      settings = defaultSettings ++ Seq(
          name := "Scala.js compiler",
          libraryDependencies ++= Seq(
              "org.scala-lang" % "scala-compiler" % scalajsScalaVersion,
              "org.scala-lang" % "scala-reflect" % scalajsScalaVersion
          ),
          mainClass := Some("scala.tools.nsc.scalajs.Main")
      )
  )

  def compileJSSettings(packageName: String) = Seq(
      compile in Compile <<= (
          javaHome, dependencyClasspath in Compile, runner,
          sources in Compile, classDirectory in Compile
      ) map { (javaHome, cp, runner, sources, classDir) =>
        val logger = ConsoleLogger()

        def doCompileJS(sourcesArgs: List[String]) = {
          Run.executeTrapExit({
            classDir.mkdir()
            val classpath = cp.map(
                _.data.getAbsolutePath()).mkString(java.io.File.pathSeparator)
            Fork.java(javaHome,
                ("-Xbootclasspath/a:" + classpath) ::
                "-Xmx512M" ::
                "scala.tools.nsc.scalajs.Main" ::
                "-d" :: classDir.getAbsolutePath() ::
                //"-verbose" ::
                sourcesArgs,
                logger)
            /*Run.run("scala.tools.nsc.scalajs.Main", cp map (_.data),
                "-d" :: classDir.getAbsolutePath() ::
                sources.map(_.getAbsolutePath()).toList,
                logger)(runner)*/
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
          compile in Compile, target in Compile, classDirectory in Compile
      ) map { (compilationResult, target, classDir) =>
        val allJSFiles = (classDir ** "*.js").get
        val output = target / (packageName + ".js")
        catJSFilesAndTheirSourceMaps(allJSFiles, output)
        output
      }
  )

  lazy val corejslib = Project(
      id = "scalajs-corejslib",
      base = file("corejslib"),
      settings = defaultSettings ++ Seq(
          name := "Scala.js core JS runtime",
          publishArtifact in Compile := false,

          packageJS in Compile <<= (
              baseDirectory, target in Compile
          ) map { (baseDirectory, target) =>
            // hard-coded because order matters!
            val fileNames =
              Seq("scalajsenv.js", "javalangObject.js", "RefTypes.js")

            val allJSFiles = fileNames map (baseDirectory / _)
            val output = target / ("scalajs-corejslib.js")
            target.mkdir()
            catJSFilesAndTheirSourceMaps(allJSFiles, output)
            output
          }
      )
  )

  lazy val javalib = Project(
      id = "scalajs-javalib",
      base = file("javalib"),
      settings = defaultSettings ++ compileJSSettings("scalajs-javalib") ++ Seq(
          name := "Java library for Scala.js",
          publishArtifact in Compile := false,

          // Override packageJS to exclude scala.js._
          packageJS in Compile <<= (
              compile in Compile, target in Compile, classDirectory in Compile
          ) map { (compilationResult, target, classDir) =>
            val allJSFiles =
              ((classDir ** "*.js") ---
                  (classDir / "scala" / "js" ** "*.js")).get
            val output = target / ("scalajs-javalib.js")
            catJSFilesAndTheirSourceMaps(allJSFiles, output)
            output
          }
      )
  ).dependsOn(compiler)

  lazy val scalalib = Project(
      id = "scalajs-scalalib",
      base = file("scalalib"),
      settings = defaultSettings ++ compileJSSettings("scalajs-scalalib") ++ Seq(
          name := "Scala library for Scala.js",
          publishArtifact in Compile := false
      )
  ).dependsOn(compiler)

  lazy val libraryAux = Project(
      id = "scalajs-library-aux",
      base = file("library-aux"),
      settings = defaultSettings ++ compileJSSettings("scalajs-library-aux") ++ Seq(
          name := "Scala.js aux library",
          publishArtifact in Compile := false
      )
  ).dependsOn(compiler)

  lazy val library = Project(
      id = "scalajs-library",
      base = file("library"),
      settings = defaultSettings ++ compileJSSettings("scalajs-library") ++ Seq(
          name := "Scala.js library"
      )
  ).dependsOn(compiler)

  // Examples

  lazy val examples = Project(
      id = "examples",
      base = file("examples"),
      settings = defaultSettings ++ Seq(
          name := "Scala.js examples"
      )
  ).aggregate(exampleHelloWorld, exampleReversi)

  lazy val exampleSettings = Seq(
      /* Add the library classpath this way to escape the dependency between
       * tasks. This avoids to recompile the library every time we compile an
       * example. This is all about working around the lack of dependency
       * analysis.
       */
      unmanagedClasspath in Compile <+= (
          classDirectory in (library, Compile)
      ) map { classDir =>
        Attributed.blank(classDir)
      }
  )

  lazy val exampleHelloWorld = Project(
      id = "helloworld",
      base = file("examples") / "helloworld",
      settings = defaultSettings ++ compileJSSettings("helloworld") ++ exampleSettings ++ Seq(
          name := "Hello World - Scala.js example"
      )
  ).dependsOn(compiler)

  lazy val exampleReversi = Project(
      id = "reversi",
      base = file("examples") / "reversi",
      settings = defaultSettings ++ compileJSSettings("reversi") ++ exampleSettings ++ Seq(
          name := "Reversi - Scala.js example"
      )
  ).dependsOn(compiler)
}
