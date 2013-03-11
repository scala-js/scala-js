import sbt._
import Keys._
import Process.cat

object ScalaJSBuild extends Build {

  val scalajsScalaVersion = "2.10.0"

  val defaultSettings = Defaults.defaultSettings ++ Seq(
      scalaVersion := scalajsScalaVersion,
      scalacOptions ++= Seq(
          "-deprecation",
          "-unchecked",
          "-feature",
          "-encoding", "utf8"
      ),
      organization := "ch.epfl.lamp",
      version := "0.1-SNAPSHOT"
  )

  lazy val root = Project(
      id = "scalajs",
      base = file("."),
      settings = defaultSettings ++ Seq(
          packageJS in Compile <<= (
              target, name,
              packageJS in (corejslib, Compile),
              packageJS in (javalib, Compile),
              packageJS in (scalalib, Compile),
              packageJS in (libraryAux, Compile),
              packageJS in (library, Compile)
          ) map { (target, name, corejslib, javalib, scalalib, libraryAux, library) =>
            val allJSFiles =
              Seq(corejslib, javalib, scalalib, libraryAux, library)
            val output = target / (name + "-runtime.js")
            target.mkdir()
            cat(allJSFiles) #> output ! ;
            output
          }
      )
  ).aggregate(
      compiler, corejslib, javalib, scalalib, libraryAux, library
  )

  lazy val compiler = Project(
      id = "scalajs-compiler",
      base = file("compiler"),
      settings = defaultSettings ++ Seq(
          libraryDependencies ++= Seq(
              "org.scala-lang" % "scala-compiler" % scalajsScalaVersion,
              "org.scala-lang" % "scala-reflect" % scalajsScalaVersion
          ),
          mainClass := Some("scala.tools.nsc.scalajs.Main")
      )
  )

  val sourcesJS = TaskKey[Seq[File]]("sources-js")
  val compileJS = TaskKey[Unit]("compile-js")
  val packageJS = TaskKey[File]("package-js")

  val compileJSSettings = Seq(
      sourcesJS <<= sources in Compile,

      compileJS in Compile <<= (
          fullClasspath in Compile, runner,
          sourcesJS, target in Compile
      ) map { (cp, runner, sources, target) =>
        val logger = ConsoleLogger()
        Run.executeTrapExit({
          //Run.run("scala.tools.nsc.scalajs.Main", cp map (_.data),
          //    sources.map(_.getAbsolutePath()), logger)(runner)
          val out = target / "jsclasses"
          out.mkdir()
          Process("java" ::
              ("-Xbootclasspath/a:" + cp.map(_.data.getAbsolutePath()).mkString(":")) ::
              "scala.tools.nsc.scalajs.Main" ::
              "-d" :: out.getAbsolutePath() ::
              //"-verbose" ::
              sources.map(_.getAbsolutePath()).toList) !
        }, logger)
      },

      packageJS in Compile <<= (
          compileJS in Compile, target in Compile, name
      ) map { (compilationResult, target, name) =>
        val allJSFiles = (target / "jsclasses" ** "*.js").get
        val output = target / (name + ".js")
        cat(allJSFiles) #> output ! ;
        output
      }
  )

  lazy val corejslib = Project(
      id = "scalajs-corejslib",
      base = file("corejslib"),
      settings = defaultSettings ++ Seq(
          packageJS in Compile <<= (
              baseDirectory, target in Compile, name
          ) map { (baseDirectory, target, name) =>
            // hard-coded because order matters!
            val fileNames = Seq("scalajsenv.js", "scalajsnatives.js",
                "javalangObject.js", "javalangClass.js", "javalangString.js",
                "javalangArray.js", "RefTypes.js")

            val allJSFiles = fileNames map (baseDirectory / _)
            val output = target / (name + ".js")
            target.mkdir()
            cat(allJSFiles) #> output ! ;
            output
          }
      )
  )

  lazy val javalib = Project(
      id = "scalajs-javalib",
      base = file("javalib"),
      settings = defaultSettings ++ compileJSSettings
  ).dependsOn(compiler)

  lazy val scalalib = Project(
      id = "scalajs-scalalib",
      base = file("scalalib"),
      settings = defaultSettings ++ compileJSSettings
  ).dependsOn(compiler)

  lazy val libraryAux = Project(
      id = "scalajs-library-aux",
      base = file("library-aux"),
      settings = defaultSettings ++ compileJSSettings
  ).dependsOn(compiler)

  lazy val library = Project(
      id = "scalajs-library",
      base = file("library"),
      settings = defaultSettings ++ compileJSSettings
  ).dependsOn(compiler)
}
