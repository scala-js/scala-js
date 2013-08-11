import sbt._
import Keys._
import Process.cat

import ch.epfl.lamp.sbtscalajs._
import ScalaJSPlugin._
import ScalaJSKeys._
import SourceMapCat.catJSFilesAndTheirSourceMaps

object ScalaJSBuild extends Build {

  val scalajsScalaVersion = "2.10.1"

  val commonSettings = Defaults.defaultSettings ++ Seq(
      organization := "ch.epfl.lamp",
      version := "0.1-SNAPSHOT",

      normalizedName ~= { _.replace("scala.js", "scalajs") }
  )

  val defaultSettings = commonSettings ++ Seq(
      scalaVersion := scalajsScalaVersion,
      scalacOptions ++= Seq(
          "-deprecation",
          "-unchecked",
          "-feature",
          "-encoding", "utf8"
      )
  )

  lazy val root: Project = Project(
      id = "scalajs",
      base = file("."),
      settings = defaultSettings ++ Seq(
          name := "Scala.js",
          publishArtifact in Compile := false,
          packageJS in Compile <<= (
              target,
              packageJS in (corejslib, Compile),
              compile in (javalib, Compile),
              compile in (scalalib, Compile),
              compile in (libraryAux, Compile),
              compile in (library, Compile),
              classDirectory in (javalib, Compile),
              classDirectory in (scalalib, Compile),
              classDirectory in (libraryAux, Compile),
              classDirectory in (library, Compile)
          ) map { (target, corejslib, i1, i2, i3, i4, d1, d2, d3, d4) =>
            val allJSFiles =
              (d1**"*.js" +++ d2**"*.js" +++ d3**"*.js" +++ d4**"*.js").get
            val sortedJSFiles = sortScalaJSOutputFiles(allJSFiles)
            val output = target / ("scalajs-runtime.js")
            target.mkdir()
            catJSFilesAndTheirSourceMaps(corejslib +: sortedJSFiles, output)
            output
          },

          clean <<= clean.dependsOn(
              // compiler, library and sbt-plugin are aggregated
              clean in corejslib, clean in javalib, clean in scalalib,
              clean in libraryAux, clean in examples,
              clean in exampleHelloWorld, clean in exampleReversi)
      )
  ).aggregate(
      compiler, plugin, library
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
          mainClass := Some("scala.tools.nsc.scalajs.Main"),
          exportJars := true
      )
  )

  lazy val plugin = Project(
      id = "scalajs-sbt-plugin",
      base = file("sbt-plugin"),
      settings = commonSettings ++ Seq(
          name := "Scala.js sbt plugin",
          sbtPlugin := true,
          scalaVersion := "2.9.2",
          scalaBinaryVersion <<= scalaVersion,
          libraryDependencies ++= Seq(
              "com.google.javascript" % "closure-compiler" % "v20130603"
          )
      )
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
              Seq("scalajsenv.js", "javalangObject.js",
                  "javalangString.js", "DummyParents.js")

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
      settings = defaultSettings ++ baseScalaJSSettings ++ Seq(
          name := "Java library for Scala.js",
          publishArtifact in Compile := false
      )
  ).dependsOn(compiler, library)

  lazy val scalalib = Project(
      id = "scalajs-scalalib",
      base = file("scalalib"),
      settings = defaultSettings ++ baseScalaJSSettings ++ Seq(
          name := "Scala library for Scala.js",
          publishArtifact in Compile := false,

          // The Scala lib is full of warnings we don't want to see
          scalacOptions ~= (_.filterNot(
              Set("-deprecation", "-unchecked", "-feature") contains _)),

          // Exclude files that are overridden in library
          excludeFilter in (Compile, unmanagedSources) ~= { superFilter =>
            superFilter || new SimpleFileFilter({ f =>
              val path = f.getPath.replace(java.io.File.separator, "/")
              (path.endsWith("/scala/package.scala")
                  || path.endsWith("/scala/Predef.scala")
                  || path.endsWith("/scala/Console.scala")
                  || path.endsWith("/scala/compat/Platform.scala")
                  || path.endsWith("/scala/runtime/BoxesRunTime.scala"))
            })
          },

          // Continuation plugin
          autoCompilerPlugins := true,
          libraryDependencies <<= (scalaVersion, libraryDependencies) { (ver, deps) =>
            deps :+ compilerPlugin("org.scala-lang.plugins" % "continuations" % ver)
          },
          scalacOptions += "-P:continuations:enable"
      )
  ).dependsOn(compiler)

  lazy val libraryAux = Project(
      id = "scalajs-library-aux",
      base = file("library-aux"),
      settings = defaultSettings ++ baseScalaJSSettings ++ Seq(
          name := "Scala.js aux library",
          publishArtifact in Compile := false
      )
  ).dependsOn(compiler)

  lazy val library = Project(
      id = "scalajs-library",
      base = file("library"),
      settings = defaultSettings ++ baseScalaJSSettings ++ Seq(
          name := "Scala.js library"
      )
  ).dependsOn(compiler)

  // Examples

  lazy val examples: Project = Project(
      id = "examples",
      base = file("examples"),
      settings = defaultSettings ++ Seq(
          name := "Scala.js examples"
      )
  ).aggregate(exampleHelloWorld, exampleReversi)

  lazy val exampleSettings = defaultSettings ++ baseScalaJSSettings ++ Seq(
      /* Add the library classpath this way to escape the dependency between
       * tasks. This avoids to recompile the library every time we compile an
       * example. This is all about working around the lack of dependency
       * analysis.
       */
      unmanagedClasspath in Compile <+= (
          classDirectory in (library, Compile)
      ) map { classDir =>
        Attributed.blank(classDir)
      },

      // Add the Scala.js runtime - same reason not to use root/package-js
      unmanagedSources in (Compile, optimizeJS) <+= (
          target in root
      ) map { rootTarget =>
        rootTarget / "scalajs-runtime.js"
      },

      // Add the startup.js file of this example project
      unmanagedSources in (Compile, optimizeJS) <+= (
          baseDirectory
      ) map { base =>
        base / "startup.js"
      }
  )

  lazy val exampleHelloWorld = Project(
      id = "helloworld",
      base = file("examples") / "helloworld",
      settings = exampleSettings ++ Seq(
          name := "Hello World - Scala.js example",
          moduleName := "helloworld"
      )
  ).dependsOn(compiler)

  lazy val exampleReversi = Project(
      id = "reversi",
      base = file("examples") / "reversi",
      settings = exampleSettings ++ Seq(
          name := "Reversi - Scala.js example",
          moduleName := "reversi"
      )
  ).dependsOn(compiler)
}
