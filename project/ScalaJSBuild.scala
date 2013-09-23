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

      normalizedName ~= { _.replace("scala.js", "scalajs").replace("scala-js", "scalajs") }
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

  // Used when compiling the compiler, adding it to scalacOptions does not help
  scala.util.Properties.setProp("scalac.patmat.analysisBudget", "1024")

  lazy val root: Project = Project(
      id = "scalajs",
      base = file("."),
      settings = defaultSettings ++ Seq(
          name := "Scala.js",
          publishArtifact in Compile := false,

          clean <<= clean.dependsOn(
              // compiler, library and sbt-plugin are aggregated
              clean in corejslib, clean in javalib, clean in scalalib,
              clean in libraryAux, clean in examples,
              clean in exampleHelloWorld, clean in exampleReversi)
      )
  ).aggregate(
      compiler, plugin, library
  )

  lazy val compiler: Project = Project(
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

  lazy val plugin: Project = Project(
      id = "scalajs-sbt-plugin",
      base = file("sbt-plugin"),
      settings = commonSettings ++ Seq(
          name := "Scala.js sbt plugin",
          sbtPlugin := true,
          scalaBinaryVersion <<= scalaVersion(CrossVersion.binaryScalaVersion),
          libraryDependencies ++= Seq(
              "com.google.javascript" % "closure-compiler" % "v20130603",
              "org.mozilla" % "rhino" % "1.7R4"
          )
      )
  )

  lazy val corejslib: Project = Project(
      id = "scalajs-corejslib",
      base = file("corejslib"),
      settings = defaultSettings ++ Seq(
          name := "Scala.js core JS runtime",
          publishArtifact in Compile := false,

          packageJS in Compile <<= (
              streams, baseDirectory, target in Compile
          ) map { (s, baseDirectory, target) =>
            // hard-coded because order matters!
            val fileNames =
              Seq("scalajsenv.js", "javalangObject.js",
                  "javalangString.js", "DummyParents.js")

            val allJSFiles = fileNames map (baseDirectory / _)
            val output = target / ("scalajs-corejslib.js")

            FileFunction.cached(s.cacheDirectory / "package-js",
                FilesInfo.lastModified, FilesInfo.exists) { dependencies =>
              target.mkdir()
              catJSFilesAndTheirSourceMaps(allJSFiles, output)
              Set(output)
            } (allJSFiles.toSet)

            output
          }
      )
  )

  lazy val javalib: Project = Project(
      id = "scalajs-javalib",
      base = file("javalib"),
      settings = defaultSettings ++ scalaJSAbstractSettings ++ Seq(
          name := "Java library for Scala.js",
          publishArtifact in Compile := false
      )
  ).dependsOn(compiler, library)

  lazy val scalalib: Project = Project(
      id = "scalajs-scalalib",
      base = file("scalalib"),
      settings = defaultSettings ++ scalaJSAbstractSettings ++ Seq(
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
                  || path.endsWith("/scala/App.scala")
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

  lazy val libraryAux: Project = Project(
      id = "scalajs-library-aux",
      base = file("library-aux"),
      settings = defaultSettings ++ scalaJSAbstractSettings ++ Seq(
          name := "Scala.js aux library",
          publishArtifact in Compile := false
      )
  ).dependsOn(compiler)

  lazy val library: Project = Project(
      id = "scalajs-library",
      base = file("library"),
      settings = defaultSettings ++ scalaJSAbstractSettings ++ Seq(
          name := "Scala.js library",

          mappings in (Compile, packageBin) <++= (
              compile in (javalib, Compile), classDirectory in (javalib, Compile),
              compile in (scalalib, Compile), classDirectory in (scalalib, Compile),
              compile in (libraryAux, Compile), classDirectory in (libraryAux, Compile)
          ) map { (_, javalibCD, _, scalalibCD, _, libraryAuxCD) =>
            val filter = ("*.js": NameFilter) | "*.js.map"
            val javalibMappings = (javalibCD ** filter) x relativeTo(javalibCD)
            val scalalibMappings = (scalalibCD ** filter) x relativeTo(scalalibCD)
            val libraryAuxMappings = (libraryAuxCD ** filter) x relativeTo(libraryAuxCD)
            javalibMappings ++ scalalibMappings ++ libraryAuxMappings
          },

          mappings in (Compile, packageBin) <+= (
              packageJS in (corejslib, Compile)
          ) map { (corejslibFile) =>
            corejslibFile -> "scalajs-corejslib.js"
          }
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

  lazy val exampleSettings = defaultSettings ++ scalaJSAbstractSettings ++ Seq(
      /* Add the library classpath this way to escape the dependency between
       * tasks. This avoids to recompile the library every time we compile an
       * example. This is all about working around the lack of dependency
       * analysis.
       */
      unmanagedClasspath in Compile <+= (
          artifactPath in (library, Compile, packageBin)
      ) map { libraryJar =>
        Attributed.blank(libraryJar)
      },

      // Add the startup.js file of this example project
      unmanagedSources in (Compile, packageJS) <+= (
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
