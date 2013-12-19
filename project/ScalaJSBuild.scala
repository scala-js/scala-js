import sbt._
import Keys._

import scala.util.Properties

import scala.scalajs.sbtplugin._
import ScalaJSPlugin._
import ScalaJSKeys._
import SourceMapCat.catJSFilesAndTheirSourceMaps

object ScalaJSBuild extends Build {

  val commonSettings = Defaults.defaultSettings ++ Seq(
      organization := "org.scala-lang.modules.scalajs",
      version := scalaJSVersion,

      normalizedName ~= {
        _.replace("scala.js", "scalajs").replace("scala-js", "scalajs")
      },

      publishTo := {
        val resolver = Resolver.sftp(
            s"scala-js-$snapshotsOrReleases",
            "repo.scala-js.org",
            s"/home/scalajsrepo/www/repo/$snapshotsOrReleases")(Resolver.ivyStylePatterns)
        Seq("PUBLISH_USER", "PUBLISH_PASS").map(Properties.envOrNone) match {
          case Seq(Some(user), Some(pass)) =>
            Some(resolver as (user, pass))
          case _ =>
            None
        }
      },

      publishMavenStyle := false
  )

  private val snapshotsOrReleases =
    if (scalaJSIsSnapshotVersion) "snapshots" else "releases"

  val defaultSettings = commonSettings ++ Seq(
      scalaVersion := scalaJSScalaVersion,
      scalacOptions ++= Seq(
          "-deprecation",
          "-unchecked",
          "-feature",
          "-encoding", "utf8"
      )
  )

  val myScalaJSSettings = scalaJSAbstractSettings ++ Seq(
      autoCompilerPlugins := true
  )

  // Used when compiling the compiler, adding it to scalacOptions does not help
  scala.util.Properties.setProp("scalac.patmat.analysisBudget", "1024")

  lazy val root: Project = Project(
      id = "scalajs",
      base = file("."),
      settings = defaultSettings ++ Seq(
          name := "Scala.js",
          publishArtifact in Compile := false,

          clean := clean.dependsOn(
              // compiler, library and sbt-plugin are aggregated
              clean in corejslib, clean in javalib, clean in scalalib,
              clean in libraryAux, clean in test, clean in examples,
              clean in exampleHelloWorld, clean in exampleReversi).value,

          publish := {},
          publishLocal := {}
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
              "org.scala-lang" % "scala-compiler" % scalaJSScalaVersion,
              "org.scala-lang" % "scala-reflect" % scalaJSScalaVersion
          ),
          exportJars := true
      )
  )

  lazy val plugin: Project = Project(
      id = "scalajs-sbt-plugin",
      base = file("sbt-plugin"),
      settings = commonSettings ++ Seq(
          name := "Scala.js sbt plugin",
          sbtPlugin := true,
          scalaBinaryVersion :=
            CrossVersion.binaryScalaVersion(scalaVersion.value),
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

          packageJS in Compile := {
            val s = streams.value
            val targetDir = (target in Compile).value

            // hard-coded because order matters!
            val fileNames =
              Seq("scalajsenv.js", "javalangObject.js",
                  "javalangString.js", "DummyParents.js")

            val allJSFiles = fileNames map (baseDirectory.value / _)
            val output = targetDir / ("scalajs-corejslib.js")

            FileFunction.cached(s.cacheDirectory / "package-js",
                FilesInfo.lastModified, FilesInfo.exists) { dependencies =>
              targetDir.mkdir()
              catJSFilesAndTheirSourceMaps(allJSFiles, output)
              Set(output)
            } (allJSFiles.toSet)

            Seq(output)
          }
      )
  )

  lazy val javalib: Project = Project(
      id = "scalajs-javalib",
      base = file("javalib"),
      settings = defaultSettings ++ myScalaJSSettings ++ Seq(
          name := "Java library for Scala.js",
          publishArtifact in Compile := false,
          scalacOptions += "-Ydelambdafy:method",
          scalacOptions += "-Yskip:cleanup,icode,jvm"
      ) ++ (
          scalaJSExternalCompileSettings
      )
  ).dependsOn(compiler % "plugin", library)

  lazy val scalalib: Project = Project(
      id = "scalajs-scalalib",
      base = file("scalalib"),
      settings = defaultSettings ++ myScalaJSSettings ++ Seq(
          name := "Scala library for Scala.js",
          publishArtifact in Compile := false,
          scalacOptions += "-Ydelambdafy:method",

          // The Scala lib is full of warnings we don't want to see
          scalacOptions ~= (_.filterNot(
              Set("-deprecation", "-unchecked", "-feature") contains _)),

          // Do not generate .class files
          scalacOptions += "-Yskip:cleanup,icode,jvm",

          // Exclude files that are overridden in library
          excludeFilter in (Compile, unmanagedSources) ~= { superFilter =>
            superFilter || new SimpleFileFilter({ f =>
              val path = f.getPath.replace(java.io.File.separator, "/")
              (path.endsWith("/scala/package.scala")
                  || path.endsWith("/scala/App.scala")
                  || path.endsWith("/scala/Console.scala")
                  || path.endsWith("/scala/compat/Platform.scala")
                  || path.endsWith("/scala/runtime/BoxesRunTime.scala")

                  // Hideous but effective way not to compile useless parts
                  || path.contains("/scala/collection/parallel/")
                  || path.contains("/scala/util/parsing/"))
            })
          },

          // Continuation plugin
          autoCompilerPlugins := true,
          libraryDependencies += compilerPlugin(
              "org.scala-lang.plugins" % "continuations" % scalaVersion.value),
          scalacOptions += "-P:continuations:enable"
      ) ++ (
          scalaJSExternalCompileSettings
      )
  ).dependsOn(compiler % "plugin", library)

  lazy val libraryAux: Project = Project(
      id = "scalajs-library-aux",
      base = file("library-aux"),
      settings = defaultSettings ++ myScalaJSSettings ++ Seq(
          name := "Scala.js aux library",
          publishArtifact in Compile := false,
          scalacOptions += "-Ydelambdafy:method",
          scalacOptions += "-Yskip:cleanup,icode,jvm"
      ) ++ (
          scalaJSExternalCompileSettings
      )
  ).dependsOn(compiler % "plugin", library)

  lazy val library: Project = Project(
      id = "scalajs-library",
      base = file("library"),
      settings = defaultSettings ++ myScalaJSSettings ++ Seq(
          name := "Scala.js library",
          scalacOptions += "-Ydelambdafy:method"
      ) ++ (
          scalaJSExternalCompileSettings
      ) ++ inConfig(Compile)(Seq(
          /* Add the .js and .js.map files from other lib projects
           * (but not .jstype files)
           */
          mappings in packageBin ++= {
            val allProducts = (
                (products in javalib).value ++
                (products in scalalib).value ++
                (products in libraryAux).value)
            val filter = ("*.js": NameFilter) | "*.js.map"
            allProducts.flatMap(dir => (dir ** filter) x relativeTo(dir))
          },

          // Add the core JS library
          mappings in packageBin +=
            (packageJS in corejslib).value.head -> "scalajs-corejslib.js"
      ))
  ).dependsOn(compiler % "plugin")

  // Utils

  /* Dirty trick to add our Scala.js library on the classpath without adding a
   * dependency between projects. This avoids to recompile the library every
   * time we make a change in the compiler, and we want to test it on an
   * example or with the test suite.
   */
  def useLibraryButDoNotDependOnIt(config: Configuration) = (
      unmanagedClasspath in config += {
        val libraryJar = (artifactPath in (library, Compile, packageBin)).value
        Attributed.blank(libraryJar)
      })

  // Examples

  lazy val examples: Project = Project(
      id = "examples",
      base = file("examples"),
      settings = defaultSettings ++ Seq(
          name := "Scala.js examples"
      )
  ).aggregate(exampleHelloWorld, exampleReversi)

  lazy val exampleSettings = defaultSettings ++ myScalaJSSettings ++ Seq(
      useLibraryButDoNotDependOnIt(Compile),

      // Add the startup.js file of this example project
      unmanagedSources in (Compile, packageJS) +=
        baseDirectory.value / "startup.js"
  )

  lazy val exampleHelloWorld = Project(
      id = "helloworld",
      base = file("examples") / "helloworld",
      settings = exampleSettings ++ Seq(
          name := "Hello World - Scala.js example",
          moduleName := "helloworld"
      )
  ).dependsOn(compiler % "plugin")

  lazy val exampleReversi = Project(
      id = "reversi",
      base = file("examples") / "reversi",
      settings = exampleSettings ++ Seq(
          name := "Reversi - Scala.js example",
          moduleName := "reversi"
      )
  ).dependsOn(compiler % "plugin")

  // Testing

  val jasmineVersion = "1.3.1"

  lazy val test: Project = Project(
      id = "scalajs-test",
      base = file("test"),
      settings = defaultSettings ++ myScalaJSSettings ++ Seq(
          name := "Scala.js test suite",
          publishArtifact in Compile := false,
          useLibraryButDoNotDependOnIt(Test),

          libraryDependencies += "org.webjars" % "jasmine" % jasmineVersion % "test",

          // We don't need the HTML reporter, since we use the Rhino reporter
          sources in (Test, packageExternalDepsJS) ~= { srcs =>
            srcs.filterNot(_.name == "jasmine-html.js")
          },

          // And a hack to make sure bootstrap.js is included before jasmine.js
          sources in (Test, Keys.test) ~= { srcs =>
            val bootstrap: File = srcs.find(_.name == "bootstrap.js").get
            val (before, after) =
              srcs.filterNot(_ == bootstrap).span(_.name != "jasmine.js")
            before ++ Seq(bootstrap) ++ after
          }
      )
  ).dependsOn(compiler % "plugin")

  lazy val partest: Project = Project(
      id = "scalajs-partest",
      base = file("partest"),
      settings = defaultSettings ++ Seq(
          name := "Partest for Scala.js",
          moduleName := "scalajs-partest",

          resolvers += Resolver.typesafeIvyRepo("releases"),

          libraryDependencies ++= Seq(
              "org.scala-sbt" % "sbt" % "0.13.0",
              "org.scala-lang.modules" %% "scala-partest" % "1.0.0-RC8",
              "org.mozilla" % "rhino" % "1.7R4"
          ),

          sources in Compile += (
              (baseDirectory in plugin).value /
              "src/main/scala/scala/scalajs/sbtplugin/RhinoBasedRun.scala")
      )
  ).dependsOn(compiler)

  lazy val partestSuite: Project = Project(
      id = "scalajs-partest-suite",
      base = file("partest-suite"),
      settings = defaultSettings ++ Seq(
          name := "Scala.js partest suite",

          /* Add an extracted version of scalajs-library.jar on the classpath.
           * The runner will need it, as it cannot cope with .js files in .jar.
           */
          dependencyClasspath in Test += {
            val s = streams.value

            val taskCacheDir = s.cacheDirectory / "extract-scalajs-library"
            val extractDir = taskCacheDir / "scalajs-library"

            val libraryJar =
              (artifactPath in (library, Compile, packageBin)).value

            val cachedExtractJar = FileFunction.cached(taskCacheDir / "cache-info",
                FilesInfo.lastModified, FilesInfo.exists) { dependencies =>

              val usefulFilesFilter = ("*.js": NameFilter) | ("*.js.map")
              s.log.info("Extracting %s ..." format libraryJar)
              if (extractDir.exists)
                IO.delete(extractDir)
              IO.createDirectory(extractDir)
              IO.unzip(libraryJar, extractDir, filter = usefulFilesFilter,
                  preserveLastModified = true)
              (extractDir ** usefulFilesFilter).get.toSet
            }

            cachedExtractJar(Set(libraryJar))

            Attributed.blank(extractDir)
          },

          fork in Test := true,
          javaOptions in Test += "-Xmx1G",
          //Uncomment what you need here
          //javaOptions in Test += "-Dscala.tools.partest.scalajs.testunknownonly=true",
          //javaOptions in Test += "-Dscala.tools.partest.scalajs.useblacklist=true",

          testFrameworks +=
            new TestFramework("scala.tools.partest.scalajs.Framework"),

          definedTests in Test +=
            new sbt.TestDefinition(
                "partest",
                // marker fingerprint since there are no test classes
                // to be discovered by sbt:
                new sbt.testing.AnnotatedFingerprint {
                  def isModule = true
                  def annotationName = "partest"
                },
                true,
                Array()
            )
      )
  ).dependsOn(partest % "test")
}
