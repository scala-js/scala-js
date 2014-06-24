import sbt._
import Keys._

import PublishToBintray.publishToBintraySettings

import java.io.{BufferedOutputStream, FileOutputStream}

import scala.collection.mutable
import scala.util.Properties

import scala.scalajs.ir
import scala.scalajs.sbtplugin._
import scala.scalajs.sbtplugin.env.rhino.RhinoJSEnv
import ScalaJSPlugin._
import ScalaJSKeys._
import ExternalCompile.scalaJSExternalCompileSettings

import scala.scalajs.tools.sourcemap._

import sbtassembly.Plugin.{AssemblyKeys, assemblySettings}
import AssemblyKeys.{assembly, assemblyOption}

object ScalaJSBuild extends Build {

  val fetchedScalaSourceDir = settingKey[File](
    "Directory the scala source is fetched to")
  val fetchScalaSource = taskKey[File](
    "Fetches the scala source for the current scala version")
  val shouldPartest = settingKey[Boolean](
    "Whether we should partest the current scala version (and fail if we can't)")

  val commonSettings = Defaults.defaultSettings ++ Seq(
      organization := "org.scala-lang.modules.scalajs",
      version := scalaJSVersion,

      normalizedName ~= {
        _.replace("scala.js", "scalajs").replace("scala-js", "scalajs")
      },

      homepage := Some(url("http://scala-js.org/")),
      licenses += ("BSD New",
          url("https://github.com/scala-js/scala-js/blob/master/LICENSE")),

      shouldPartest := {
        val testListDir = (
          (resourceDirectory in (partestSuite, Test)).value / "scala"
            / "tools" / "partest" / "scalajs" / scalaVersion.value
        )
        testListDir.exists
      }
  )

  private val snapshotsOrReleases =
    if (scalaJSIsSnapshotVersion) "snapshots" else "releases"

  private def publishToScalaJSRepoSettings = Seq(
      publishTo := {
        Seq("PUBLISH_USER", "PUBLISH_PASS").map(Properties.envOrNone) match {
          case Seq(Some(user), Some(pass)) =>
            Some(Resolver.sftp(
                s"scala-js-$snapshotsOrReleases",
                "repo.scala-js.org",
                s"/home/scalajsrepo/www/repo/$snapshotsOrReleases")(
                Resolver.ivyStylePatterns) as (user, pass))
          case _ =>
            None
        }
      }
  )

  val publishSettings = (
      if (Properties.envOrNone("PUBLISH_TO_BINTRAY") == Some("true"))
        publishToBintraySettings
      else
        publishToScalaJSRepoSettings
  ) ++ Seq(
      publishMavenStyle := false
  )

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
      autoCompilerPlugins := true,
      checkScalaJSIR := true
  )

  val scalaJSSourceMapSettings = scalacOptions ++= {
    if (scalaJSIsSnapshotVersion) Seq()
    else Seq(
      // Link source maps to github sources
      "-P:scalajs:relSourceMap:" + root.base.toURI,
      "-P:scalajs:absSourceMap:" +
      "https://raw.githubusercontent.com/scala-js/scala-js/v" +
      scalaJSVersion + "/"
    )
  }

  // Used when compiling the compiler, adding it to scalacOptions does not help
  scala.util.Properties.setProp("scalac.patmat.analysisBudget", "1024")

  override lazy val settings = super.settings :+ {
    // Most of the projects cross-compile
    crossScalaVersions := Seq(
      "2.10.2",
      "2.10.3",
      "2.10.4",
      "2.11.0",
      "2.11.1"
    )
  }

  lazy val root: Project = Project(
      id = "scalajs",
      base = file("."),
      settings = defaultSettings ++ Seq(
          name := "Scala.js",
          publishArtifact in Compile := false,

          clean := clean.dependsOn(
              // compiler, library and jasmineTestFramework are aggregated
              clean in tools, clean in plugin,
              clean in javalib, clean in scalalib,
              clean in libraryAux,
              clean in examples, clean in exampleHelloWorld,
              clean in exampleReversi, clean in exampleTesting,
              clean in test, clean in partest, clean in partestSuite).value,

          publish := {},
          publishLocal := {}
      )
  ).aggregate(
      compiler, library, testBridge, jasmineTestFramework
  )

  /* This project is not really used in the build. Its sources are actually
   * added as sources of the `compiler` project (and meant to be added to the
   * `tools` project as well).
   * It exists mostly to be used in IDEs, because they don't like very much to
   * have code in a project that lives outside the project's directory, and
   * like even less that code be shared by different projects.
   */
  lazy val irProject: Project = Project(
      id = "scalajs-ir",
      base = file("ir"),
      settings = defaultSettings ++ Seq(
          name := "Scala.js IR",
          exportJars := true
      )
  )

  lazy val compiler: Project = Project(
      id = "scalajs-compiler",
      base = file("compiler"),
      settings = defaultSettings ++ publishSettings ++ Seq(
          name := "Scala.js compiler",
          crossVersion := CrossVersion.full, // because compiler api is not binary compatible
          unmanagedSourceDirectories in Compile +=
            (scalaSource in (irProject, Compile)).value,
          libraryDependencies ++= Seq(
              "org.scala-lang" % "scala-compiler" % scalaVersion.value,
              "org.scala-lang" % "scala-reflect" % scalaVersion.value,
              "com.novocode" % "junit-interface" % "0.9" % "test"
          ),
          testOptions += Tests.Setup { () =>
            val testOutDir = (streams.value.cacheDirectory / "scalajs-compiler-test")
            IO.createDirectory(testOutDir)
            sys.props("scala.scalajs.compiler.test.output") =
              testOutDir.getAbsolutePath
            sys.props("scala.scalajs.compiler.test.scalajslib") =
              (artifactPath in (library, Compile, packageBin)).value.getAbsolutePath
            sys.props("scala.scalajs.compiler.test.scalalib") = {

              def isScalaLib(att: Attributed[File]) = {
                att.metadata.get(moduleID.key).exists { mId =>
                  mId.organization == "org.scala-lang" &&
                  mId.name         == "scala-library"  &&
                  mId.revision     == scalaVersion.value
                }
              }

              val lib = (managedClasspath in Test).value.find(isScalaLib)
              lib.map(_.data.getAbsolutePath).getOrElse {
                streams.value.log.error("Couldn't find Scala library on the classpath. CP: " + (managedClasspath in Test).value); ""
              }
            }
          },
          exportJars := true
      )
  )

  lazy val tools: Project = Project(
      id = "scalajs-tools",
      base = file("tools"),
      settings = defaultSettings ++ publishSettings ++ Seq(
          name := "Scala.js tools",
          scalaVersion := "2.10.4",
          unmanagedSourceDirectories in Compile +=
            (scalaSource in (irProject, Compile)).value,
          libraryDependencies ++= Seq(
              "com.google.javascript" % "closure-compiler" % "v20130603",
              "com.googlecode.json-simple" % "json-simple" % "1.1.1",
              "com.novocode" % "junit-interface" % "0.9" % "test"
          )
      )
  )

  lazy val plugin: Project = Project(
      id = "scalajs-sbt-plugin",
      base = file("sbt-plugin"),
      settings = commonSettings ++ publishSettings ++ Seq(
          name := "Scala.js sbt plugin",
          sbtPlugin := true,
          scalaBinaryVersion :=
            CrossVersion.binaryScalaVersion(scalaVersion.value),
          libraryDependencies ++= Seq(
              "org.mozilla" % "rhino" % "1.7R4",
              "org.webjars" % "envjs" % "1.2",
              "com.novocode" % "junit-interface" % "0.9" % "test"
          )
      )
  ).dependsOn(tools)

  lazy val delambdafySetting = {
    scalacOptions ++= (
        if (scalaBinaryVersion.value == "2.10") Seq()
        else Seq("-Ydelambdafy:method"))
  }

  private def serializeHardcodedIR(base: File,
      infoAndTree: (ir.Infos.ClassInfo, ir.Trees.ClassDef)): File = {
    // We assume that there are no weird characters in the full name
    val fullName = infoAndTree._1.name
    val output = base / (fullName.replace('.', '/') + ".sjsir")
    IO.createDirectory(output.getParentFile)
    val stream = new BufferedOutputStream(new FileOutputStream(output))
    try {
      ir.InfoSerializers.serialize(stream, infoAndTree._1)
      ir.Serializers.serialize(stream, infoAndTree._2)
    } finally {
      stream.close()
    }
    output
  }

  lazy val javalib: Project = Project(
      id = "scalajs-javalib",
      base = file("javalib"),
      settings = defaultSettings ++ myScalaJSSettings ++ Seq(
          name := "Java library for Scala.js",
          publishArtifact in Compile := false,
          delambdafySetting,
          scalacOptions += "-Yskip:cleanup,icode,jvm",
          scalaJSSourceMapSettings,

          resourceGenerators in Compile <+= Def.task {
            val base = (resourceManaged in Compile).value
            Seq(
                serializeHardcodedIR(base, JavaLangObject.InfoAndTree),
                serializeHardcodedIR(base, JavaLangString.InfoAndTree)
            )
          }
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
          delambdafySetting,

          // The Scala lib is full of warnings we don't want to see
          scalacOptions ~= (_.filterNot(
              Set("-deprecation", "-unchecked", "-feature") contains _)),


          scalacOptions ++= List(
            // Do not generate .class files
            "-Yskip:cleanup,icode,jvm",
            // Tell plugin to hack fix bad classOf trees
            "-P:scalajs:fixClassOf",
            // Link source maps to github sources
            "-P:scalajs:relSourceMap:" +
            fetchedScalaSourceDir.value.toURI,
            "-P:scalajs:absSourceMap:" +
            "https://raw.githubusercontent.com/scala/scala/v" +
            scalaVersion.value + "/"
            ),

          fetchedScalaSourceDir := (
            baseDirectory.value / "fetchedSources" /
            scalaVersion.value
          ),

          fetchScalaSource := {
            import org.eclipse.jgit.api._

            val s = streams.value
            val ver = scalaVersion.value
            val trgDir = fetchedScalaSourceDir.value

            if (!trgDir.exists) {
              s.log.info(s"Fetching Scala source version $ver")

              // Make parent dirs and stuff
              IO.createDirectory(trgDir)

              // Clone scala source code
              new CloneCommand()
                .setDirectory(trgDir)
                .setURI("https://github.com/scala/scala.git")
                .call()

            }

            // Checkout proper ref. We do this anyway so we fail if
            // something is wrong
            val git = Git.open(trgDir)
            s.log.info(s"Checking out Scala source version $ver")
            git.checkout().setName(s"v$ver").call()

            trgDir
          },

          unmanagedSourceDirectories in Compile := {
            // Calculates all prefixes of the current Scala version
            // (including the empty prefix) to construct override
            // directories like the following:
            // - override-2.10.2-RC1
            // - override-2.10.2
            // - override-2.10
            // - override-2
            // - override
            val ver = scalaVersion.value
            val base = baseDirectory.value
            val parts = ver.split(Array('.','-'))
            val verList = parts.inits.map { ps =>
              val len = ps.mkString(".").length
              // re-read version, since we lost '.' and '-'
              ver.substring(0, len)
            }
            def dirStr(v: String) =
              if (v.isEmpty) "overrides" else s"overrides-$v"
            val dirs = verList.map(base / dirStr(_)).filter(_.exists)
            dirs.toSeq // most specific shadow less specific
          },

          // Compute sources
          // Files in earlier src dirs shadow files in later dirs
          sources in Compile := {
            // Sources coming from the sources of Scala
            val scalaSrcDir = fetchScalaSource.value / "src"
            val scalaSrcDirs = (scalaSrcDir / "library") :: (
                if (!scalaVersion.value.startsWith("2.10.")) Nil
                else (scalaSrcDir / "continuations" / "library") :: Nil)

            // All source directories (overrides shadow scalaSrcDirs)
            val sourceDirectories =
              (unmanagedSourceDirectories in Compile).value ++ scalaSrcDirs

            // Filter sources with overrides
            def normPath(f: File): String =
              f.getPath.replace(java.io.File.separator, "/")

            val sources = mutable.ListBuffer.empty[File]
            val paths = mutable.Set.empty[String]

            for {
              srcDir <- sourceDirectories
              normSrcDir = normPath(srcDir)
              src <- (srcDir ** "*.scala").get
            } {
              val normSrc = normPath(src)
              val path = normSrc.substring(normSrcDir.length)
              val useless =
                path.contains("/scala/collection/parallel/") ||
                path.contains("/scala/util/parsing/")
              if (!useless) {
                if (paths.add(path))
                  sources += src
                else
                  streams.value.log.debug(s"not including $src")
              }
            }

            sources.result()
          },

          // Continuation plugin (when using 2.10.x)
          autoCompilerPlugins := true,
          libraryDependencies ++= {
            val ver = scalaVersion.value
            if (ver.startsWith("2.10."))
              Seq(compilerPlugin("org.scala-lang.plugins" % "continuations" % ver))
            else
              Nil
          },
          scalacOptions ++= {
            if (scalaVersion.value.startsWith("2.10."))
              Seq("-P:continuations:enable")
            else
              Nil
          }
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
          delambdafySetting,
          scalacOptions += "-Yskip:cleanup,icode,jvm",
          scalaJSSourceMapSettings
      ) ++ (
          scalaJSExternalCompileSettings
      )
  ).dependsOn(compiler % "plugin", library)

  lazy val library: Project = Project(
      id = "scalajs-library",
      base = file("library"),
      settings = defaultSettings ++ publishSettings ++ myScalaJSSettings ++ Seq(
          name := "Scala.js library",
          delambdafySetting,
          scalaJSSourceMapSettings,
          scalacOptions in (Compile, doc) += "-implicits"
      ) ++ (
          scalaJSExternalCompileSettings
      ) ++ inConfig(Compile)(Seq(
          /* Add the .js, .js.map and .sjsinfo files from other lib projects
           * (but not .class files)
           */
          mappings in packageBin ++= {
            val allProducts = (
                (products in javalib).value ++
                (products in scalalib).value ++
                (products in libraryAux).value)
            val filter = ("*.sjsir": NameFilter)
            allProducts.flatMap(dir => (dir ** filter) x relativeTo(dir))
          }
      ))
  ).dependsOn(compiler % "plugin")

  // Scala.js command line interface
  lazy val cli: Project = Project(
      id = "scalajs-cli",
      base = file("cli"),
      settings = defaultSettings ++ assemblySettings ++ Seq(
          name := "Scala.js CLI",
          scalaVersion := "2.10.4", // adapt version to tools
          libraryDependencies ++= Seq(
              "com.github.scopt" %% "scopt" % "3.2.0"
          ),

          // assembly options
          mainClass in assembly := None, // don't want an executable JAR
          assemblyOption in assembly ~= { _.copy(includeScala = false) },
          AssemblyKeys.jarName in assembly :=
            s"${normalizedName.value}-assembly_${scalaBinaryVersion.value}-${version.value}.jar"
      )
  ).dependsOn(tools)

  // Test framework
  lazy val testBridge = Project(
      id = "scalajs-test-bridge",
      base = file("test-bridge"),
      settings = defaultSettings ++ publishSettings ++ myScalaJSSettings ++ Seq(
          name := "Scala.js test bridge",
          delambdafySetting,
          scalaJSSourceMapSettings
      )
  ).dependsOn(compiler % "plugin", library)

  lazy val jasmineTestFramework = Project(
      id = "scalajs-jasmine-test-framework",
      base = file("jasmine-test-framework"),
      settings = defaultSettings ++ publishSettings ++ myScalaJSSettings ++ Seq(
          name := "Scala.js jasmine test framework",

          jsDependencies ++= Seq(
            ProvidedJS / "jasmine-polyfills.js",
            "org.webjars" % "jasmine" % "1.3.1" /
              "jasmine.js" dependsOn "jasmine-polyfills.js"
          ),
          scalaJSSourceMapSettings
      )
  ).dependsOn(compiler % "plugin", library, testBridge)

  // Utils

  /* Dirty trick to add our Scala.js library on the classpath without adding a
   * dependency between projects. This avoids to recompile the library every
   * time we make a change in the compiler, and we want to test it on an
   * example or with the test suite.
   */
  def useProjectButDoNotDependOnIt(project: Project, config: Configuration) = (
      unmanagedClasspath in config += {
        val libraryJar = (artifactPath in (project, Compile, packageBin)).value
        Attributed.blank(libraryJar)
      })

  def useLibraryButDoNotDependOnIt = Seq(
      useProjectButDoNotDependOnIt(library, Compile),
      useProjectButDoNotDependOnIt(library, Runtime),
      useProjectButDoNotDependOnIt(library, Test)
  )

  def useJasmineTestFrameworkButDoNotDependOnIt = Seq(
      useProjectButDoNotDependOnIt(testBridge, Test),
      useProjectButDoNotDependOnIt(jasmineTestFramework, Test),
      libraryDependencies ++=
        (libraryDependencies in jasmineTestFramework).value map (_ % "test")
  )

  // Examples

  lazy val examples: Project = Project(
      id = "examples",
      base = file("examples"),
      settings = defaultSettings ++ Seq(
          name := "Scala.js examples"
      )
  ).aggregate(exampleHelloWorld, exampleReversi)

  lazy val exampleSettings = defaultSettings ++ myScalaJSSettings ++ (
      useLibraryButDoNotDependOnIt ++
      useJasmineTestFrameworkButDoNotDependOnIt
  )

  lazy val exampleHelloWorld = Project(
      id = "helloworld",
      base = file("examples") / "helloworld",
      settings = exampleSettings ++ Seq(
          name := "Hello World - Scala.js example",
          moduleName := "helloworld",
          persistLauncher := true
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

  lazy val exampleTesting = Project(
      id = "testing",
      base = file("examples") / "testing",
      settings = exampleSettings ++ Seq(
          name := "Testing - Scala.js example",
          moduleName := "testing",
          requiresDOM := true,

          jsDependencies ++= Seq(
            "org.webjars" % "jquery" % "1.10.2" / "jquery.js" % "test"
          )
      )
  ).dependsOn(compiler % "plugin")

  // Testing

  lazy val test: Project = Project(
      id = "scalajs-test",
      base = file("test"),
      settings = defaultSettings ++ myScalaJSSettings ++ (
          useLibraryButDoNotDependOnIt ++
          useJasmineTestFrameworkButDoNotDependOnIt
      ) ++ Seq(
          name := "Scala.js test suite",
          publishArtifact in Compile := false,

          scalacOptions ~= (_.filter(_ != "-deprecation")),

          sources in Test ++= {
            if (!scalaVersion.value.startsWith("2.10") &&
                scalacOptions.value.contains("-Xexperimental")) {
              (((sourceDirectory in Test).value / "require-sam") ** "*.scala").get
            } else {
              Nil
            }
          }
      )
  ).dependsOn(compiler % "plugin")

  lazy val noIrCheckTest: Project = Project(
      id = "scalajs-no-ir-check-test",
      base = file("no-ir-check-test"),
      settings = defaultSettings ++ myScalaJSSettings ++ (
          useLibraryButDoNotDependOnIt ++
          useJasmineTestFrameworkButDoNotDependOnIt
      ) ++ Seq(
          name := "Scala.js not IR checked tests",
          checkScalaJSIR := false,
          publishArtifact in Compile := false
     )
  ).dependsOn(compiler % "plugin")

  lazy val partest: Project = Project(
      id = "scalajs-partest",
      base = file("partest"),
      settings = defaultSettings ++ Seq(
          name := "Partest for Scala.js",
          moduleName := "scalajs-partest",

          resolvers += Resolver.typesafeIvyRepo("releases"),

          libraryDependencies ++= {
            if (shouldPartest.value)
              Seq(
                "org.scala-sbt" % "sbt" % "0.13.0",
                "org.scala-lang.modules" %% "scala-partest" % "1.0.0",
                "com.google.javascript" % "closure-compiler" % "v20130603",
                "org.mozilla" % "rhino" % "1.7R4",
                "com.googlecode.json-simple" % "json-simple" % "1.1.1"
              )
            else Seq()
          },

          unmanagedSourceDirectories in Compile ++= {
            val toolsBase = (scalaSource in (tools, Compile)).value /
                "scala/scalajs/tools"
            val pluginBase = ((scalaSource in (plugin, Compile)).value /
                "scala/scalajs/sbtplugin")
            Seq(
              pluginBase / "env",
              pluginBase / "sourcemap",
              toolsBase
            )
          },

          unmanagedResourceDirectories in Compile ++=
            (unmanagedResourceDirectories in (tools, Compile)).value,

          sources in Compile := {
            if (shouldPartest.value) {
              val baseSrcs = (sources in Compile).value
              val d = (scalaSource in (plugin, Compile)).value
              val newSrcs = Seq(
                d / "scala/scalajs/sbtplugin/JSUtils.scala")
              baseSrcs ++ newSrcs
            } else Seq()
          }

      )
  ).dependsOn(compiler)

  lazy val partestSuite: Project = Project(
      id = "scalajs-partest-suite",
      base = file("partest-suite"),
      settings = defaultSettings ++ (
          useLibraryButDoNotDependOnIt
      ) ++ Seq(
          name := "Scala.js partest suite",

          fork in Test := true,
          javaOptions in Test += "-Xmx1G",

          testFrameworks ++= {
            if (shouldPartest.value)
              Seq(new TestFramework("scala.tools.partest.scalajs.Framework"))
            else Seq()
          },

          definedTests in Test ++= {
            if (shouldPartest.value)
              Seq(new sbt.TestDefinition(
                s"partest-${scalaVersion.value}",
                // marker fingerprint since there are no test classes
                // to be discovered by sbt:
                new sbt.testing.AnnotatedFingerprint {
                  def isModule = true
                  def annotationName = "partest"
                },
                true,
                Array()
              ))
            else Seq()
          }
      )
  ).dependsOn(partest % "test")
}
