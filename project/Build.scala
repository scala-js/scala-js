import sbt._
import Keys._

import bintray.Plugin.bintrayPublishSettings
import bintray.Keys.{repository, bintrayOrganization, bintray}

import com.typesafe.tools.mima.plugin.MimaPlugin.mimaDefaultSettings
import com.typesafe.tools.mima.plugin.MimaKeys.{previousArtifact, binaryIssueFilters}

import java.io.{
  BufferedOutputStream,
  FileOutputStream,
  BufferedWriter,
  FileWriter
}

import scala.collection.mutable
import scala.util.Properties

import org.scalajs.core.ir
import org.scalajs.core.ir.Utils.escapeJS

import org.scalajs.sbtplugin._
import org.scalajs.jsenv.rhino.RhinoJSEnv
import org.scalajs.jsenv.nodejs.NodeJSEnv
import org.scalajs.jsenv.phantomjs.PhantomJSEnv
import ScalaJSPlugin.autoImport._
import ExternalCompile.scalaJSExternalCompileSettings
import Implicits._

import org.scalajs.core.tools.sourcemap._
import org.scalajs.core.tools.io.MemVirtualJSFile
import org.scalajs.core.tools.sem.CheckedBehavior

import sbtassembly.Plugin.{AssemblyKeys, assemblySettings}
import AssemblyKeys.{assembly, assemblyOption}

object Build extends sbt.Build {

  val fetchScalaSource = taskKey[File](
    "Fetches the scala source for the current scala version")
  val shouldPartest = settingKey[Boolean](
    "Whether we should partest the current scala version (and fail if we can't)")

  val previousVersion = "0.6.2"
  val previousSJSBinaryVersion =
    ScalaJSCrossVersion.binaryScalaJSVersion(previousVersion)

  val commonSettings = Seq(
      scalaVersion := "2.11.6",
      organization := "org.scala-js",
      version := scalaJSVersion,

      normalizedName ~= {
        _.replace("scala.js", "scalajs").replace("scala-js", "scalajs")
      },

      homepage := Some(url("http://scala-js.org/")),
      licenses += ("BSD New",
          url("https://github.com/scala-js/scala-js/blob/master/LICENSE")),
      scmInfo := Some(ScmInfo(
          url("https://github.com/scala-js/scala-js"),
          "scm:git:git@github.com:scala-js/scala-js.git",
          Some("scm:git:git@github.com:scala-js/scala-js.git"))),

      shouldPartest := {
        val testListDir = (
          (resourceDirectory in (partestSuite, Test)).value / "scala"
            / "tools" / "partest" / "scalajs" / scalaVersion.value
        )
        testListDir.exists
      },

      scalacOptions ++= Seq(
          "-deprecation",
          "-unchecked",
          "-feature",
          "-encoding", "utf8"
      )
  ) ++ mimaDefaultSettings

  val publishSettings = Seq(
      publishMavenStyle := true,
      publishTo := {
        val nexus = "https://oss.sonatype.org/"
        if (isSnapshot.value)
          Some("snapshots" at nexus + "content/repositories/snapshots")
        else
          Some("releases" at nexus + "service/local/staging/deploy/maven2")
      },
      pomExtra := (
          <developers>
            <developer>
              <id>sjrd</id>
              <name>Sébastien Doeraene</name>
              <url>https://github.com/sjrd/</url>
            </developer>
            <developer>
              <id>gzm0</id>
              <name>Tobias Schlatter</name>
              <url>https://github.com/gzm0/</url>
            </developer>
          </developers>
      ),
      pomIncludeRepository := { _ => false }
  )

  private def publishToScalaJSRepoSettings = Seq(
      publishTo := {
        Seq("PUBLISH_USER", "PUBLISH_PASS").map(Properties.envOrNone) match {
          case Seq(Some(user), Some(pass)) =>
            val snapshotsOrReleases =
              if (scalaJSIsSnapshotVersion) "snapshots" else "releases"
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

  private def publishToBintraySettings = (
      bintrayPublishSettings
  ) ++ Seq(
      repository in bintray := "scala-js-releases",
      bintrayOrganization in bintray := Some("scala-js")
  )

  val publishIvySettings = (
      if (Properties.envOrNone("PUBLISH_TO_BINTRAY") == Some("true"))
        publishToBintraySettings
      else
        publishToScalaJSRepoSettings
  ) ++ Seq(
      publishMavenStyle := false
  )

  val myScalaJSSettings = ScalaJSPluginInternal.scalaJSAbstractSettings ++ Seq(
      autoCompilerPlugins := true,
      scalaJSOptimizerOptions ~= (_.withCheckScalaJSIR(true)),
      testFrameworks +=
        TestFramework("org.scalajs.jasminetest.JasmineFramework")
  )

  val scalaJSSourceMapSettings = scalacOptions ++= {
    if (scalaJSIsSnapshotVersion) Seq()
    else Seq(
      // Link source maps to github sources
      "-P:scalajs:mapSourceURI:" + root.base.toURI +
      "->https://raw.githubusercontent.com/scala-js/scala-js/v" +
      scalaJSVersion + "/"
    )
  }

  /** Depend library as if (exportJars in library) was set to false */
  val compileWithLibrarySetting = {
    internalDependencyClasspath in Compile ++= {
      val prods = (products in (library, Compile)).value
      val analysis = (compile in (library, Compile)).value

      prods.map(p => Classpaths.analyzed(p, analysis))
    }
  }

  override lazy val settings = super.settings ++ Seq(
      // Most of the projects cross-compile
      crossScalaVersions := Seq(
        "2.10.2",
        "2.10.3",
        "2.10.4",
        "2.10.5",
        "2.11.0",
        "2.11.1",
        "2.11.2",
        "2.11.4",
        "2.11.5",
        "2.11.6"
      ),
      // Default stage
      scalaJSStage in Global := PreLinkStage
  )

  lazy val root: Project = Project(
      id = "scalajs",
      base = file("."),
      settings = commonSettings ++ Seq(
          name := "Scala.js",
          publishArtifact in Compile := false,

          clean := clean.dependsOn(
              clean in compiler,
              clean in irProject, clean in irProjectJS,
              clean in tools, clean in toolsJS, clean in jsEnvs,
              clean in testAdapter, clean in plugin,
              clean in javalanglib, clean in javalib, clean in scalalib,
              clean in libraryAux, clean in library, clean in javalibEx,
              clean in stubs, clean in cli,
              clean in testInterface, clean in jasmineTestFramework,
              clean in examples, clean in helloworld,
              clean in reversi, clean in testingExample,
              clean in testSuite, clean in noIrCheckTest,
              clean in javalibExTestSuite,
              clean in partest, clean in partestSuite).value,

          publish := {},
          publishLocal := {}
      )
  )

  val commonIrProjectSettings = (
      commonSettings ++ publishSettings
  ) ++ Seq(
      name := "Scala.js IR",

      binaryIssueFilters ++= BinaryIncompatibilities.IR
  )

  lazy val irProject: Project = Project(
      id = "ir",
      base = file("ir"),
      settings = commonIrProjectSettings ++ Seq(
          previousArtifact := Some(
              "org.scala-js" % s"scalajs-ir_${scalaBinaryVersion.value}" % previousVersion)
      )
  )

  lazy val irProjectJS: Project = Project(
      id = "irJS",
      base = file("ir/.js"),
      settings = commonIrProjectSettings ++ myScalaJSSettings ++ Seq(
          crossVersion := ScalaJSCrossVersion.binary,
          unmanagedSourceDirectories in Compile +=
            (scalaSource in Compile in irProject).value,
          previousArtifact := Some(
              "org.scala-js" % s"scalajs-ir_sjs${previousSJSBinaryVersion}_${scalaBinaryVersion.value}" % previousVersion)
      )
  ).dependsOn(compiler % "plugin", javalibEx)

  lazy val compiler: Project = Project(
      id = "compiler",
      base = file("compiler"),
      settings = commonSettings ++ publishSettings ++ Seq(
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
              (packageBin in (library, Compile)).value.getAbsolutePath
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

  val commonToolsSettings = (
      commonSettings ++ publishSettings
  ) ++ Seq(
      name := "Scala.js tools",

      unmanagedSourceDirectories in Compile +=
        baseDirectory.value.getParentFile / "shared/src/main/scala",

      sourceGenerators in Compile <+= Def.task {
        ScalaJSEnvGenerator.generateEnvHolder(
          baseDirectory.value.getParentFile,
          (sourceManaged in Compile).value)
      },

      binaryIssueFilters ++= BinaryIncompatibilities.Tools
  )

  lazy val tools: Project = Project(
      id = "tools",
      base = file("tools/jvm"),
      settings = commonToolsSettings ++ Seq(
          libraryDependencies ++= Seq(
              "com.google.javascript" % "closure-compiler" % "v20130603",
              "com.googlecode.json-simple" % "json-simple" % "1.1.1",
              "com.novocode" % "junit-interface" % "0.9" % "test"
          ),
          previousArtifact := Some(
              "org.scala-js" % s"scalajs-tools_${scalaBinaryVersion.value}" % previousVersion)
      )
  ).dependsOn(irProject)

  lazy val toolsJS: Project = Project(
      id = "toolsJS",
      base = file("tools/js"),
      settings = myScalaJSSettings ++ commonToolsSettings ++ Seq(
          crossVersion := ScalaJSCrossVersion.binary
      ) ++ inConfig(Test) {
        // Redefine test to run Node.js and link HelloWorld
        test := {
          if (scalaJSStage.value == Stage.PreLink)
            error("Can't run toolsJS/test in preLink stage")

          val cp = {
            for (e <- (fullClasspath in Test).value)
              yield s""""${escapeJS(e.data.getAbsolutePath)}""""
          }

          val code = {
            s"""
            var lib = scalajs.QuickLinker().linkTestSuiteNode(${cp.mkString(", ")});

            var __ScalaJSEnv = null;

            eval("(function() { 'use strict'; " +
              lib + ";" +
              "scalajs.TestRunner().runTests();" +
            "}).call(this);");
            """
          }

          val launcher = new MemVirtualJSFile("Generated launcher file")
            .withContent(code)

          val runner = jsEnv.value.jsRunner(scalaJSExecClasspath.value,
              launcher, streams.value.log, scalaJSConsole.value)

          runner.run()
        }
      } ++ Seq(
          previousArtifact := Some(
              "org.scala-js" % s"scalajs-tools_sjs${previousSJSBinaryVersion}_${scalaBinaryVersion.value}" % previousVersion)
      )
  ).dependsOn(compiler % "plugin", javalibEx, testSuite % "test->test", irProjectJS)

  lazy val jsEnvs: Project = Project(
      id = "jsEnvs",
      base = file("js-envs"),
      settings = commonSettings ++ publishSettings ++ Seq(
          name := "Scala.js JS Envs",
          libraryDependencies ++= Seq(
              "io.apigee" % "rhino" % "1.7R5pre4",
              "org.webjars" % "envjs" % "1.2",
              "com.novocode" % "junit-interface" % "0.9" % "test"
          ) ++ ScalaJSPluginInternal.phantomJSJettyModules.map(_ % "provided"),
          previousArtifact := Some(
              "org.scala-js" % s"scalajs-js-envs_${scalaBinaryVersion.value}" % previousVersion)
      )
  ).dependsOn(tools)

  lazy val testAdapter = Project(
      id = "testAdapter",
      base = file("test-adapter"),
      settings = commonSettings ++ publishSettings ++ Seq(
          name := "Scala.js sbt test adapter",
          libraryDependencies += "org.scala-sbt" % "test-interface" % "1.0",
          previousArtifact := Some(
              "org.scala-js" % s"scalajs-sbt-test-adapter_${scalaBinaryVersion.value}" % previousVersion),
          binaryIssueFilters ++= BinaryIncompatibilities.TestAdapter
      )
  ).dependsOn(jsEnvs)

  lazy val plugin: Project = Project(
      id = "sbtPlugin",
      base = file("sbt-plugin"),
      settings = commonSettings ++ publishIvySettings ++ Seq(
          name := "Scala.js sbt plugin",
          normalizedName := "sbt-scalajs",
          name in bintray := "sbt-scalajs-plugin", // "sbt-scalajs" was taken
          sbtPlugin := true,
          scalaVersion := "2.10.5",
          scalaBinaryVersion :=
            CrossVersion.binaryScalaVersion(scalaVersion.value),
          previousArtifact := Some(
              Defaults.sbtPluginExtra("org.scala-js" % s"sbt-scalajs" % previousVersion,
                  sbtBinaryVersion.value, scalaBinaryVersion.value))
      )
  ).dependsOn(tools, jsEnvs, testAdapter)

  lazy val delambdafySetting = {
    scalacOptions ++= (
        if (scalaBinaryVersion.value == "2.10") Seq()
        else Seq("-Ydelambdafy:method"))
  }

  private def serializeHardcodedIR(base: File,
      infoAndTree: (ir.Infos.ClassInfo, ir.Trees.ClassDef)): File = {
    // We assume that there are no weird characters in the full name
    val fullName = ir.Definitions.decodeClassName(infoAndTree._1.encodedName)
    val output = base / (fullName.replace('.', '/') + ".sjsir")

    if (!output.exists()) {
      IO.createDirectory(output.getParentFile)
      val stream = new BufferedOutputStream(new FileOutputStream(output))
      try {
        ir.InfoSerializers.serialize(stream, infoAndTree._1)
        ir.Serializers.serialize(stream, infoAndTree._2)
      } finally {
        stream.close()
      }
    }
    output
  }

  lazy val javalanglib: Project = Project(
      id = "javalanglib",
      base = file("javalanglib"),
      settings = commonSettings ++ myScalaJSSettings ++ Seq(
          name := "java.lang library for Scala.js",
          publishArtifact in Compile := false,
          delambdafySetting,
          scalacOptions += "-Yskip:cleanup,icode,jvm",
          scalaJSSourceMapSettings,
          compileWithLibrarySetting,

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
  ).dependsOn(compiler % "plugin")

  lazy val javalib: Project = Project(
      id = "javalib",
      base = file("javalib"),
      settings = commonSettings ++ myScalaJSSettings ++ Seq(
          name := "Java library for Scala.js",
          publishArtifact in Compile := false,
          delambdafySetting,
          scalacOptions += "-Yskip:cleanup,icode,jvm",
          scalaJSSourceMapSettings,
          compileWithLibrarySetting
      ) ++ (
          scalaJSExternalCompileSettings
      )
  ).dependsOn(compiler % "plugin")

  lazy val scalalib: Project = Project(
      id = "scalalib",
      base = file("scalalib"),
      settings = commonSettings ++ myScalaJSSettings ++ Seq(
          name := "Scala library for Scala.js",
          publishArtifact in Compile := false,
          delambdafySetting,
          compileWithLibrarySetting,

          // The Scala lib is full of warnings we don't want to see
          scalacOptions ~= (_.filterNot(
              Set("-deprecation", "-unchecked", "-feature") contains _)),


          scalacOptions ++= List(
            // Do not generate .class files
            "-Yskip:cleanup,icode,jvm",
            // Tell plugin to hack fix bad classOf trees
            "-P:scalajs:fixClassOf",
            // Link source maps to github sources of original Scalalib
            "-P:scalajs:mapSourceURI:" +
            (artifactPath in fetchScalaSource).value.toURI +
            "->https://raw.githubusercontent.com/scala/scala/v" +
            scalaVersion.value + "/src/library/"
            ),

          // Link sources in override directories to our GitHub repo
          scalaJSSourceMapSettings,

          artifactPath in fetchScalaSource :=
            target.value / "scalaSources" / scalaVersion.value,

          fetchScalaSource := {
            val s = streams.value
            val cacheDir = s.cacheDirectory
            val ver = scalaVersion.value
            val trgDir = (artifactPath in fetchScalaSource).value

            val report = updateClassifiers.value
            val scalaLibSourcesJar = report.select(
                configuration = Set("compile"),
                module = moduleFilter(name = "scala-library"),
                artifact = artifactFilter(`type` = "src")).headOption.getOrElse {
              sys.error(s"Could not fetch scala-library sources for version $ver")
            }

            FileFunction.cached(cacheDir / s"fetchScalaSource-$ver",
                FilesInfo.lastModified, FilesInfo.exists) { dependencies =>
              s.log.info(s"Unpacking Scala library sources to $trgDir...")

              if (trgDir.exists)
                IO.delete(trgDir)
              IO.createDirectory(trgDir)
              IO.unzip(scalaLibSourcesJar, trgDir)
            } (Set(scalaLibSourcesJar))

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
            val scalaSrcDir = fetchScalaSource.value

            // All source directories (overrides shadow scalaSrcDir)
            val sourceDirectories =
              (unmanagedSourceDirectories in Compile).value :+ scalaSrcDir

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
  ).dependsOn(compiler % "plugin")

  lazy val libraryAux: Project = Project(
      id = "libraryAux",
      base = file("library-aux"),
      settings = commonSettings ++ myScalaJSSettings ++ Seq(
          name := "Scala.js aux library",
          publishArtifact in Compile := false,
          delambdafySetting,
          scalacOptions += "-Yskip:cleanup,icode,jvm",
          scalaJSSourceMapSettings,
          compileWithLibrarySetting
      ) ++ (
          scalaJSExternalCompileSettings
      )
  ).dependsOn(compiler % "plugin")

  /** Patch the scaladoc css */
  private val patchDocSetting = {
    /* After executing the normal doc command, copy everything verbatim to
     * `patched-api` (same directory structure). In addition, append our
     * additional doc CSS to `lib/template.css` after copying.
     */

    doc in Compile := {
      val docDir = (doc in Compile).value
      val cacheDir = streams.value.cacheDirectory
      val outDir = crossTarget.value / "patched-api"
      val docPaths =
        Path.selectSubpaths(docDir, new SimpleFileFilter(_.isFile)).toMap

      val additionalStylesFile =
        (baseDirectory in library).value / "additional-doc-styles.css"

      FileFunction.cached(cacheDir,
          FilesInfo.lastModified, FilesInfo.exists) { files =>
        for {
          file <- files
          if file != additionalStylesFile
        } yield {
          val relPath = docPaths(file)
          val outFile = outDir / relPath
          IO.copyFile(file, outFile)

          if (relPath == "lib/template.css") {
            val styles = IO.read(additionalStylesFile)
            IO.append(outFile, styles)
          }

          outFile
        }
      } (docPaths.keySet + additionalStylesFile)

      outDir
    }
  }

  lazy val library: Project = Project(
      id = "library",
      base = file("library"),
      settings = commonSettings ++ publishSettings ++ myScalaJSSettings ++ Seq(
          name := "Scala.js library",
          delambdafySetting,
          scalaJSSourceMapSettings,
          scalacOptions in (Compile, doc) ++= Seq("-implicits", "-groups"),
          exportJars := true,
          previousArtifact := Some(
              "org.scala-js" % s"scalajs-library_${scalaBinaryVersion.value}" % previousVersion)
      ) ++ (
          scalaJSExternalCompileSettings
      ) ++ inConfig(Compile)(Seq(
          /* Add the .sjsir files from other lib projects
           * (but not .class files)
           */
          mappings in packageBin := {
            /* From library, we must take everyting, except the
             * java.nio.TypedArrayBufferBridge object, whose actual
             * implementation is in javalib.
             */
            val superMappings = (mappings in packageBin).value
            val libraryMappings = superMappings.filter(
                _._2.replace('\\', '/') !=
                  "scala/scalajs/js/typedarray/TypedArrayBufferBridge$.sjsir")

            val allProducts = (
                (products in javalanglib).value ++
                (products in javalib).value ++
                (products in scalalib).value ++
                (products in libraryAux).value)
            val filter = ("*.sjsir": NameFilter)
            val otherMappings =
              allProducts.flatMap(base => Path.selectSubpaths(base, filter))

            libraryMappings ++ otherMappings
          },

          patchDocSetting
      ))
  ).dependsOn(compiler % "plugin")

  lazy val javalibEx: Project = Project(
      id = "javalibEx",
      base = file("javalib-ex"),
      settings = commonSettings ++ publishSettings ++ myScalaJSSettings ++ Seq(
          name := "Scala.js JavaLib Ex",
          delambdafySetting,
          scalacOptions += "-Yskip:cleanup,icode,jvm",
          scalaJSSourceMapSettings,
          exportJars := true,
          jsDependencies +=
            "org.webjars" % "jszip" % "2.4.0" / "jszip.min.js" commonJSName "JSZip",
          patchDocSetting
      ) ++ (
          scalaJSExternalCompileSettings
      )
  ).dependsOn(compiler % "plugin", library)

  lazy val stubs: Project = Project(
      id = "stubs",
      base = file("stubs"),
      settings = commonSettings ++ publishSettings ++ Seq(
          name := "Scala.js Stubs",
          libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
          previousArtifact := Some(
              "org.scala-js" % s"scalajs-stubs_${scalaBinaryVersion.value}" % previousVersion)
      )
  )

  // Scala.js command line interface
  lazy val cli: Project = Project(
      id = "cli",
      base = file("cli"),
      settings = commonSettings ++ assemblySettings ++ Seq(
          name := "Scala.js CLI",
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
  lazy val testInterface = Project(
      id = "testInterface",
      base = file("test-interface"),
      settings = commonSettings ++ publishSettings ++ myScalaJSSettings ++ Seq(
          name := "Scala.js test interface",
          delambdafySetting,
          scalaJSSourceMapSettings,
          previousArtifact := Some(
              "org.scala-js" % s"scalajs-test-interface_${scalaBinaryVersion.value}" % previousVersion)
      )
  ).dependsOn(compiler % "plugin", library)

  lazy val jasmineTestFramework = Project(
      id = "jasmineTestFramework",
      base = file("jasmine-test-framework"),
      settings = commonSettings ++ myScalaJSSettings ++ Seq(
          name := "Scala.js jasmine test framework",

          jsDependencies ++= Seq(
            ProvidedJS / "jasmine-polyfills.js",
            "org.webjars" % "jasmine" % "1.3.1" /
              "jasmine.js" dependsOn "jasmine-polyfills.js"
          ),
          scalaJSSourceMapSettings
      )
  ).dependsOn(compiler % "plugin", library, testInterface)

  // Examples

  lazy val examples: Project = Project(
      id = "examples",
      base = file("examples"),
      settings = commonSettings ++ Seq(
          name := "Scala.js examples"
      )
  ).aggregate(helloworld, reversi, testingExample)

  lazy val exampleSettings = commonSettings ++ myScalaJSSettings

  lazy val helloworld: Project = Project(
      id = "helloworld",
      base = file("examples") / "helloworld",
      settings = exampleSettings ++ Seq(
          name := "Hello World - Scala.js example",
          moduleName := "helloworld",
          persistLauncher := true
      )
  ).dependsOn(compiler % "plugin", library)

  lazy val reversi = Project(
      id = "reversi",
      base = file("examples") / "reversi",
      settings = exampleSettings ++ Seq(
          name := "Reversi - Scala.js example",
          moduleName := "reversi"
      )
  ).dependsOn(compiler % "plugin", library)

  lazy val testingExample = Project(
      id = "testingExample",
      base = file("examples") / "testing",
      settings = exampleSettings ++ Seq(
          name := "Testing - Scala.js example",
          moduleName := "testing",

          jsDependencies ++= Seq(
            RuntimeDOM % "test",
            "org.webjars" % "jquery" % "1.10.2" / "jquery.js" % "test"
          )
      )
  ).dependsOn(compiler % "plugin", library, jasmineTestFramework % "test")

  // Testing

  val testTagSettings = Seq(
      testOptions in Test ++= {
        val envTags = (jsEnv in Test).value match {
          case env: RhinoJSEnv =>
            val baseArgs = Seq("-trhino")
            val args =
              if (env.sourceMap) baseArgs :+ "-tsource-maps"
              else baseArgs

            Seq(Tests.Argument(args: _*))

          case env: NodeJSEnv =>
            val baseArgs = Seq("-tnodejs", "-ttypedarray")
            val args = {
              if (env.sourceMap) {
                if (!env.hasSourceMapSupport) {
                  val projectId = thisProject.value.id
                  sys.error("You must install Node.js source map support to " +
                    "run the full Scala.js test suite (npm install " +
                    "source-map-support). To deactivate source map " +
                    s"tests, do: set postLinkJSEnv in $projectId := " +
                    "NodeJSEnv().value.withSourceMap(false)")
                }
                baseArgs :+ "-tsource-maps"
              } else
                baseArgs
            }

            Seq(Tests.Argument(args: _*))

          case _: PhantomJSEnv =>
            Seq(Tests.Argument("-tphantomjs"))
          case _ =>
            Seq()
        }

        val sems = (scalaJSSemantics in Test).value
        val semTags = (
            if (sems.asInstanceOfs == CheckedBehavior.Compliant)
              Seq(Tests.Argument("-tcompliant-asinstanceofs"))
            else
              Seq()
        ) ++ (
            if (sems.moduleInit == CheckedBehavior.Compliant)
              Seq(Tests.Argument("-tcompliant-moduleinit"))
            else
              Seq()
        ) ++ (
            if (sems.strictFloats) Seq(Tests.Argument("-tstrict-floats"))
            else Seq()
        )

        val stageTag = Tests.Argument((scalaJSStage in Test).value match {
          case PreLinkStage => "-tprelink-stage"
          case FastOptStage => "-tfastopt-stage"
          case FullOptStage => "-tfullopt-stage"
        })

        envTags ++ semTags :+ stageTag
      }
  )

  lazy val testSuite: Project = Project(
      id = "testSuite",
      base = file("test-suite"),
      settings = commonSettings ++ myScalaJSSettings ++ testTagSettings ++ Seq(
          name := "Scala.js test suite",
          publishArtifact in Compile := false,

          scalacOptions ~= (_.filter(_ != "-deprecation")),

          scalaJSSemantics ~= (_.withRuntimeClassName(_.fullName match {
            case "org.scalajs.testsuite.compiler.ReflectionTest$RenamedTestClass" =>
              "renamed.test.Class"
            case fullName =>
              fullName
          })),

          sources in Test ++= {
            if (!scalaVersion.value.startsWith("2.10") &&
                scalacOptions.value.contains("-Xexperimental")) {
              (((sourceDirectory in Test).value / "require-sam") ** "*.scala").get
            } else {
              Nil
            }
          },

          /* Generate a scala source file that throws exceptions in
             various places (while attaching the source line to the
             exception). When we catch the exception, we can then
             compare the attached source line and the source line
             calculated via the source maps.

             see test-suite/src/test/resources/SourceMapTestTemplate.scala
           */
          sourceGenerators in Test <+= Def.task {
            val dir = (sourceManaged in Test).value
            IO.createDirectory(dir)

            val template = IO.read((resourceDirectory in Test).value /
              "SourceMapTestTemplate.scala")

            def lineNo(cs: CharSequence) =
              (0 until cs.length).count(i => cs.charAt(i) == '\n') + 1

            var i = 0
            val pat = "/\\*{2,3}/".r
            val replaced = pat.replaceAllIn(template, { mat =>
              val lNo = lineNo(mat.before)
              val res =
                if (mat.end - mat.start == 5)
                  // matching a /***/
                  s"if (TC.is($i)) { throw new TestException($lNo) } else "
                else
                  // matching a /**/
                  s"; if (TC.is($i)) { throw new TestException($lNo) } ;"

              i += 1

              res
            })

            val outFile = dir / "SourceMapTest.scala"
            IO.write(outFile, replaced.replace("0/*<testCount>*/", i.toString))
            Seq(outFile)
          }
      )
  ).dependsOn(compiler % "plugin", library, jasmineTestFramework % "test")

  lazy val noIrCheckTest: Project = Project(
      id = "noIrCheckTest",
      base = file("no-ir-check-test"),
      settings = commonSettings ++ myScalaJSSettings ++ testTagSettings ++ Seq(
          name := "Scala.js not IR checked tests",
          scalaJSOptimizerOptions ~= (_.
              withCheckScalaJSIR(false).
              withBypassLinkingErrors(true)
          ),
          publishArtifact in Compile := false
     )
  ).dependsOn(compiler % "plugin", library, jasmineTestFramework % "test")

  lazy val javalibExTestSuite: Project = Project(
      id = "javalibExTestSuite",
      base = file("javalib-ex-test-suite"),
      settings = commonSettings ++ myScalaJSSettings ++ testTagSettings ++ Seq(
          name := "JavaLib Ex Test Suite",
          publishArtifact in Compile := false,

          scalacOptions in Test ~= (_.filter(_ != "-deprecation"))
      )
  ).dependsOn(compiler % "plugin", javalibEx, jasmineTestFramework % "test")

  lazy val partest: Project = Project(
      id = "partest",
      base = file("partest"),
      settings = commonSettings ++ Seq(
          name := "Partest for Scala.js",
          moduleName := "scalajs-partest",

          resolvers += Resolver.typesafeIvyRepo("releases"),

          artifactPath in fetchScalaSource :=
            baseDirectory.value / "fetchedSources" / scalaVersion.value,

          fetchScalaSource := {
            import org.eclipse.jgit.api._

            val s = streams.value
            val ver = scalaVersion.value
            val trgDir = (artifactPath in fetchScalaSource).value

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

          libraryDependencies ++= {
            if (shouldPartest.value)
              Seq(
                "org.scala-sbt" % "sbt" % sbtVersion.value,
                "org.scala-lang.modules" %% "scala-partest" % "1.0.1",
                "com.google.javascript" % "closure-compiler" % "v20130603",
                "io.apigee" % "rhino" % "1.7R5pre4",
                "com.googlecode.json-simple" % "json-simple" % "1.1.1"
              )
            else Seq()
          },

          sources in Compile := {
            if (shouldPartest.value) {
              // Partest sources and some sources of sbtplugin (see above)
              val baseSrcs = (sources in Compile).value
              // Sources for tools (and hence IR)
              val toolSrcs = (sources in (tools, Compile)).value
              // Sources for js-envs
              val jsenvSrcs = {
                val jsenvBase = ((scalaSource in (jsEnvs, Compile)).value /
                  "org/scalajs/jsenv")

                val scalaFilter: FileFilter = "*.scala"
                val files = (
                    (jsenvBase * scalaFilter) +++
                    (jsenvBase / "nodejs" ** scalaFilter) +++
                    (jsenvBase / "rhino" ** scalaFilter))

                files.get
              }
              toolSrcs ++ baseSrcs ++ jsenvSrcs
            } else Seq()
          }

      )
  ).dependsOn(compiler)

  lazy val partestSuite: Project = Project(
      id = "partestSuite",
      base = file("partest-suite"),
      settings = commonSettings ++ Seq(
          name := "Scala.js partest suite",

          fork in Test := true,
          javaOptions in Test += "-Xmx1G",

          testFrameworks ++= {
            if (shouldPartest.value)
              Seq(new TestFramework("scala.tools.partest.scalajs.Framework"))
            else Seq()
          },

          definedTests in Test <++= Def.taskDyn[Seq[sbt.TestDefinition]] {
            if (shouldPartest.value) Def.task {
              val _ = (fetchScalaSource in partest).value
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
            } else {
              Def.task(Seq())
            }
          }
      )
  ).dependsOn(partest % "test", library)
}
