package build

import scala.language.implicitConversions

import scala.annotation.tailrec

import sbt._
import Keys._

import com.typesafe.tools.mima.plugin.MimaPlugin.autoImport._
import de.heikoseeberger.sbtheader.HeaderPlugin.autoImport._

import java.io.{
  BufferedOutputStream,
  FileOutputStream
}

import scala.collection.mutable
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.Properties

import org.scalajs.ir

import org.scalajs.sbtplugin._
import org.scalajs.jsenv.{JSEnv, RunConfig, Input}
import org.scalajs.jsenv.JSUtils.escapeJS
import org.scalajs.jsenv.nodejs.NodeJSEnv

import ScalaJSPlugin.autoImport.{ModuleKind => _, _}
import ExternalCompile.scalaJSExternalCompileSettings
import Loggers._

import org.scalajs.linker._
import org.scalajs.linker.irio._

/* Things that we want to expose in the sbt command line (and hence also in
 * `ci/matrix.xml`).
 */
object ExposedValues extends AutoPlugin {
  object autoImport {
    // set scalaJSLinkerConfig in someProject ~= makeCompliant
    val makeCompliant: StandardLinker.Config => StandardLinker.Config = {
      _.withSemantics { semantics =>
        semantics
          .withAsInstanceOfs(CheckedBehavior.Compliant)
          .withArrayIndexOutOfBounds(CheckedBehavior.Compliant)
          .withModuleInit(CheckedBehavior.Compliant)
          .withStrictFloats(true)
      }
    }

    val CheckedBehavior = org.scalajs.linker.CheckedBehavior

    type NodeJSEnvForcePolyfills = build.NodeJSEnvForcePolyfills
  }
}

object MyScalaJSPlugin extends AutoPlugin {
  override def requires: Plugins = ScalaJSPlugin

  val isGeneratingEclipse =
    Properties.envOrElse("GENERATING_ECLIPSE", "false").toBoolean

  val wantSourceMaps = settingKey[Boolean]("Whether source maps should be used")

  override def projectSettings: Seq[Setting[_]] = Def.settings(
      /* Remove libraryDependencies on ourselves; we use .dependsOn() instead
       * inside this build.
       */
      libraryDependencies ~= { libDeps =>
        val blacklist =
          Set("scalajs-compiler", "scalajs-library", "scalajs-test-bridge")
        libDeps.filterNot(dep => blacklist.contains(dep.name))
      },

      /* Most of our Scala.js libraries are not cross-compiled against the
       * the Scala.js binary version number.
       */
      crossVersion := CrossVersion.binary,

      scalaJSLinkerConfig ~= (_.withCheckIR(true)),

      wantSourceMaps := true,

      jsEnv := new NodeJSEnv(
          NodeJSEnv.Config().withSourceMap(wantSourceMaps.value)),

      // Link source maps
      scalacOptions ++= {
        val base = (baseDirectory in LocalProject("scalajs")).value
        if (isGeneratingEclipse) Seq()
        else if (isSnapshot.value) Seq()
        else Seq(
          // Link source maps to github sources
          "-P:scalajs:mapSourceURI:" + base.toURI +
          "->https://raw.githubusercontent.com/scala-js/scala-js/v" +
          scalaJSVersion + "/"
        )
      }
  )
}

object Build {
  import MyScalaJSPlugin.isGeneratingEclipse

  val bintrayProjectName = settingKey[String](
      "Project name on Bintray")

  val setModuleLoopbackScript = taskKey[Option[java.nio.file.Path]](
      "In the test suite, under ES modules, the script that sets the " +
      "loopback module namespace")

  val fetchScalaSource = taskKey[File](
    "Fetches the scala source for the current scala version")
  val shouldPartest = settingKey[Boolean](
    "Whether we should partest the current scala version (and fail if we can't)")

  /* MiMa configuration -- irrelevant while in 1.0.0-SNAPSHOT.
  val previousVersion = "0.6.28"
  val previousSJSBinaryVersion =
    ScalaJSCrossVersion.binaryScalaJSVersion(previousVersion)
  val previousBinaryCrossVersion =
    CrossVersion.binaryMapped(v => s"sjs${previousSJSBinaryVersion}_$v")

  val scalaVersionsUsedForPublishing: Set[String] =
    Set("2.10.7", "2.11.12", "2.12.8")
  val newScalaBinaryVersionsInThisRelease: Set[String] =
    Set()
  */

  def hasNewCollections(version: String): Boolean = {
    !version.startsWith("2.10.") &&
    !version.startsWith("2.11.") &&
    !version.startsWith("2.12.")
  }

  /** Returns the appropriate subdirectory of `sourceDir` depending on the
   *  collection "era" used by the `scalaV`.
   *
   *  It can be the new collections (2.13.x+) or the old collections (until
   *  2.12.x).
   */
  def collectionsEraDependentDirectory(scalaV: String, sourceDir: File): File =
    if (hasNewCollections(scalaV)) sourceDir / "scala-new-collections"
    else sourceDir / "scala-old-collections"

  val javaVersion = settingKey[Int](
    "The major Java SDK version that should be assumed for compatibility. " +
    "Defaults to what sbt is running with.")

  val javaDocBaseURL: String = "http://docs.oracle.com/javase/8/docs/api/"

  private def includeIf(testDir: File, condition: Boolean): List[File] =
    if (condition) List(testDir)
    else Nil

  val previousArtifactSetting: Setting[_] = {
    mimaPreviousArtifacts ++= {
      /* MiMa is completely disabled while we are in 1.0.0-SNAPSHOT.
      val scalaV = scalaVersion.value
      val scalaBinaryV = scalaBinaryVersion.value
      if (!scalaVersionsUsedForPublishing.contains(scalaV)) {
        // This artifact will not be published. Binary compatibility is irrelevant.
        Set.empty
      } else if (newScalaBinaryVersionsInThisRelease.contains(scalaBinaryV)) {
        // New in this release, no binary compatibility to comply to
        Set.empty
      } else {
        val thisProjectID = projectID.value
        val previousCrossVersion = thisProjectID.crossVersion match {
          case ScalaJSCrossVersion.binary => previousBinaryCrossVersion
          case crossVersion               => crossVersion
        }
        /* Filter out e:info.apiURL as it expects 0.6.7-SNAPSHOT, whereas the
         * artifact we're looking for has 0.6.6 (for example).
         */
        val prevExtraAttributes =
          thisProjectID.extraAttributes.filterKeys(_ != "e:info.apiURL")
        val prevProjectID =
          (thisProjectID.organization % thisProjectID.name % previousVersion)
            .cross(previousCrossVersion)
            .extra(prevExtraAttributes.toSeq: _*)
        Set(CrossVersion(scalaV, scalaBinaryV)(prevProjectID).cross(CrossVersion.Disabled))
      }
      */
      Set.empty
    }
  }

  val commonSettings = Seq(
      scalaVersion := "2.12.8",
      organization := "org.scala-js",
      version := scalaJSVersion,

      normalizedName ~= {
        _.replace("scala.js", "scalajs").replace("scala-js", "scalajs")
      },

      homepage := Some(url("https://www.scala-js.org/")),
      startYear := Some(2013),
      licenses += (("Apache-2.0", url("https://www.apache.org/licenses/LICENSE-2.0"))),
      headerLicense := Some(HeaderLicense.Custom(
        s"""Scala.js (${homepage.value.get})
           |
           |Copyright EPFL.
           |
           |Licensed under Apache License 2.0
           |(https://www.apache.org/licenses/LICENSE-2.0).
           |
           |See the NOTICE file distributed with this work for
           |additional information regarding copyright ownership.
           |""".stripMargin
      )),
      scmInfo := Some(ScmInfo(
          url("https://github.com/scala-js/scala-js"),
          "scm:git:git@github.com:scala-js/scala-js.git",
          Some("scm:git:git@github.com:scala-js/scala-js.git"))),

      shouldPartest := {
        val testListDir = (
          (resourceDirectory in (LocalProject("partestSuite"), Test)).value / "scala"
            / "tools" / "partest" / "scalajs" / scalaVersion.value
        )
        testListDir.exists
      },

      scalacOptions ++= Seq(
          "-deprecation",
          "-unchecked",
          "-feature",
          "-encoding", "utf8"
      ),

      // Scaladoc linking
      apiURL := {
        val name = normalizedName.value
        Some(url(s"http://www.scala-js.org/api/$name/$scalaJSVersion/"))
      },
      autoAPIMappings := true,

      // Add Java Scaladoc mapping
      apiMappings ++= {
        val optRTJar = {
          val bootClasspath = System.getProperty("sun.boot.class.path")
          if (bootClasspath != null) {
            // JDK <= 8, there is an rt.jar (or classes.jar) on the boot classpath
            val jars = bootClasspath.split(java.io.File.pathSeparator)
            def matches(path: String, name: String): Boolean =
              path.endsWith(s"${java.io.File.separator}$name.jar")
            val jar = jars.find(matches(_, "rt")) // most JREs
              .orElse(jars.find(matches(_, "classes"))) // Java 6 on Mac OS X
              .get
            Some(file(jar))
          } else {
            // JDK >= 9, maybe sbt gives us a fake rt.jar in `scala.ext.dirs`
            val scalaExtDirs = Option(System.getProperty("scala.ext.dirs"))
            scalaExtDirs.map(extDirs => file(extDirs) / "rt.jar")
          }
        }

        optRTJar.fold[Map[File, URL]] {
          Map.empty
        } { rtJar =>
          assert(rtJar.exists, s"$rtJar does not exist")
          Map(rtJar -> url(javaDocBaseURL))
        }
      },

      /* Add a second Java Scaladoc mapping for cases where Scala actually
       * understands the jrt:/ filesystem of Java 9.
       */
      apiMappings +=
        file("/modules/java.base") -> url(javaDocBaseURL),

      /* Patch the ScalaDoc we generate.
       *
       *  After executing the normal doc command, copy everything to the
       *  `patched-api` directory (same internal directory structure) while
       *  patching the following:
       *
       *  - Append `additional-doc-styles.css` to `lib/template.css`
       *  - Fix external links to the JavaDoc, i.e. change
       *    `${javaDocBaseURL}index.html#java.lang.String` to
       *    `${javaDocBaseURL}index.html?java/lang/String.html`
       */
      doc in Compile := {
        // Where to store the patched docs
        val outDir = crossTarget.value / "patched-api"

        // Find all files in the current docs
        val docPaths = {
          val docDir = (doc in Compile).value
          Path.selectSubpaths(docDir, new SimpleFileFilter(_.isFile)).toMap
        }

        /* File with our CSS styles (needs to be canonical so that the
         * comparison below works)
         */
        val additionalStylesFile =
          (root.base / "assets/additional-doc-styles.css").getCanonicalFile

        // Regex and replacement function for JavaDoc linking
        val javadocAPIRe =
          s"""\"(\\Q${javaDocBaseURL}index.html\\E)#([^"]*)\"""".r

        val logger = streams.value.log
        val errorsSeen = mutable.Set.empty[String]

        val fixJavaDocLink = { (m: scala.util.matching.Regex.Match) =>
          val frag = m.group(2)

          // Fail when encountering links to class members
          if (frag.contains("@") && !errorsSeen.contains(frag)) {
            errorsSeen += frag
            logger.error(s"Cannot fix JavaDoc link to member: $frag")
          }

          m.group(1) + "?" + frag.replace('.', '/') + ".html"
        }

        FileFunction.cached(streams.value.cacheDirectory,
            FilesInfo.lastModified, FilesInfo.exists) { files =>
          for {
            file <- files
            if file != additionalStylesFile
          } yield {
            val relPath = docPaths(file)
            val outFile = outDir / relPath

            if (relPath == "lib/template.css") {
              val styles = IO.read(additionalStylesFile)
              IO.copyFile(file, outFile)
              IO.append(outFile, styles)
            } else if (relPath.endsWith(".html")) {
              val content = IO.read(file)
              val patched = javadocAPIRe.replaceAllIn(content, fixJavaDocLink)
              IO.write(outFile, patched)
            } else {
              IO.copyFile(file, outFile)
            }

            outFile
          }
        } (docPaths.keySet + additionalStylesFile)

        if (errorsSeen.nonEmpty)
          throw new MessageOnlyException("ScalaDoc patching had errors")

        outDir
      }
  )

  val noClassFilesSettings: Setting[_] = (
      scalacOptions in (Compile, compile) ++= {
        if (isGeneratingEclipse) Seq()
        else Seq("-Yskip:cleanup,icode,jvm")
      }
  )

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
            <developer>
              <id>nicolasstucki</id>
              <name>Nicolas Stucki</name>
              <url>https://github.com/nicolasstucki/</url>
            </developer>
          </developers>
      ),
      pomIncludeRepository := { _ => false }
  )

  val fatalWarningsSettings = Seq(
      // The pattern matcher used to exceed its analysis budget before 2.11.5
      scalacOptions ++= {
        scalaVersion.value.split('.') match {
          case Array("2", "10", _)                 => Nil
          case Array("2", "11", x)
              if x.takeWhile(_.isDigit).toInt <= 4 => Nil
          case _                                   => Seq("-Xfatal-warnings")
        }
      },

      scalacOptions in (Compile, doc) := {
        val baseOptions = (scalacOptions in (Compile, doc)).value

        // in Scala 2.10, some ScalaDoc links fail
        val fatalInDoc = scalaBinaryVersion.value != "2.10"

        if (fatalInDoc) baseOptions
        else baseOptions.filterNot(_ == "-Xfatal-warnings")
      }
  )

  private def publishToBintraySettings = Def.settings(
      publishTo := {
        val proj = bintrayProjectName.value
        val ver = version.value
        if (isSnapshot.value) {
          None // Bintray does not support snapshots
        } else {
          val url = new java.net.URL(
              s"https://api.bintray.com/content/scala-js/scala-js-releases/$proj/$ver")
          val patterns = Resolver.ivyStylePatterns
          Some(Resolver.url("bintray", url)(patterns))
        }
      }
  )

  val publishIvySettings = Def.settings(
      publishToBintraySettings,
      publishMavenStyle := false
  )

  private def parallelCollectionsDependencies(
      scalaVersion: String): Seq[ModuleID] = {
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((2, n)) if n >= 13 =>
        Seq("org.scala-lang.modules" %% "scala-parallel-collections" % "0.1.2")

      case _ => Nil
    }
  }

  implicit class ProjectOps(val project: Project) extends AnyVal {
    /** Uses the Scala.js compiler plugin. */
    def withScalaJSCompiler: Project =
      if (isGeneratingEclipse) project
      else project.dependsOn(compiler % "plugin")

    def withScalaJSJUnitPlugin: Project = {
      project.settings(
          scalacOptions in Test ++= {
            if (isGeneratingEclipse) {
              Seq.empty
            } else {
              val jar = (packageBin in (jUnitPlugin, Compile)).value
              Seq(s"-Xplugin:$jar")
            }
          }
      )
    }

    /** Depends on library as if (exportJars in library) was set to false. */
    def dependsOnLibraryNoJar: Project = {
      if (isGeneratingEclipse) {
        project.dependsOn(library)
      } else {
        project.settings(
            internalDependencyClasspath in Compile ++= {
              val prods = (products in (library, Compile)).value
              val analysis = (compile in (library, Compile)).value
              prods.map(p => Classpaths.analyzed(p, analysis))
            }
        )
      }
    }

    /** Depends on the sources of another project. */
    def dependsOnSource(dependency: Project): Project = {
      if (isGeneratingEclipse) {
        project.dependsOn(dependency)
      } else {
        project.settings(
            unmanagedSourceDirectories in Compile +=
              (scalaSource in (dependency, Compile)).value
        )
      }
    }
  }

  val thisBuildSettings = Def.settings(
      // JDK version we are running with
      javaVersion in Global := {
        val fullVersion = System.getProperty("java.version")
        val v = fullVersion.stripPrefix("1.").takeWhile(_.isDigit).toInt
        sLog.value.info(s"Detected JDK version $v")
        if (v < 8)
          throw new MessageOnlyException("This build requires JDK 8 or later. Aborting.")
        v
      }
  )

  lazy val root: Project = Project(id = "scalajs", base = file(".")).settings(
      commonSettings,
      name := "Scala.js",
      publishArtifact in Compile := false,

      {
        val allProjects = Seq(
            compiler, irProject, irProjectJS, logging, loggingJS,
            linker, linkerJS,
            jsEnvs, jsEnvsTestKit, nodeJSEnv, testAdapter, plugin,
            javalanglib, javalib, scalalib, libraryAux, library, minilib,
            testInterface, jUnitRuntime, testBridge, jUnitPlugin, jUnitAsyncJS,
            jUnitAsyncJVM, jUnitTestOutputsJS, jUnitTestOutputsJVM,
            helloworld, reversi, testingExample, testSuite, testSuiteJVM,
            testSuiteEx,
            partest, partestSuite,
            scalaTestSuite
        )

        val keys = Seq[TaskKey[_]](
            clean, headerCreate in Compile, headerCreate in Test,
            headerCheck in Compile, headerCheck in Test
        )

        for (key <- keys) yield {
          /* The match is only used to capture the type parameter `a` of
           * each individual TaskKey.
           */
          key match {
            case key: TaskKey[a] =>
              key := key.dependsOn(allProjects.map(key in _): _*).value
          }
        }
      },

      headerCreate := (headerCreate in Test).dependsOn(headerCreate in Compile).value,
      headerCheck := (headerCheck in Test).dependsOn(headerCheck in Compile).value,

      publish := {},
      publishLocal := {}
  )

  val commonIrProjectSettings = Def.settings(
      commonSettings,
      publishSettings,
      fatalWarningsSettings,
      name := "Scala.js IR",
      previousArtifactSetting,
      mimaBinaryIssueFilters ++= BinaryIncompatibilities.IR,
      exportJars := true, // required so ScalaDoc linking works

      testOptions += Tests.Argument(TestFrameworks.JUnit, "-a", "-s")
  )

  lazy val irProject: Project = Project(id = "ir", base = file("ir")).settings(
      commonIrProjectSettings,
      libraryDependencies +=
        "com.novocode" % "junit-interface" % "0.9" % "test"
  )

  lazy val irProjectJS: Project = Project(
      id = "irJS", base = file("ir/.js")
  ).enablePlugins(
      MyScalaJSPlugin
  ).settings(
      commonIrProjectSettings,
      crossVersion := ScalaJSCrossVersion.binary,
      unmanagedSourceDirectories in Compile +=
        (scalaSource in Compile in irProject).value,
      unmanagedSourceDirectories in Test +=
        (scalaSource in Test in irProject).value
  ).withScalaJSCompiler.withScalaJSJUnitPlugin.dependsOn(
      library, jUnitRuntime % "test", testBridge % "test"
  )

  lazy val compiler: Project = project.settings(
      commonSettings,
      publishSettings,
      fatalWarningsSettings,
      name := "Scala.js compiler",
      crossVersion := CrossVersion.full, // because compiler api is not binary compatible
      libraryDependencies ++= Seq(
          "org.scala-lang" % "scala-compiler" % scalaVersion.value,
          "org.scala-lang" % "scala-reflect" % scalaVersion.value,
          "com.novocode" % "junit-interface" % "0.9" % "test"
      ),
      testOptions += Tests.Argument(TestFrameworks.JUnit, "-a"),
      testOptions += Tests.Setup { () =>
        val testOutDir = (streams.value.cacheDirectory / "scalajs-compiler-test")
        IO.createDirectory(testOutDir)
        System.setProperty("scala.scalajs.compiler.test.output",
            testOutDir.getAbsolutePath)
        System.setProperty("scala.scalajs.compiler.test.scalajslib",
            (packageBin in (LocalProject("library"), Compile)).value.getAbsolutePath)

        def scalaArtifact(name: String): String = {
          def isTarget(att: Attributed[File]) = {
            att.metadata.get(moduleID.key).exists { mId =>
              mId.organization == "org.scala-lang" &&
              mId.name == name &&
              mId.revision == scalaVersion.value
            }
          }

          (managedClasspath in Test).value.find(isTarget).fold {
            streams.value.log.error(s"Couldn't find $name on the classpath")
            ""
          } { lib =>
            lib.data.getAbsolutePath
          }
        }

        System.setProperty("scala.scalajs.compiler.test.scalalib",
            scalaArtifact("scala-library"))

        System.setProperty("scala.scalajs.compiler.test.scalareflect",
            scalaArtifact("scala-reflect"))
      },
      exportJars := true
  ).dependsOnSource(irProject)

  val commonLoggingSettings = Def.settings(
      commonSettings,
      publishSettings,
      fatalWarningsSettings,
      name := "Scala.js Logging",
      previousArtifactSetting,
      mimaBinaryIssueFilters ++= BinaryIncompatibilities.Logging,
      exportJars := true, // required so ScalaDoc linking works

      unmanagedSourceDirectories in Compile +=
        baseDirectory.value.getParentFile / "shared/src/main/scala"
  )

  lazy val logging: Project = (project in file("logging/jvm")).settings(
      commonLoggingSettings
  )

  lazy val loggingJS: Project = (project in file("logging/js")).enablePlugins(
      MyScalaJSPlugin
  ).settings(
      commonLoggingSettings,
      crossVersion := ScalaJSCrossVersion.binary
  ).withScalaJSCompiler.dependsOn(
      library
  )

  val commonLinkerSettings = Def.settings(
      commonSettings,
      publishSettings,
      fatalWarningsSettings,
      name := "Scala.js linker",

      unmanagedSourceDirectories in Compile +=
        baseDirectory.value.getParentFile / "shared/src/main/scala",
      unmanagedSourceDirectories in Test +=
        baseDirectory.value.getParentFile / "shared/src/test/scala",

      sourceGenerators in Test += Def.task {
        ConstantHolderGenerator.generate(
            (sourceManaged in Test).value,
            "org.scalajs.linker.testutils.StdlibHolder",
            "minilib" -> (packageBin in (LocalProject("minilib"), Compile)).value,
            "fulllib" -> (packageBin in (LocalProject("library"), Compile)).value)
      }.taskValue,

      previousArtifactSetting,
      mimaBinaryIssueFilters ++= BinaryIncompatibilities.Linker,
      exportJars := true, // required so ScalaDoc linking works

      testOptions += Tests.Argument(TestFrameworks.JUnit, "-a")
  )

  lazy val linker: Project = (project in file("linker/jvm")).settings(
      commonLinkerSettings,
      libraryDependencies ++= Seq(
          "com.google.javascript" % "closure-compiler" % "v20190513",
          "com.novocode" % "junit-interface" % "0.9" % "test"
      ) ++ (
          parallelCollectionsDependencies(scalaVersion.value)
      ),
      fork in Test := true
  ).dependsOn(irProject, logging, jUnitAsyncJVM % "test")

  lazy val linkerJS: Project = (project in file("linker/js")).enablePlugins(
      MyScalaJSPlugin
  ).settings(
      commonLinkerSettings,
      crossVersion := ScalaJSCrossVersion.binary,
      scalaJSLinkerConfig in Test ~= (_.withModuleKind(ModuleKind.CommonJSModule))
  ).withScalaJSCompiler.withScalaJSJUnitPlugin.dependsOn(
      library, irProjectJS, loggingJS, jUnitRuntime % "test", testBridge % "test", jUnitAsyncJS % "test"
  )

  lazy val jsEnvs: Project = (project in file("js-envs")).settings(
      commonSettings,
      publishSettings,
      fatalWarningsSettings,
      name := "Scala.js JS Envs",
      libraryDependencies += "com.novocode" % "junit-interface" % "0.9" % "test",
      previousArtifactSetting,
      mimaBinaryIssueFilters ++= BinaryIncompatibilities.JSEnvs
  ).dependsOn(logging)

  lazy val jsEnvsTestKit: Project = (project in file("js-envs-test-kit")).settings(
      commonSettings,
      publishSettings,
      fatalWarningsSettings,
      name := "Scala.js JS Envs Test Kit",
      libraryDependencies ++= Seq(
          "com.google.jimfs" % "jimfs" % "1.1",
          "junit" % "junit" % "4.12",
          "com.novocode" % "junit-interface" % "0.9" % "test"
      ),
      previousArtifactSetting,
      mimaBinaryIssueFilters ++= BinaryIncompatibilities.JSEnvsTestKit
  ).dependsOn(jsEnvs)

  lazy val nodeJSEnv: Project = (project in file("nodejs-env")).settings(
      commonSettings,
      publishSettings,
      fatalWarningsSettings,
      name := "Scala.js Node.js env",
      normalizedName := "scalajs-nodejs-env",
      moduleName := "scalajs-env-nodejs",
      libraryDependencies ++= Seq(
          "com.google.jimfs" % "jimfs" % "1.1",
          "com.novocode" % "junit-interface" % "0.9" % "test"
      ),
      previousArtifactSetting
  ).dependsOn(jsEnvs, jsEnvsTestKit % "test")

  lazy val testAdapter = (project in file("test-adapter")).settings(
      commonSettings,
      publishSettings,
      fatalWarningsSettings,
      name := "Scala.js sbt test adapter",
      libraryDependencies += "org.scala-sbt" % "test-interface" % "1.0",
      libraryDependencies +=
        "com.novocode" % "junit-interface" % "0.11" % "test",
      previousArtifactSetting,
      mimaBinaryIssueFilters ++= BinaryIncompatibilities.TestAdapter,
      unmanagedSourceDirectories in Compile +=
        baseDirectory.value.getParentFile / "test-common/src/main/scala",
      unmanagedSourceDirectories in Test +=
        baseDirectory.value.getParentFile / "test-common/src/test/scala"
  ).dependsOn(jsEnvs)

  lazy val plugin: Project = Project(id = "sbtPlugin", base = file("sbt-plugin")).settings(
      commonSettings,
      publishIvySettings,
      fatalWarningsSettings,
      name := "Scala.js sbt plugin",
      normalizedName := "sbt-scalajs",
      bintrayProjectName := "sbt-scalajs-plugin", // "sbt-scalajs" was taken
      sbtPlugin := true,
      sbtVersion in pluginCrossBuild := {
        scalaVersion.value match {
          case v if v.startsWith("2.10.") => "0.13.17"
          case _ => "1.0.0"
        }
      },
      scalaBinaryVersion :=
        CrossVersion.binaryScalaVersion(scalaVersion.value),
      previousArtifactSetting,
      mimaBinaryIssueFilters ++= BinaryIncompatibilities.SbtPlugin,

      addSbtPlugin("org.portable-scala" % "sbt-platform-deps" % "1.0.0"),

      // Add API mappings for sbt (seems they don't export their API URL)
      apiMappings ++= {
        val deps = (externalDependencyClasspath in Compile).value

        val sbtJars = deps filter { attributed =>
          val p = attributed.data.getPath
          p.contains("/org.scala-sbt/") && p.endsWith(".jar")
        }

        val docUrl =
          url(s"http://www.scala-sbt.org/${sbtVersion.value}/api/")

        sbtJars.map(_.data -> docUrl).toMap
      }
  ).dependsOn(linker, jsEnvs, nodeJSEnv, testAdapter)

  lazy val delambdafySetting = {
    scalacOptions ++= (
        if (isGeneratingEclipse) Seq()
        else Seq("-Ydelambdafy:method"))
  }

  lazy val ensureSAMSupportSetting: Setting[_] = {
    scalacOptions ++= {
      if (scalaBinaryVersion.value == "2.11") Seq("-Xexperimental")
      else Nil
    }
  }

  private def serializeHardcodedIR(base: File,
      classDef: ir.Trees.ClassDef): File = {
    // We assume that there are no weird characters in the full name
    val fullName = ir.Definitions.decodeClassName(classDef.name.name)
    val output = base / (fullName.replace('.', '/') + ".sjsir")

    if (!output.exists()) {
      IO.createDirectory(output.getParentFile)
      val stream = new BufferedOutputStream(new FileOutputStream(output))
      try ir.Serializers.serialize(stream, classDef)
      finally stream.close()
    }
    output
  }

  lazy val javalanglib: Project = project.enablePlugins(
      MyScalaJSPlugin
  ).settings(
      commonSettings,
      fatalWarningsSettings,
      name := "java.lang library for Scala.js",
      publishArtifact in Compile := false,
      delambdafySetting,
      ensureSAMSupportSetting,
      noClassFilesSettings,

      resourceGenerators in Compile += Def.task {
        val base = (resourceManaged in Compile).value
        Seq(serializeHardcodedIR(base, JavaLangObject.TheClassDef))
      }.taskValue,
      scalaJSExternalCompileSettings
  ).withScalaJSCompiler.dependsOnLibraryNoJar

  lazy val javalib: Project = project.enablePlugins(
      MyScalaJSPlugin
  ).settings(
      commonSettings,
      fatalWarningsSettings,
      name := "Java library for Scala.js",
      publishArtifact in Compile := false,
      delambdafySetting,
      ensureSAMSupportSetting,
      noClassFilesSettings,
      scalaJSExternalCompileSettings,

      headerSources in Compile ~= { srcs =>
        srcs.filter { src =>
          val path = src.getPath.replace('\\', '/')
          !path.contains("/java/math/") &&
          !path.endsWith("/java/util/concurrent/ThreadLocalRandom.scala")
        }
      }
  ).withScalaJSCompiler.dependsOnLibraryNoJar

  lazy val scalalib: Project = project.enablePlugins(
      MyScalaJSPlugin
  ).settings(
      commonSettings,
      /* Link source maps to the GitHub sources of the original scalalib
       * #2195 This must come *before* the option added by MyScalaJSPlugin
       * because mapSourceURI works on a first-match basis.
       */
      scalacOptions := {
        val previousScalacOptions = scalacOptions.value
        val sourceMapOption = {
          "-P:scalajs:mapSourceURI:" +
          (artifactPath in fetchScalaSource).value.toURI +
          "->https://raw.githubusercontent.com/scala/scala/v" +
          scalaVersion.value + "/src/library/"
        }
        sourceMapOption +: previousScalacOptions
      },
      name := "Scala library for Scala.js",
      publishArtifact in Compile := false,
      delambdafySetting,
      noClassFilesSettings,

      // The Scala lib is full of warnings we don't want to see
      scalacOptions ~= (_.filterNot(
          Set("-deprecation", "-unchecked", "-feature") contains _)),

      // Tell the plugin to hack-fix bad classOf trees
      scalacOptions += "-P:scalajs:fixClassOf",

      libraryDependencies +=
        "org.scala-lang" % "scala-library" % scalaVersion.value classifier "sources",

      artifactPath in fetchScalaSource :=
        target.value / "scalaSources" / scalaVersion.value,

      /* Work around for #2649. We would like to always use `update`, but
       * that fails if the scalaVersion we're looking for happens to be the
       * version of Scala used by sbt itself. This is clearly a bug in sbt,
       * which we work around here by using `updateClassifiers` instead in
       * that case.
       */
      update in fetchScalaSource := Def.taskDyn {
        if (scalaVersion.value == scala.util.Properties.versionNumberString)
          updateClassifiers
        else
          update
      }.value,

      fetchScalaSource := {
        val s = streams.value
        val cacheDir = s.cacheDirectory
        val ver = scalaVersion.value
        val trgDir = (artifactPath in fetchScalaSource).value

        val report = (update in fetchScalaSource).value
        val scalaLibSourcesJar = report.select(
            configuration = Set("compile"),
            module = moduleFilter(name = "scala-library"),
            artifact = artifactFilter(classifier = "sources")).headOption.getOrElse {
          throw new Exception(
              s"Could not fetch scala-library sources for version $ver")
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
        // - override-2.11.0-RC1
        // - override-2.11.0
        // - override-2.11
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

      headerSources in Compile := Nil,
      headerSources in Test := Nil,

      scalaJSExternalCompileSettings
  ).withScalaJSCompiler.dependsOnLibraryNoJar

  lazy val libraryAux: Project = (project in file("library-aux")).enablePlugins(
      MyScalaJSPlugin
  ).settings(
      commonSettings,
      fatalWarningsSettings,
      name := "Scala.js aux library",
      publishArtifact in Compile := false,
      delambdafySetting,
      noClassFilesSettings,
      scalaJSExternalCompileSettings
  ).withScalaJSCompiler.dependsOnLibraryNoJar

  lazy val library: Project = project.enablePlugins(
      MyScalaJSPlugin
  ).settings(
      commonSettings,
      publishSettings,
      fatalWarningsSettings,
      name := "Scala.js library",
      delambdafySetting,
      ensureSAMSupportSetting,
      exportJars := !isGeneratingEclipse,
      previousArtifactSetting,
      mimaBinaryIssueFilters ++= BinaryIncompatibilities.Library,

      scalaJSExternalCompileSettings,

      test in Test := {
        streams.value.log.warn("Skipping library/test. Run testSuite/test to test library.")
      },

      inConfig(Compile)(Seq(
          scalacOptions in doc ++= Seq(
              "-implicits",
              "-groups",
              "-doc-title", "Scala.js",
              "-doc-version", scalaJSVersion
          ),

          unmanagedSourceDirectories +=
            collectionsEraDependentDirectory(scalaVersion.value, sourceDirectory.value),

          // Filter doc sources to remove implementation details from doc.
          sources in doc := {
            val prev = (sources in doc).value
            val javaV = javaVersion.value
            val scalaV = scalaVersion.value

            /* On Java 9+, Scaladoc will crash with "bad constant pool tag 20"
             * until version 2.12.1 included. The problem seems to have been
             * fixed in 2.12.2, perhaps through
             * https://github.com/scala/scala/pull/5711.
             * See also #3152.
             */
            val mustAvoidJavaDoc = {
              javaV >= 9 && {
                scalaV.startsWith("2.10.") ||
                scalaV.startsWith("2.11.") ||
                scalaV == "2.12.0" ||
                scalaV == "2.12.1"
              }
            }

            if (!mustAvoidJavaDoc) {
              def containsFileFilter(s: String): FileFilter = new FileFilter {
                override def accept(f: File): Boolean = {
                  val path = f.getAbsolutePath.replace('\\', '/')
                  path.contains(s)
                }
              }

              val filter: FileFilter = (
                  AllPassFilter
                    -- containsFileFilter("/scala/scalajs/runtime/")
                    -- containsFileFilter("/scala/scalajs/js/annotation/internal/")
                    -- "*.nodoc.scala"
              )

              (sources in doc).value.filter(filter.accept)
            } else {
              Nil
            }
          },

          /* Add compiled .class files to doc dependencyClasspath, so we can
           * still compile even with only part of the files being present.
           */
          dependencyClasspath in doc ++= exportedProducts.value,

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

            val filter = ("*.sjsir": NameFilter)

            val otherProducts = (
                (products in LocalProject("javalanglib")).value ++
                (products in LocalProject("javalib")).value ++
                (products in LocalProject("scalalib")).value ++
                (products in LocalProject("libraryAux")).value)
            val otherMappings =
              otherProducts.flatMap(base => Path.selectSubpaths(base, filter))

            libraryMappings ++ otherMappings
          }
      ))
  ).withScalaJSCompiler

  lazy val minilib: Project = project.enablePlugins(
      MyScalaJSPlugin
  ).settings(
      commonSettings,
      fatalWarningsSettings,
      name := "scalajs-minilib",

      noClassFilesSettings,
      scalaJSExternalCompileSettings,
      inConfig(Compile)(Seq(
          mappings in packageBin := {
            val superMappings = (mappings in packageBin).value
            val libraryMappings = (mappings in (library, packageBin)).value

            val whitelisted = libraryMappings.filter { mapping =>
              MiniLib.Whitelist.contains(mapping._2.replace('\\', '/'))
            }

            whitelisted ++ superMappings
          }
      ))
  ).withScalaJSCompiler.dependsOn(library)

  // The Scala.js version of sbt-testing-interface
  lazy val testInterface = (project in file("test-interface")).enablePlugins(
      MyScalaJSPlugin
  ).settings(
      commonSettings,
      publishSettings,
      fatalWarningsSettings,
      name := "Scala.js test interface",
      delambdafySetting,
      previousArtifactSetting,
      mimaBinaryIssueFilters ++= BinaryIncompatibilities.TestInterface
  ).withScalaJSCompiler.dependsOn(library)

  lazy val testBridge = (project in file("test-bridge")).enablePlugins(
      MyScalaJSPlugin
  ).settings(
      commonSettings,
      publishSettings,
      fatalWarningsSettings,
      name := "Scala.js test bridge",
      delambdafySetting,
      /* By design, the test-bridge has a completely private API (it is
       * only loaded through a privately-known top-level export), so it
       * does not have `previousArtifactSetting` nor
       * `mimaBinaryIssueFilters`.
       */
      unmanagedSourceDirectories in Compile +=
        baseDirectory.value.getParentFile / "test-common/src/main/scala"
      /* Note: We cannot add the test-common tests, since they test async
       * stuff and JUnit does not support async tests. Therefore we need to
       * block, so we cannot run on JS.
       */
  ).withScalaJSCompiler.dependsOn(library, testInterface)

  lazy val jUnitRuntime = (project in file("junit-runtime")).enablePlugins(
      MyScalaJSPlugin
  ).settings(
      commonSettings,
      publishSettings,
      fatalWarningsSettings,
      name := "Scala.js JUnit test runtime",

      headerSources in Compile ~= { srcs =>
        srcs.filter { src =>
          val path = src.getPath.replace('\\', '/')
          !path.contains("/org/junit/") && !path.contains("/org/hamcrest/")
        }
      }
  ).withScalaJSCompiler.dependsOn(testInterface)

  val commonJUnitTestOutputsSettings = Def.settings(
      commonSettings,
      publishArtifact in Compile := false,
      parallelExecution in Test := false,
      unmanagedSourceDirectories in Test +=
        baseDirectory.value.getParentFile / "shared/src/test/scala",
      testOptions in Test ++= Seq(
          Tests.Argument(TestFrameworks.JUnit, "-a", "-s"),
          Tests.Filter(_.endsWith("Assertions"))
      )
  )

  lazy val jUnitTestOutputsJS = (project in file("junit-test/output-js")).enablePlugins(
      MyScalaJSPlugin
  ).settings(
      commonJUnitTestOutputsSettings,
      name := "Tests for Scala.js JUnit output in JS."
  ).withScalaJSCompiler.withScalaJSJUnitPlugin.dependsOn(
      jUnitRuntime % "test", testBridge % "test", jUnitAsyncJS % "test"
  )


  lazy val jUnitTestOutputsJVM = (project in file("junit-test/output-jvm")).settings(
      commonJUnitTestOutputsSettings,
      name := "Tests for Scala.js JUnit output in JVM.",
      libraryDependencies ++= Seq(
          "org.scala-sbt" % "test-interface" % "1.0" % "test",
          "com.novocode" % "junit-interface" % "0.11" % "test"
      )
  ).dependsOn(
       jUnitAsyncJVM % "test"
  )

  lazy val jUnitPlugin = (project in file("junit-plugin")).settings(
      commonSettings,
      publishSettings,
      fatalWarningsSettings,
      name := "Scala.js JUnit test plugin",
      crossVersion := CrossVersion.full,
      libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value,
      exportJars := true
  )

  lazy val jUnitAsyncJS = (project in file("junit-async/js")).enablePlugins(
      MyScalaJSPlugin
  ).withScalaJSCompiler.settings(
      commonSettings,
      name := "Scala.js internal JUnit async JS support",
      publishArtifact in Compile := false
  ).dependsOn(library)

  lazy val jUnitAsyncJVM = (project in file("junit-async/jvm")).settings(
      commonSettings,
      name := "Scala.js internal JUnit async JVM support",
      publishArtifact in Compile := false
  )

  // Examples

  lazy val examples: Project = project.settings(
      commonSettings,
      name := "Scala.js examples"
  ).aggregate(helloworld, reversi, testingExample)

  lazy val exampleSettings = commonSettings ++ fatalWarningsSettings ++ Def.settings(
      headerSources in Compile := Nil,
      headerSources in Test := Nil
  )

  lazy val helloworld: Project = (project in (file("examples") / "helloworld")).enablePlugins(
      MyScalaJSPlugin
  ).settings(
      exampleSettings,
      name := "Hello World - Scala.js example",
      moduleName := "helloworld",
      scalaJSUseMainModuleInitializer := true
  ).withScalaJSCompiler.dependsOn(library)

  lazy val reversi = (project in (file("examples") / "reversi")).enablePlugins(
      MyScalaJSPlugin
  ).settings(
      exampleSettings,
      name := "Reversi - Scala.js example",
      moduleName := "reversi"
  ).withScalaJSCompiler.dependsOn(library)

  lazy val testingExample = (project in (file("examples") / "testing")).enablePlugins(
      MyScalaJSPlugin
  ).settings(
      exampleSettings,
      name := "Testing - Scala.js example",
      moduleName := "testing",

      test in Test := {
        throw new MessageOnlyException(
            "testingExample/test is not supported because it requires DOM " +
            "support. Use testingExample/testHtml instead.")
      }
  ).withScalaJSCompiler.withScalaJSJUnitPlugin.dependsOn(
      library, jUnitRuntime % "test", testBridge % "test"
  )

  // Testing

  def testSuiteCommonSettings(isJSTest: Boolean): Seq[Setting[_]] = Seq(
      publishArtifact in Compile := false,
      scalacOptions ~= (_.filter(_ != "-deprecation")),

      // Need reflect for typechecking macros
      libraryDependencies +=
        "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided",

      testOptions += Tests.Argument(TestFrameworks.JUnit, "-a", "-s"),

      unmanagedSourceDirectories in Test ++= {
        val testDir = (sourceDirectory in Test).value
        val sharedTestDir =
          testDir.getParentFile.getParentFile.getParentFile / "shared/src/test"

        val scalaV = scalaVersion.value
        val isScalaAtLeast212 = !scalaV.startsWith("2.11.")

        List(sharedTestDir / "scala", sharedTestDir / "require-jdk7",
            sharedTestDir / "require-jdk8") ++
        includeIf(testDir / "require-2.12", isJSTest && isScalaAtLeast212)
      },

      sources in Test ++= {
        val supportsSAM = scalaBinaryVersion.value match {
          case "2.11" => scalacOptions.value.contains("-Xexperimental")
          case _      => true
        }

        val scalaV = scalaVersion.value

        /* Can't add require-sam as unmanagedSourceDirectories because of the
         * use of scalacOptions. Hence sources are added individually.
         * Note that a testSuite/test will not trigger a compile when sources
         * are modified in require-sam
         */
        if (supportsSAM) {
          val testDir = (sourceDirectory in Test).value
          val sharedTestDir =
            testDir.getParentFile.getParentFile.getParentFile / "shared/src/test"

          val allSAMSources = {
            ((sharedTestDir / "require-sam") ** "*.scala").get ++
            (if (isJSTest) ((testDir / "require-sam") ** "*.scala").get else Nil)
          }

          val hasBugWithOverriddenMethods =
            Set("2.12.0", "2.12.1", "2.12.2", "2.12.3", "2.12.4").contains(scalaV)

          if (hasBugWithOverriddenMethods)
            allSAMSources.filter(_.getName != "SAMWithOverridingBridgesTest.scala")
          else
            allSAMSources
        } else {
          Nil
        }
      }
  )

  def testSuiteBootstrapSetting = Def.settings(
      Defaults.testSettings,
      ScalaJSPlugin.testConfigSettings,

      fullOptJS := {
        throw new MessageOnlyException("fullOptJS is not supported in Bootstrap")
      },

      fastOptJS := {
        val s = streams.value

        val out = (artifactPath in fastOptJS).value

        val linkerModule =
          (scalaJSLinkedFile in (testSuiteLinker, Compile)).value.data

        val cp = Attributed.data(fullClasspath.value)
        val cpFiles = (scalaJSIR in fastOptJS).value.get(scalaJSSourceFiles).get

        FileFunction.cached(s.cacheDirectory, FilesInfo.lastModified,
            FilesInfo.exists) { _ =>

          val cpPaths = cp
            .map(f => "\"" + escapeJS(f.getAbsolutePath) + "\"")
            .mkString("[", ", ", "]")

          val code = {
            s"""
              var toolsTestModule = require("${escapeJS(linkerModule.getPath)}");
              var linker = toolsTestModule.TestSuiteLinker;
              var result =
                linker.linkTestSuiteNode($cpPaths, "${escapeJS(out.getAbsolutePath)}");

              result.catch(e => {
                console.error(e);
                process.exit(1);
              });
            """
          }

          val launcherFile = crossTarget.value / "test-suite-linker.js"
          IO.write(launcherFile, code)

          val config = RunConfig().withLogger(sbtLogger2ToolsLogger(s.log))
          val input = Input.ScriptsToLoad(List(launcherFile.toPath))

          s.log.info(s"Linking test suite with JS linker")

          val jsEnv = new NodeJSEnv(
            NodeJSEnv.Config()
              .withArgs(List("--max_old_space_size=3072"))
              .withSourceMap(false))

          val run = jsEnv.start(input, config)
          Await.result(run.future, Duration.Inf)
          Set(out)
        } ((cpFiles :+ linkerModule).toSet)

        Attributed.blank(out)
      },

      compile := (compile in Test).value,
      fullClasspath := (fullClasspath in Test).value,
      testSuiteJSExecutionFilesSetting
  )

  def testSuiteJSExecutionFilesSetting: Setting[_] = {
    jsEnvInput := {
      val resourceDir = (resourceDirectory in Test).value
      val f = (resourceDir / "NonNativeJSTypeTestNatives.js").toPath

      jsEnvInput.value match {
        case Input.ScriptsToLoad(prevFiles) =>
          Input.ScriptsToLoad(f :: prevFiles)
        case Input.ESModulesToLoad(prevFiles) =>
          Input.ESModulesToLoad(f :: prevFiles)
        case Input.CommonJSModulesToLoad(prevFiles) =>
          Input.CommonJSModulesToLoad(f :: prevFiles)
      }
    }
  }

  lazy val Bootstrap = config("bootstrap")
    .describedAs("Configuration that uses a JS linker instead of the JVM")

  lazy val testSuite: Project = (project in file("test-suite/js")).enablePlugins(
      MyScalaJSPlugin
  ).configs(Bootstrap).settings(
      commonSettings,
      inConfig(Test)(testSuiteJSExecutionFilesSetting),
      testSuiteCommonSettings(isJSTest = true),
      name := "Scala.js test suite",

      unmanagedSourceDirectories in Test ++= {
        val testDir = (sourceDirectory in Test).value
        val scalaV = scalaVersion.value

        val moduleKind = scalaJSLinkerConfig.value.moduleKind

        collectionsEraDependentDirectory(scalaV, testDir) ::
        includeIf(testDir / "require-modules",
            moduleKind != ModuleKind.NoModule) :::
        includeIf(testDir / "require-dynamic-import",
            moduleKind == ModuleKind.ESModule) // this is an approximation that works for now
      },

      scalaJSLinkerConfig ~= { _.withSemantics(TestSuiteLinkerOptions.semantics _) },
      scalaJSModuleInitializers in Test ++= TestSuiteLinkerOptions.moduleInitializers,

      /* The script that calls setExportsNamespaceForExportsTest to provide
       * ExportsTest with a loopback reference to its own exports namespace.
       * Only when using an ES module.
       * See the comment in ExportsTest for more details.
       */
      setModuleLoopbackScript in Test := Def.settingDyn[Task[Option[java.nio.file.Path]]] {
        (scalaJSLinkerConfig in Test).value.moduleKind match {
          case ModuleKind.ESModule =>
            Def.task {
              val linkedFile = (scalaJSLinkedFile in Test).value.data
              val uri = linkedFile.toURI.toASCIIString

              val ext = {
                val name = linkedFile.getName
                val dotPos = name.lastIndexOf('.')
                if (dotPos < 0) ".js" else name.substring(dotPos)
              }

              val setNamespaceScriptFile =
                crossTarget.value / (linkedFile.getName + "-loopback" + ext)

              /* Due to the asynchronous nature of ES module loading, there
               * exists a theoretical risk for a race condition here. It is
               * possible that tests will start running and reaching the
               * ExportsTest before this module is executed. It's quite
               * unlikely, though, given all the message passing for the com
               * and all that.
               */
              IO.write(setNamespaceScriptFile,
                  s"""
                    |import * as mod from "${escapeJS(uri)}";
                    |mod.setExportsNamespaceForExportsTest(mod);
                  """.stripMargin)

              Some(setNamespaceScriptFile.toPath)
            }

          case _ =>
            Def.task {
              None
            }
        }
      }.value,

      jsEnvInput in Test := {
        val prev = (jsEnvInput in Test).value
        val loopbackScript = (setModuleLoopbackScript in Test).value

        loopbackScript match {
          case None =>
            prev
          case Some(script) =>
            val Input.ESModulesToLoad(modules) = prev
            Input.ESModulesToLoad(modules :+ script)
        }
      },

      sourceGenerators in Compile += Def.task {
        val stage = scalaJSStage.value

        val linkerConfig = stage match {
          case FastOptStage => (scalaJSLinkerConfig in (Compile, fastOptJS)).value
          case FullOptStage => (scalaJSLinkerConfig in (Compile, fullOptJS)).value
        }

        val moduleKind = linkerConfig.moduleKind
        val sems = linkerConfig.semantics

        ConstantHolderGenerator.generate(
            (sourceManaged in Compile).value,
            "org.scalajs.testsuite.utils.BuildInfo",
            "scalaVersion" -> scalaVersion.value,
            "hasSourceMaps" -> MyScalaJSPlugin.wantSourceMaps.value,
            "isNoModule" -> (moduleKind == ModuleKind.NoModule),
            "isESModule" -> (moduleKind == ModuleKind.ESModule),
            "isCommonJSModule" -> (moduleKind == ModuleKind.CommonJSModule),
            "isFullOpt" -> (stage == Stage.FullOpt),
            "compliantAsInstanceOfs" -> (sems.asInstanceOfs == CheckedBehavior.Compliant),
            "compliantArrayIndexOutOfBounds" -> (sems.arrayIndexOutOfBounds == CheckedBehavior.Compliant),
            "compliantModuleInit" -> (sems.moduleInit == CheckedBehavior.Compliant),
            "strictFloats" -> sems.strictFloats,
            "productionMode" -> sems.productionMode,
            "es2015" -> linkerConfig.esFeatures.useECMAScript2015
        )
      }.taskValue,

      /* Generate a scala source file that throws exceptions in
       * various places (while attaching the source line to the
       * exception). When we catch the exception, we can then
       * compare the attached source line and the source line
       * calculated via the source maps.
       *
       * see test-suite/src/test/resources/SourceMapTestTemplate.scala
       */
      sourceGenerators in Test += Def.task {
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
        val unitTests =
          (0 until i).map(i => s"@Test def workTest$i(): Unit = test($i)").mkString("; ")
        IO.write(outFile,
            replaced.replace("@Test def workTest(): Unit = ???", unitTests))
        Seq(outFile)
      }.taskValue,

      /* Blacklist LongTest.scala in FullOpt, because it generates so much
       * code, through optimizer-based generative programming, that Closure
       * loses it on that code.
       */
      sources in Test := {
        val prev = (sources in Test).value
        scalaJSStage.value match {
          case FastOptStage =>
            prev
          case FullOptStage =>
            prev.filter(!_.getPath.replace('\\', '/').endsWith("compiler/LongTest.scala"))
        }
      },

      /* Because of the above tweak of `sources` depending on the value of
       * `scalaJSStage`, it is ill-advised to invoke a linking task that does
       * not correspond to the current `scalaJSStage`.
       */
      for ((key, stage) <- Seq(fastOptJS -> FastOptStage, fullOptJS -> FullOptStage)) yield {
        key in Test := {
          /* Note that due to the way dependencies between tasks work, the
           * actual linking *will* be computed anyway, but it's not too late to
           * prevent the user from doing anything meaningful with it
           * afterwards.
           */
          val actual = (key in Test).value
          if (scalaJSStage.value != stage) {
            throw new MessageOnlyException(
                s"testSuite/test:${key.key} can only be invoked when " +
                s"(scalaJSStage in testSuite).value is $stage")
          }
          actual
        }
      },

      inConfig(Bootstrap)(testSuiteBootstrapSetting)
  ).withScalaJSCompiler.withScalaJSJUnitPlugin.dependsOn(
      library, jUnitRuntime, testBridge % "test", jUnitAsyncJS % "test"
  )

  lazy val testSuiteJVM: Project = (project in file("test-suite/jvm")).settings(
      commonSettings,
      testSuiteCommonSettings(isJSTest = false),
      name := "Scala.js test suite on JVM",

      /* Scala.js always assumes en-US, UTF-8 and NL as line separator by
       * default. Since some of our tests rely on these defaults (notably to
       * test them), we have to force the same values on the JVM.
       */
      fork in Test := true,
      javaOptions in Test ++= Seq(
          "-Dfile.encoding=UTF-8",
          "-Duser.country=US", "-Duser.language=en",
          "-Dline.separator=\n"
      ),

      libraryDependencies +=
        "com.novocode" % "junit-interface" % "0.11" % "test"
  )

  /* Additional test suite, for tests that should not be part of the normal
   * test suite for various reasons. The most common reason is that the tests
   * in there "fail to fail" if they happen in the larger test suite, due to
   * all the other code that's there (can have impact on dce, optimizations,
   * GCC, etc.).
   *
   * TODO Ideally, we should have a mechanism to separately compile, link and
   * test each file in this test suite, so that we're sure that do not
   * interfere with other.
   */
  lazy val testSuiteEx: Project = (project in file("test-suite-ex")).enablePlugins(
      MyScalaJSPlugin
  ).settings(
      commonSettings,
      name := "Scala.js test suite ex",
      publishArtifact in Compile := false,
      testOptions += Tests.Argument(TestFrameworks.JUnit, "-a", "-s"),
      scalacOptions in Test ~= (_.filter(_ != "-deprecation"))
  ).withScalaJSCompiler.withScalaJSJUnitPlugin.dependsOn(
      library, jUnitRuntime, testBridge % "test", testSuite
  )

  lazy val testSuiteLinker = (project in file("test-suite-linker")).enablePlugins(
      MyScalaJSPlugin
  ).settings(
      exampleSettings,
      name := "Scala.js test suite linker",
      scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule)),
      sources in Compile +=
        baseDirectory.value.getParentFile / "project/TestSuiteLinkerOptions.scala"
  ).withScalaJSCompiler.dependsOn(linkerJS)

  lazy val partest: Project = project.settings(
      commonSettings,
      fatalWarningsSettings,
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
        if (shouldPartest.value) {
          Seq(
              "org.scala-sbt" % "sbt" % sbtVersion.value,
              {
                val v = scalaVersion.value
                if (v == "2.11.0" || v == "2.11.1" || v == "2.11.2")
                  "org.scala-lang.modules" %% "scala-partest" % "1.0.13"
                else if (v.startsWith("2.11."))
                  "org.scala-lang.modules" %% "scala-partest" % "1.0.16"
                else
                  "org.scala-lang.modules" %% "scala-partest" % "1.1.4"
              }
          )
        } else {
          Seq()
        }
      },

      unmanagedSourceDirectories in Compile += {
        val sourceRoot = (sourceDirectory in Compile).value.getParentFile
        val v = scalaVersion.value
        if (v == "2.11.0" || v == "2.11.1" || v == "2.11.2")
          sourceRoot / "main-partest-1.0.13"
        else
          sourceRoot / "main-partest-1.0.16"
      },

      sources in Compile := {
        if (shouldPartest.value)
          (sources in Compile).value
        else
          Nil
      }
  ).dependsOn(compiler, linker, nodeJSEnv)

  lazy val partestSuite: Project = (project in file("partest-suite")).settings(
      commonSettings,
      fatalWarningsSettings,
      name := "Scala.js partest suite",

      fork in Test := true,
      javaOptions in Test += "-Xmx1G",

      // Override the dependency of partest - see #1889
      dependencyOverrides += "org.scala-lang" % "scala-library" % scalaVersion.value % "test",

      testFrameworks ++= {
        if (shouldPartest.value)
          Seq(new TestFramework("scala.tools.partest.scalajs.Framework"))
        else Seq()
      },

      definedTests in Test ++= Def.taskDyn[Seq[sbt.TestDefinition]] {
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
      }.value
  ).dependsOn(partest % "test", library)

  lazy val scalaTestSuite: Project = (project in file("scala-test-suite")).enablePlugins(
      MyScalaJSPlugin
  ).settings(
      commonSettings,
      publishArtifact in Compile := false,

      testOptions += Tests.Argument(TestFrameworks.JUnit, "-a", "-s"),

      unmanagedSources in Test ++= {
        def loadList(listName: String): Set[String] = {
          val listsDir = (resourceDirectory in Test).value / scalaVersion.value
          val buff = scala.io.Source.fromFile(listsDir / listName)
          val lines = buff.getLines().collect {
            case line if !line.startsWith("#") && line.nonEmpty => line
          }.toSeq
          val linesSet = lines.toSet
          if (linesSet.size != lines.size) {
            val msg = listName + " contains contains duplicates: " +
                lines.diff(linesSet.toSeq).toSet
            throw new AssertionError(msg.toString)
          }
          linesSet
        }

        val whitelist: Set[String] = loadList("WhitelistedTests.txt")
        val blacklist: Set[String] = loadList("BlacklistedTests.txt")

        val jUnitTestsPath =
          (fetchScalaSource in partest).value / "test" / "junit"

        val scalaScalaJUnitSources = {
          (jUnitTestsPath ** "*.scala").get.flatMap { file =>
            file.relativeTo(jUnitTestsPath) match {
              case Some(rel) => List((rel.toString.replace('\\', '/'), file))
              case None      => Nil
            }
          }
        }

        // Check the coherence of the lists against the files found.
        val allClasses = scalaScalaJUnitSources.map(_._1).toSet
        val inBothLists = blacklist.intersect(whitelist)
        val allListed = blacklist.union(whitelist)
        val inNoList = allClasses.diff(allListed)
        val nonexistentBlacklisted = blacklist.diff(allClasses)
        val nonexistentWhitelisted = whitelist.diff(allClasses)
        if (inBothLists.nonEmpty || inNoList.nonEmpty ||
            nonexistentBlacklisted.nonEmpty || nonexistentWhitelisted.nonEmpty) {
          val msg = new StringBuffer("Errors in black or white lists.\n")
          if (inBothLists.nonEmpty) {
            msg.append("Sources listed both in black and white list: ")
            msg.append(inBothLists).append('\n')
          }
          if (inNoList.nonEmpty) {
            msg.append("Sources not listed in back or white list: ")
            msg.append(inNoList).append('\n')
          }
          if (nonexistentBlacklisted.nonEmpty) {
            msg.append("Sources not found for blacklisted tests: ")
            msg.append(nonexistentBlacklisted).append('\n')
          }
          if (nonexistentWhitelisted.nonEmpty) {
            msg.append("Sources not found for whitelisted tests: ")
            msg.append(nonexistentWhitelisted).append('\n')
          }
          throw new AssertionError(msg.toString)
        }

        scalaScalaJUnitSources.collect {
          case fTup if whitelist(fTup._1) => fTup._2
        }
      }
  ).withScalaJSCompiler.withScalaJSJUnitPlugin.dependsOn(jUnitRuntime, testBridge % "test")

}
