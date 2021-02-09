package build

import scala.language.implicitConversions

import scala.annotation.tailrec

import sbt._
import Keys._

import com.typesafe.tools.mima.plugin.MimaPlugin.autoImport._
import de.heikoseeberger.sbtheader.HeaderPlugin.autoImport._
import ScriptedPlugin.autoImport._

import java.util.Arrays

import scala.collection.immutable.Range
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
import org.scalastyle.sbt.ScalastylePlugin.autoImport.scalastyle
import ExternalCompile.scalaJSExternalCompileSettings
import Loggers._

import org.scalajs.linker.interface._

/* Things that we want to expose in the sbt command line (and hence also in
 * `ci/matrix.xml`).
 */
object ExposedValues extends AutoPlugin {
  object autoImport {
    // set scalaJSLinkerConfig in someProject ~= makeCompliant
    val makeCompliant: StandardConfig => StandardConfig = {
      _.withSemantics { semantics =>
        semantics
          .withAsInstanceOfs(CheckedBehavior.Compliant)
          .withArrayIndexOutOfBounds(CheckedBehavior.Compliant)
          .withModuleInit(CheckedBehavior.Compliant)
          .withStrictFloats(true)
      }
    }

    val CheckedBehavior = org.scalajs.linker.interface.CheckedBehavior

    val ModuleSplitStyle = org.scalajs.linker.interface.ModuleSplitStyle

    type NodeJSEnvForcePolyfills = build.NodeJSEnvForcePolyfills
  }
}

final case class ExpectedSizes(fastLink: Range, fullLink: Range,
    fastLinkGz: Range, fullLinkGz: Range)

object MyScalaJSPlugin extends AutoPlugin {
  override def requires: Plugins = ScalaJSPlugin

  val isGeneratingForIDE = {
    Properties.envOrElse("GENERATING_ECLIPSE", "false").toBoolean ||
    Properties.envOrElse("METALS_ENABLED", "false").toBoolean
  }

  val wantSourceMaps = settingKey[Boolean]("Whether source maps should be used")

  val testHtmlJSDom = taskKey[Unit]("Run testHtml through JSDom")

  val writePackageJSON = taskKey[Unit](
      "Write package.json to configure module type for Node.js")

  val checksizes = taskKey[Unit]("Check expected output sizes")

  val expectedSizes = settingKey[Option[ExpectedSizes]]("Expected sizes for checksizes")

  def addScalaJSCompilerOption(option: String): Setting[_] =
    addScalaJSCompilerOption(Def.setting(option))

  def addScalaJSCompilerOption(option: Def.Initialize[String]): Setting[_] =
    addScalaJSCompilerOption(None, option)

  def addScalaJSCompilerOptionInConfig(config: Configuration,
      option: String): Setting[_] = {
    addScalaJSCompilerOption(Some(config), Def.setting(option))
  }

  def addScalaJSCompilerOption(config: Option[Configuration],
      option: Def.Initialize[String]): Setting[_] = {
    config.fold(scalacOptions)(scalacOptions in _) ++= {
      val o = option.value
      if (isGeneratingForIDE) Nil
      else Seq(s"-P:scalajs:$o")
    }
  }

  override def globalSettings: Seq[Setting[_]] = Def.settings(
      fullClasspath in scalaJSLinkerImpl := {
        (fullClasspath in (Build.linker.v2_12, Runtime)).value
      },
  )

  override def projectSettings: Seq[Setting[_]] = Def.settings(
      /* Remove libraryDependencies on ourselves; we use .dependsOn() instead
       * inside this build.
       */
      libraryDependencies ~= { libDeps =>
        val blacklist =
          Set("scalajs-compiler", "scalajs-library", "scalajs-test-bridge")
        libDeps.filterNot(dep => blacklist.contains(dep.name))
      },

      scalaJSLinkerConfig ~= (_.withCheckIR(true)),

      wantSourceMaps := true,

      jsEnv := new NodeJSEnv(
          NodeJSEnv.Config().withSourceMap(wantSourceMaps.value)),

      jsEnvInput in Compile :=
        (jsEnvInput in Compile).dependsOn(writePackageJSON).value,

      jsEnvInput in Test :=
        (jsEnvInput in Test).dependsOn(writePackageJSON).value,

      writePackageJSON := {
        val packageType = scalaJSLinkerConfig.value.moduleKind match {
          case ModuleKind.NoModule       => "commonjs"
          case ModuleKind.CommonJSModule => "commonjs"
          case ModuleKind.ESModule       => "module"
        }

        val path = target.value / "package.json"

        IO.write(path, s"""{"type": "$packageType"}\n""")
      },

      expectedSizes := None,

      checksizes := {
        val logger = streams.value.log

        val maybeExpected = expectedSizes.value

        /* The deprecated tasks do exactly what we want in terms of module /
         * file resolution. So we use them instead of building it again.
         */
        val fast = (fastOptJS in Compile).value.data
        val full = (fullOptJS in Compile).value.data

        val desc = s"${thisProject.value.id} Scala ${scalaVersion.value}"

        maybeExpected.fold {
          logger.info(s"Ignoring checksizes for " + desc)
        } { expected =>
          val fastGz = new File(fast.getPath() + ".gz")
          val fullGz = new File(full.getPath() + ".gz")

          IO.gzip(fast, fastGz)
          IO.gzip(full, fullGz)

          val fastSize = fast.length()
          val fullSize = full.length()
          val fastGzSize = fastGz.length()
          val fullGzSize = fullGz.length()

          logger.info(s"Checksizes: $desc")
          logger.info(s"fastLink size = $fastSize (expected ${expected.fastLink})")
          logger.info(s"fullLink size = $fullSize (expected ${expected.fullLink})")
          logger.info(s"fastLink gzip size = $fastGzSize (expected ${expected.fastLinkGz})")
          logger.info(s"fullLink gzip size = $fullGzSize (expected ${expected.fullLinkGz})")

          val ok = (
              expected.fastLink.contains(fastSize) &&
              expected.fullLink.contains(fullSize) &&
              expected.fastLinkGz.contains(fastGzSize) &&
              expected.fullLinkGz.contains(fullGzSize)
          )

          if (!ok)
            throw new MessageOnlyException("checksizes failed")
        }
      },

      // Link source maps to GitHub sources
      if (scalaJSVersion.endsWith("-SNAPSHOT")) {
        Nil
      } else {
        addScalaJSCompilerOption(Def.setting {
          "mapSourceURI:" +
          (baseDirectory in LocalProject("scalajs")).value.toURI +
          "->https://raw.githubusercontent.com/scala-js/scala-js/v" +
          scalaJSVersion + "/"
        })
      },

      testHtmlJSDom in Test := {
        val target = crossTarget.value.toPath().toAbsolutePath()

        // When serving `target` over HTTP, the path of the runner file.
        val runnerPath = {
          val runner = (testHtml in Test).value.data.toPath().toAbsolutePath()
          target.relativize(runner).toString()
        }

        val code = new ProcessBuilder(
            "node", "scripts/test-html.js", target.toString(), runnerPath)
          .inheritIO()
          .start()
          .waitFor()

        if (code != 0)
          throw new MessageOnlyException("testHtmlJSDom failed")
      }
  )
}

object Build {
  import MyScalaJSPlugin.{addScalaJSCompilerOption, addScalaJSCompilerOptionInConfig, isGeneratingForIDE}

  val scalastyleCheck = taskKey[Unit]("Run scalastyle")

  val fetchScalaSource = taskKey[File](
    "Fetches the scala source for the current scala version")
  val shouldPartest = settingKey[Boolean](
    "Whether we should partest the current scala version (and fail if we can't)")

  val packageMinilib = taskKey[File]("Produces the minilib jar.")

  val previousVersions = List("1.0.0", "1.0.1", "1.1.0", "1.1.1",
      "1.2.0", "1.3.0", "1.3.1", "1.4.0")
  val previousVersion = previousVersions.last

  val previousBinaryCrossVersion = CrossVersion.binaryWith("sjs1_", "")

  val scalaVersionsUsedForPublishing: Set[String] =
    Set("2.11.12", "2.12.12", "2.13.4")
  val newScalaBinaryVersionsInThisRelease: Set[String] =
    Set()

  def hasNewCollections(version: String): Boolean = {
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
        Set(prevProjectID)
      }
    }
  }

  val commonSettings = Seq(
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

      scalacOptions ++= Seq(
          "-deprecation",
          "-unchecked",
          "-feature",
          "-encoding", "utf8"
      ),

      scalastyleCheck := Def.task {
        val _ = (scalastyle in Compile).toTask("").value
        (scalastyle in Test).toTask("").value
      }.value,

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

  val noClassFilesSettings: Setting[_] = {
    scalacOptions in (Compile, compile) += {
      if (isGeneratingForIDE) "-Yskip:jvm"
      else "-Ystop-after:jscode"
    }
  }

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

  val fatalWarningsSettings = Def.settings(
      scalacOptions += "-Xfatal-warnings",

      scalacOptions in (Compile, doc) := {
        val prev = (scalacOptions in (Compile, doc)).value
        val scalaV = scalaVersion.value
        def scaladocFullySupportsJDKgreaterThan8 = {
          !scalaV.startsWith("2.11.") &&
          !scalaV.startsWith("2.12.") &&
          scalaV != "2.13.0" && scalaV != "2.13.1" && scalaV != "2.13.2"
        }
        if (javaVersion.value > 8 && !scaladocFullySupportsJDKgreaterThan8)
          prev.filter(_ != "-Xfatal-warnings")
        else
          prev
      }
  )

  val cleanIRSettings = Def.settings(
      products in Compile := {
        val s = streams.value

        val prevProducts = (products in Compile).value

        val outputDir = crossTarget.value / "cleaned-classes"

        val libFileMappings = (PathFinder(prevProducts) ** "*.sjsir")
          .pair(Path.rebase(prevProducts, outputDir))

        val dependencyFiles = {
          val cp = Attributed.data((internalDependencyClasspath in Compile).value)
          (PathFinder(cp) ** "*.sjsir").get
        }

        FileFunction.cached(s.cacheDirectory / "cleaned-sjsir",
            FilesInfo.lastModified, FilesInfo.exists) { _ =>
          s.log.info(s"Patching sjsir files for ${thisProject.value.id} ...")

          if (outputDir.exists)
            IO.delete(outputDir)
          IO.createDirectory(outputDir)

          JavalibIRCleaner.cleanIR(dependencyFiles, libFileMappings, s.log)
        } ((dependencyFiles ++ libFileMappings.map(_._1)).toSet)

        Seq(outputDir)
      }
  )

  private def parallelCollectionsDependencies(
      scalaVersion: String): Seq[ModuleID] = {
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((2, n)) if n >= 13 =>
        Seq("org.scala-lang.modules" %% "scala-parallel-collections" % "0.2.0")

      case _ => Nil
    }
  }

  implicit class ProjectOps(val project: Project) extends AnyVal {
    /** Uses the Scala.js compiler plugin. */
    def withScalaJSCompiler2_12: Project = {
      if (isGeneratingForIDE) project
      else project.dependsOn(compiler.v2_12 % "plugin")
    }

    def withScalaJSJUnitPlugin2_12: Project = {
      project.settings(
          scalacOptions in Test ++= {
            val jar = (packageBin in (jUnitPlugin.v2_12, Compile)).value
            if (isGeneratingForIDE) Seq.empty
            else Seq(s"-Xplugin:$jar")
          }
      )
    }
  }

  implicit class MultiProjectOps(val project: MultiScalaProject) extends AnyVal {
    /** Uses the Scala.js compiler plugin. */
    def withScalaJSCompiler: MultiScalaProject = {
      if (isGeneratingForIDE) project
      else project.dependsOn(compiler % "plugin")
    }

    def withScalaJSJUnitPlugin: MultiScalaProject = {
      project.zippedSettings(jUnitPlugin) { jUnitPlugin =>
        scalacOptions in Test ++= {
          val jar = (packageBin in (jUnitPlugin, Compile)).value
          if (isGeneratingForIDE) Seq.empty
          else Seq(s"-Xplugin:$jar")
        }
      }
    }

    /** Depends on library as if (exportJars in library) was set to false. */
    def dependsOnLibraryNoJar: MultiScalaProject = {
      if (isGeneratingForIDE) {
        project.dependsOn(library)
      } else {
        project.zippedSettings(library) { library =>
          internalDependencyClasspath in Compile ++= {
            val prods = (products in (library, Compile)).value
            val analysis = (compile in (library, Compile)).value
            prods.map(p => Classpaths.analyzed(p, analysis))
          }
        }
      }
    }

    /** Depends on the sources of another project. */
    def dependsOnSource(dependency: MultiScalaProject): MultiScalaProject = {
      if (isGeneratingForIDE) {
        project.dependsOn(dependency)
      } else {
        project.zippedSettings(dependency) { dependency =>
          unmanagedSourceDirectories in Compile ++=
            (unmanagedSourceDirectories in (dependency, Compile)).value
        }
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
      NoIDEExport.noIDEExportSettings,

      {
        val allProjects: Seq[Project] = Seq(
            plugin, linkerPrivateLibrary
        ) ++ Seq(
            compiler, irProject, irProjectJS,
            linkerInterface, linkerInterfaceJS, linker, linkerJS,
            testAdapter,
            javalanglib, javalib, scalalib, libraryAux, library,
            testInterface, jUnitRuntime, testBridge, jUnitPlugin, jUnitAsyncJS,
            jUnitAsyncJVM, jUnitTestOutputsJS, jUnitTestOutputsJVM,
            helloworld, reversi, testingExample, testSuite, testSuiteJVM,
            javalibExtDummies, testSuiteEx, testSuiteExJVM, testSuiteLinker,
            partest, partestSuite,
            scalaTestSuite
        ).flatMap(_.componentProjects)

        val keys = Seq[TaskKey[_]](
            clean, headerCreate in Compile, headerCreate in Test,
            headerCheck in Compile, headerCheck in Test, scalastyleCheck
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

      testOptions += Tests.Argument(TestFrameworks.JUnit, "-a", "-s"),

      unmanagedSourceDirectories in Compile +=
        baseDirectory.value.getParentFile.getParentFile / "shared/src/main/scala",
      unmanagedSourceDirectories in Test +=
        baseDirectory.value.getParentFile.getParentFile / "shared/src/test/scala",
  )

  lazy val irProject: MultiScalaProject = MultiScalaProject(
      id = "ir", base = file("ir/jvm")
  ).settings(
      commonIrProjectSettings,
      libraryDependencies +=
        "com.novocode" % "junit-interface" % "0.9" % "test"
  )

  lazy val irProjectJS: MultiScalaProject = MultiScalaProject(
      id = "irJS", base = file("ir/js")
  ).enablePlugins(
      MyScalaJSPlugin
  ).settings(
      commonIrProjectSettings,
  ).withScalaJSCompiler.withScalaJSJUnitPlugin.dependsOn(
      library, jUnitRuntime % "test", testBridge % "test"
  )

  lazy val compiler: MultiScalaProject = MultiScalaProject(
      id = "compiler", base = file("compiler")
  ).settings(
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
      exportJars := true,

      testOptions += Tests.Argument(TestFrameworks.JUnit, "-a"),
  ).zippedSettings("library")(library =>
      testOptions += {
        val s = streams.value
        val sjslib = (packageBin in (library, Compile)).value

        Tests.Setup { () =>
          val testOutDir = (s.cacheDirectory / "scalajs-compiler-test")
          IO.createDirectory(testOutDir)
          System.setProperty("scala.scalajs.compiler.test.output",
              testOutDir.getAbsolutePath)
          System.setProperty("scala.scalajs.compiler.test.scalajslib",
              sjslib.getAbsolutePath)

          def scalaArtifact(name: String): String = {
            def isTarget(att: Attributed[File]) = {
              att.metadata.get(moduleID.key).exists { mId =>
                mId.organization == "org.scala-lang" &&
                mId.name == name &&
                mId.revision == scalaVersion.value
              }
            }

            (managedClasspath in Test).value.find(isTarget).fold {
              s.log.error(s"Couldn't find $name on the classpath")
              ""
            } { lib =>
              lib.data.getAbsolutePath
            }
          }

          System.setProperty("scala.scalajs.compiler.test.scalalib",
              scalaArtifact("scala-library"))

          System.setProperty("scala.scalajs.compiler.test.scalareflect",
              scalaArtifact("scala-reflect"))
        }
      }
  ).dependsOnSource(irProject)

  val commonLinkerInterfaceSettings = Def.settings(
      commonSettings,
      publishSettings,
      fatalWarningsSettings,
      name := "Scala.js linker interface",

      unmanagedSourceDirectories in Compile +=
        baseDirectory.value.getParentFile.getParentFile / "shared/src/main/scala",
      unmanagedSourceDirectories in Test +=
        baseDirectory.value.getParentFile.getParentFile / "shared/src/test/scala",

      previousArtifactSetting,
      mimaBinaryIssueFilters ++= BinaryIncompatibilities.LinkerInterface,
      exportJars := true, // required so ScalaDoc linking works

      testOptions += Tests.Argument(TestFrameworks.JUnit, "-a")
  )

  lazy val linkerInterface: MultiScalaProject = MultiScalaProject(
      id = "linkerInterface", base = file("linker-interface/jvm")
  ).settings(
      commonLinkerInterfaceSettings,
      libraryDependencies ++= Seq(
          "org.scala-js" %% "scalajs-logging" % "1.1.1",
          "com.novocode" % "junit-interface" % "0.11" % "test",
      ),
  ).dependsOn(irProject, jUnitAsyncJVM % "test")

  lazy val linkerInterfaceJS: MultiScalaProject = MultiScalaProject(
      id = "linkerInterfaceJS", base = file("linker-interface/js")
  ).enablePlugins(
      MyScalaJSPlugin
  ).settings(
      commonLinkerInterfaceSettings,

      /* Add the sources of scalajs-logging to managed sources. This is outside
       * of `target/` so that `clean` does not remove them, making IDE happier.
       */
      managedSourceDirectories in Compile +=
        baseDirectory.value / "scalajs-logging-src",

      // Source generator to retrieve the sources of scalajs-logging
      sourceGenerators in Compile += Def.task {
        val s = streams.value
        val log = s.log

        // Retrieve the source jar of scalajs-logging
        val retrieveDir = baseDirectory.value / "scalajs-logging-src-jars"
        val binVer = scalaBinaryVersion.value
        val lm = dependencyResolution.value
        val jars = lm.retrieve(
            "org.scala-js" % s"scalajs-logging_$binVer" % "1.1.1" classifier "sources" intransitive(),
            scalaModuleInfo = None, retrieveDir, log)
          .fold(w => throw w.resolveException, _.distinct)
        assert(jars.size == 1, jars.toString())
        val jar = jars.head

        // Extract it
        val targetDir = baseDirectory.value / "scalajs-logging-src"
        val cacheDir = s.cacheDirectory / "scalajs-logging-src-cache"
        val fileSet = FileFunction.cached(cacheDir, FilesInfo.lastModified, FilesInfo.exists) { _ =>
          s.log.info(s"Unpacking scalajs-logging sources to $targetDir...")
          if (targetDir.exists)
            IO.delete(targetDir)
          IO.createDirectory(targetDir)
          IO.unzip(jar, targetDir)
        } (Set(jar))

        fileSet.toSeq.filter(_.getPath().endsWith(".scala"))
      }.taskValue,
  ).withScalaJSCompiler.withScalaJSJUnitPlugin.dependsOn(
      library, irProjectJS, jUnitRuntime % "test", testBridge % "test", jUnitAsyncJS % "test",
  )

  lazy val linkerPrivateLibrary: Project = (project in file("linker-private-library")).enablePlugins(
      MyScalaJSPlugin
  ).settings(
      commonSettings,
      scalaVersion := "2.12.12",
      fatalWarningsSettings,
      name := "Scala.js linker private library",
      publishArtifact in Compile := false,
      delambdafySetting,
      cleanIRSettings
  ).withScalaJSCompiler2_12.withScalaJSJUnitPlugin2_12.dependsOn(
      library.v2_12, jUnitRuntime.v2_12 % "test", testBridge.v2_12 % "test",
  )

  def commonLinkerSettings(library: LocalProject) = Def.settings(
      commonSettings,
      publishSettings,
      fatalWarningsSettings,
      name := "Scala.js linker",
      ensureSAMSupportSetting,

      unmanagedSourceDirectories in Compile +=
        baseDirectory.value.getParentFile.getParentFile / "shared/src/main/scala",
      unmanagedSourceDirectories in Test +=
        baseDirectory.value.getParentFile.getParentFile / "shared/src/test/scala",

      if (isGeneratingForIDE) {
        unmanagedSourceDirectories in Test +=
          baseDirectory.value.getParentFile.getParentFile / "shared/src/test/scala-ide-stubs"
      } else {
        sourceGenerators in Test += Def.task {
          val s = streams.value
          val log = s.log
          val lm = dependencyResolution.value
          val binVer = scalaBinaryVersion.value

          val retrieveDir = s.cacheDirectory / "previous-stdlibs"

          val previousStdLibs = previousVersions.map { version =>
            val jars = lm.retrieve("org.scala-js" % s"scalajs-library_$binVer" % version intransitive(),
                scalaModuleInfo = None, retrieveDir, log)
              .fold(w => throw w.resolveException, _.distinct)
            assert(jars.size == 1, jars.toString())
            version -> jars.head
          }.toMap

          ConstantHolderGenerator.generate(
              (sourceManaged in Test).value,
              "org.scalajs.linker.testutils.StdlibHolder",
              "minilib" -> (packageMinilib in (library, Compile)).value,
              "fulllib" -> (packageBin in (library, Compile)).value,
              "previousLibs" -> previousStdLibs,
          )
        }.taskValue
      },

      previousArtifactSetting,
      mimaBinaryIssueFilters ++= BinaryIncompatibilities.Linker,

      mimaBinaryIssueFilters ++= {
        // Always exclude packages where we give no compatibility guarantee.
        import com.typesafe.tools.mima.core.Problem
        import com.typesafe.tools.mima.core.ProblemFilters.exclude

        Seq(
            exclude[Problem]("org.scalajs.linker.analyzer.*"),
            exclude[Problem]("org.scalajs.linker.backend.*"),
            exclude[Problem]("org.scalajs.linker.checker.*"),
            exclude[Problem]("org.scalajs.linker.frontend.*")
        )
      },

      exportJars := true, // required so ScalaDoc linking works

      testOptions += Tests.Argument(TestFrameworks.JUnit, "-a")
  )

  lazy val linker: MultiScalaProject = MultiScalaProject(
      id = "linker", base = file("linker/jvm")
  ).zippedSettings("library")(
    commonLinkerSettings _
  ).settings(
      libraryDependencies ++= Seq(
          "com.google.javascript" % "closure-compiler" % "v20210106",
          "com.novocode" % "junit-interface" % "0.9" % "test",
          "com.google.jimfs" % "jimfs" % "1.1" % "test"
      ) ++ (
          parallelCollectionsDependencies(scalaVersion.value)
      ),

      resourceGenerators in Compile += Def.task {
        val s = streams.value
        val baseResourceDir = (resourceManaged in Compile).value
        val resourceDir = baseResourceDir / "org/scalajs/linker/backend/emitter"

        val privateLibProducts = (products in (linkerPrivateLibrary, Compile)).value

        // Copy all *.sjsir files to resourceDir.
        val mappings = (privateLibProducts ** "*.sjsir").pair(Path.flat(resourceDir))
        Sync.sync(s.cacheStoreFactory.make("linker-library"))(mappings)

        mappings.unzip._2
      }.taskValue,

      fork in Test := true
  ).dependsOn(linkerInterface, irProject, jUnitAsyncJVM % "test")

  lazy val linkerJS: MultiScalaProject = MultiScalaProject(
      id = "linkerJS", base = file("linker/js")
  ).enablePlugins(
      MyScalaJSPlugin
  ).zippedSettings("library")(
      commonLinkerSettings _
  ).settings(
      if (isGeneratingForIDE) {
        unmanagedSourceDirectories in Compile +=
          baseDirectory.value.getParentFile.getParentFile / "js/src/main/scala-ide-stubs"
      } else {
        sourceGenerators in Compile += Def.task {
          val dir = (sourceManaged in Compile).value
          val privateLibProducts = (products in (linkerPrivateLibrary, Compile)).value

          val content = {
            val namesAndContents = for {
              f <- (privateLibProducts ** "*.sjsir").get
            } yield {
              val bytes = IO.readBytes(f)
              val base64 = java.util.Base64.getEncoder().encodeToString(bytes)
              s""""${f.getName}" -> "$base64""""
            }

            s"""
            |package org.scalajs.linker.backend.emitter
            |
            |import org.scalajs.linker.interface.IRFile
            |import org.scalajs.linker.standard.MemIRFileImpl
            |
            |object PrivateLibHolder {
            |  private val namesAndContents = Seq(
            |    ${namesAndContents.mkString(",\n    ")}
            |  )
            |
            |  val files: Seq[IRFile] = {
            |    for ((name, contentBase64) <- namesAndContents) yield {
            |      new MemIRFileImpl(
            |          path = "org/scalajs/linker/runtime/" + name,
            |          version = Some(""), // this indicates that the file never changes
            |          content = java.util.Base64.getDecoder().decode(contentBase64)
            |      )
            |    }
            |  }
            |}
            """.stripMargin
          }

          IO.createDirectory(dir)
          val output = dir / "PrivateLibHolder.scala"
          IO.write(output, content)
          Seq(output)
        }.taskValue,
      },

      scalaJSLinkerConfig in Test ~= (_.withModuleKind(ModuleKind.CommonJSModule))
  ).withScalaJSCompiler.withScalaJSJUnitPlugin.dependsOn(
      linkerInterfaceJS, library, irProjectJS, jUnitRuntime % "test", testBridge % "test", jUnitAsyncJS % "test"
  )

  lazy val testAdapter: MultiScalaProject = MultiScalaProject(
      id = "testAdapter", base = file("test-adapter")
  ).settings(
      commonSettings,
      publishSettings,
      fatalWarningsSettings,
      name := "Scala.js sbt test adapter",
      libraryDependencies ++= Seq(
          "org.scala-sbt" % "test-interface" % "1.0",
          "org.scala-js" %% "scalajs-js-envs" % "1.1.1",
          "com.novocode" % "junit-interface" % "0.11" % "test",
          "com.google.jimfs" % "jimfs" % "1.1" % "test",
      ),
      previousArtifactSetting,
      mimaBinaryIssueFilters ++= BinaryIncompatibilities.TestAdapter,
      unmanagedSourceDirectories in Compile +=
        baseDirectory.value.getParentFile.getParentFile / "test-common/src/main/scala",
      unmanagedSourceDirectories in Test +=
        baseDirectory.value.getParentFile.getParentFile / "test-common/src/test/scala"
  ).dependsOn(jUnitAsyncJVM % "test")

  lazy val plugin: Project = Project(id = "sbtPlugin", base = file("sbt-plugin"))
      .enablePlugins(ScriptedPlugin).settings(
      commonSettings,
      publishSettings,
      fatalWarningsSettings,
      name := "Scala.js sbt plugin",
      normalizedName := "sbt-scalajs",
      sbtPlugin := true,
      crossScalaVersions := Seq("2.12.12"),
      scalaVersion := crossScalaVersions.value.head,
      sbtVersion := "1.0.0",
      scalaBinaryVersion :=
        CrossVersion.binaryScalaVersion(scalaVersion.value),
      previousArtifactSetting,
      mimaBinaryIssueFilters ++= BinaryIncompatibilities.SbtPlugin,

      addSbtPlugin("org.portable-scala" % "sbt-platform-deps" % "1.0.0"),
      libraryDependencies += "org.scala-js" %% "scalajs-js-envs" % "1.1.1",
      libraryDependencies += "org.scala-js" %% "scalajs-env-nodejs" % "1.1.1",

      scriptedLaunchOpts += "-Dplugin.version=" + version.value,

      scriptedLaunchOpts ++= {
        // Forward Ivy home options.
        for {
          o <- Seq("sbt.boot.directory", "sbt.ivy.home", "ivy.home", "sbt.global.base")
          v <- sys.props.get(o)
        } yield {
          s"-D$o=$v"
        }
      },

      scriptedDependencies := {
        scriptedDependencies.dependsOn(
            // Compiler Plugins
            publishLocal in compiler.v2_11,
            publishLocal in jUnitPlugin.v2_11,

            publishLocal in compiler.v2_12,
            publishLocal in jUnitPlugin.v2_12,

            publishLocal in compiler.v2_13,
            publishLocal in jUnitPlugin.v2_13,

            // JS libs
            publishLocal in library.v2_11,
            publishLocal in testInterface.v2_11,
            publishLocal in testBridge.v2_11,
            publishLocal in jUnitRuntime.v2_11,
            publishLocal in irProjectJS.v2_11,

            publishLocal in library.v2_12,
            publishLocal in testInterface.v2_12,
            publishLocal in testBridge.v2_12,
            publishLocal in jUnitRuntime.v2_12,
            publishLocal in irProjectJS.v2_12,

            publishLocal in library.v2_13,
            publishLocal in testInterface.v2_13,
            publishLocal in testBridge.v2_13,
            publishLocal in jUnitRuntime.v2_13,
            publishLocal in irProjectJS.v2_13,

            // JVM libs
            publishLocal in irProject.v2_12,
            publishLocal in linkerInterface.v2_12,
            publishLocal in linker.v2_12,
            publishLocal in testAdapter.v2_12,
        ).value
      },

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
  ).dependsOn(linkerInterface.v2_12, testAdapter.v2_12)

  lazy val delambdafySetting = {
    scalacOptions ++= (
        if (isGeneratingForIDE) Seq()
        else Seq("-Ydelambdafy:method"))
  }

  lazy val ensureSAMSupportSetting: Setting[_] = {
    scalacOptions ++= {
      if (scalaBinaryVersion.value == "2.11") Seq("-Xexperimental")
      else Nil
    }
  }

  lazy val javalanglib: MultiScalaProject = MultiScalaProject(
      id = "javalanglib", base = file("javalanglib")
  ).enablePlugins(
      MyScalaJSPlugin
  ).settings(
      commonSettings,
      fatalWarningsSettings,
      name := "java.lang library for Scala.js",
      publishArtifact in Compile := false,
      delambdafySetting,
      ensureSAMSupportSetting,
      noClassFilesSettings,

      /* When writing code in the java.lang package, references to things
       * like `Boolean` or `Double` refer to `j.l.Boolean` or `j.l.Double`.
       * Usually this is not what we want (we want the primitive types
       * instead), but the implicits available in `Predef` hide mistakes by
       * introducing boxing and unboxing where required. The `-Yno-predef`
       * flag prevents these mistakes from happening.
       */
      scalacOptions += "-Yno-predef",
      // We implement JDK classes, so we emit static forwarders for all static objects
      addScalaJSCompilerOption("genStaticForwardersForNonTopLevelObjects"),

      resourceGenerators in Compile += Def.task {
        val output = (resourceManaged in Compile).value / "java/lang/Object.sjsir"
        val data = JavaLangObject.irBytes

        if (!output.exists || !Arrays.equals(data, IO.readBytes(output))) {
          IO.write(output, data)
        }

        Seq(output)
      }.taskValue,
      scalaJSExternalCompileSettings,
      cleanIRSettings,
  ).withScalaJSCompiler.dependsOnLibraryNoJar

  lazy val javalib: MultiScalaProject = MultiScalaProject(
      id = "javalib", base = file("javalib")
  ).enablePlugins(
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

      /* Do not import `Predef._` so that we have a better control of when
       * we rely on the Scala library.
       */
      scalacOptions += "-Yno-predef",
      // We implement JDK classes, so we emit static forwarders for all static objects
      addScalaJSCompilerOption("genStaticForwardersForNonTopLevelObjects"),

      /* In the javalib, we often need to perform `a.equals(b)` with operands
       * of unconstrained types in order to implement the JDK specs. Scala
       * 2.13.4+ warns for such calls, but those warnings are always noise in
       * the javalib, so we globally silence them.
       */
      scalacOptions ++= {
        val scalaV = scalaVersion.value
        val scalaWarnsForNonCooperativeEquals = {
          !scalaV.startsWith("2.11.") &&
          !scalaV.startsWith("2.12.") &&
          scalaV != "2.13.0" && scalaV != "2.13.1" && scalaV != "2.13.2" && scalaV != "2.13.3"
        }
        if (scalaWarnsForNonCooperativeEquals)
          Seq("-Wconf:cat=other-non-cooperative-equals:s")
        else
          Nil
      },

      headerSources in Compile ~= { srcs =>
        srcs.filter { src =>
          val path = src.getPath.replace('\\', '/')
          !path.contains("/java/math/") &&
          !path.endsWith("/java/util/concurrent/ThreadLocalRandom.scala")
        }
      }
  ).withScalaJSCompiler.dependsOnLibraryNoJar

  lazy val scalalib: MultiScalaProject = MultiScalaProject(
      id = "scalalib", base = file("scalalib")
  ).enablePlugins(
      MyScalaJSPlugin
  ).settings(
      commonSettings,
      /* Link source maps to the GitHub sources of the original scalalib
       * #2195 This must come *before* the option added by MyScalaJSPlugin
       * because mapSourceURI works on a first-match basis.
       * That is why we cannot use addScalaJSCompilerOption.
       */
      scalacOptions := {
        val prev = scalacOptions.value
        if (isGeneratingForIDE) {
          prev
        } else {
          val option = {
            "-P:scalajs:mapSourceURI:" +
            (artifactPath in fetchScalaSource).value.toURI +
            "->https://raw.githubusercontent.com/scala/scala/v" +
            scalaVersion.value + "/src/library/"
          }
          option +: prev
        }
      },
      name := "Scala library for Scala.js",
      publishArtifact in Compile := false,
      NoIDEExport.noIDEExportSettings,
      delambdafySetting,
      noClassFilesSettings,

      // Ignore scalastyle for this project
      scalastyleCheck := {},

      // The Scala lib is full of warnings we don't want to see
      scalacOptions ~= (_.filterNot(
          Set("-deprecation", "-unchecked", "-feature") contains _)),

      // Tell the plugin to hack-fix bad classOf trees
      addScalaJSCompilerOption("fixClassOf"),

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
            configuration = configurationFilter("compile"),
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
        // - override-2.13.0-RC1
        // - override-2.13.0
        // - override-2.13
        // - override-2
        // - override
        val ver = scalaVersion.value
        val base = baseDirectory.value.getParentFile
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

        val s = streams.value

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
              s.log.debug(s"not including $src")
          }
        }

        sources.result()
      },

      headerSources in Compile := Nil,
      headerSources in Test := Nil,

      scalaJSExternalCompileSettings
  ).withScalaJSCompiler.dependsOnLibraryNoJar

  lazy val libraryAux: MultiScalaProject = MultiScalaProject(
      id = "libraryAux", base = file("library-aux")
  ).enablePlugins(
      MyScalaJSPlugin
  ).settings(
      commonSettings,
      fatalWarningsSettings,
      name := "Scala.js aux library",
      publishArtifact in Compile := false,
      NoIDEExport.noIDEExportSettings,
      delambdafySetting,
      noClassFilesSettings,
      scalaJSExternalCompileSettings
  ).withScalaJSCompiler.dependsOnLibraryNoJar

  lazy val library: MultiScalaProject = MultiScalaProject(
      id = "library", base = file("library")
  ).enablePlugins(
      MyScalaJSPlugin
  ).settings(
      commonSettings,
      publishSettings,
      crossVersion := CrossVersion.binary, // no _sjs suffix
      fatalWarningsSettings,
      name := "Scala.js library",
      delambdafySetting,
      ensureSAMSupportSetting,
      exportJars := !isGeneratingForIDE,
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

              prev.filter(filter.accept)
            } else {
              Nil
            }
          },

          /* Add compiled .class files to doc dependencyClasspath, so we can
           * still compile even with only part of the files being present.
           */
          dependencyClasspath in doc ++= exportedProducts.value,
      ))
  ).zippedSettings(Seq("javalanglib", "javalib", "scalalib", "libraryAux"))(localProjects =>
        inConfig(Compile)(Seq(
          /* Add the .sjsir files from other lib projects
           * (but not .class files)
           */
          mappings in packageBin := {
            /* From library, we must take everything, except the
             * java.nio.TypedArrayBufferBridge object, whose actual
             * implementation is in javalib.
             */
            val superMappings = (mappings in packageBin).value
            val libraryMappings = superMappings.filter { mapping =>
              !mapping._2.replace('\\', '/').startsWith(
                  "scala/scalajs/js/typedarray/TypedArrayBufferBridge")
            }

            val filter = ("*.sjsir": NameFilter)

            val otherProducts = (
                (products in localProjects(0)).value ++
                (products in localProjects(1)).value ++
                (products in localProjects(2)).value ++
                (products in localProjects(3)).value)
            val otherMappings =
              otherProducts.flatMap(base => Path.selectSubpaths(base, filter))

            libraryMappings ++ otherMappings
          },

          packageMinilib := {
            val sources = (mappings in packageBin).value.filter { mapping =>
              MiniLib.Whitelist.contains(mapping._2.replace('\\', '/'))
            }
            val jar = crossTarget.value / "minilib.jar"
            val config = new sbt.Package.Configuration(sources, jar, Nil)
            val s = streams.value
            sbt.Package(config, s.cacheStoreFactory, s.log)
            jar
          }
      ))
  ).withScalaJSCompiler

  // The Scala.js version of sbt-testing-interface
  lazy val testInterface: MultiScalaProject = MultiScalaProject(
      id = "testInterface", base = file("test-interface")
  ).enablePlugins(
      MyScalaJSPlugin
  ).settings(
      commonSettings,
      publishSettings,
      crossVersion := CrossVersion.binary, // no _sjs suffix
      fatalWarningsSettings,
      name := "Scala.js test interface",
      delambdafySetting,
      previousArtifactSetting,
      mimaBinaryIssueFilters ++= BinaryIncompatibilities.TestInterface
  ).withScalaJSCompiler.dependsOn(library)

  lazy val testBridge: MultiScalaProject = MultiScalaProject(
      id = "testBridge", base = file("test-bridge")
  ).enablePlugins(
      MyScalaJSPlugin
  ).settings(
      commonSettings,
      publishSettings,
      crossVersion := CrossVersion.binary, // no _sjs suffix
      fatalWarningsSettings,
      name := "Scala.js test bridge",
      delambdafySetting,
      /* By design, the test-bridge has a completely private API (it is
       * only loaded through a privately-known top-level export), so it
       * does not have `previousArtifactSetting` nor
       * `mimaBinaryIssueFilters`.
       */
      unmanagedSourceDirectories in Compile +=
        baseDirectory.value.getParentFile.getParentFile / "test-common/src/main/scala",
      unmanagedSourceDirectories in Test +=
        baseDirectory.value.getParentFile.getParentFile / "test-common/src/test/scala"
  ).withScalaJSCompiler.withScalaJSJUnitPlugin.dependsOn(
      library, testInterface, jUnitRuntime % "test", jUnitAsyncJS % "test"
  )

  lazy val jUnitRuntime: MultiScalaProject = MultiScalaProject(
      id = "jUnitRuntime", base = file("junit-runtime")
  ).enablePlugins(
      MyScalaJSPlugin
  ).settings(
      commonSettings,
      publishSettings,
      crossVersion := CrossVersion.binary, // no _sjs suffix
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
        baseDirectory.value.getParentFile.getParentFile / "shared/src/test/scala",
      testOptions in Test ++= Seq(
          Tests.Argument(TestFrameworks.JUnit, "-a", "-s"),
          Tests.Filter(_.endsWith("Assertions"))
      )
  )

  lazy val jUnitTestOutputsJS: MultiScalaProject = MultiScalaProject(
      id = "jUnitTestOutputsJS", base = file("junit-test/output-js")
  ).enablePlugins(
      MyScalaJSPlugin
  ).settings(
      commonJUnitTestOutputsSettings,
      name := "Tests for Scala.js JUnit output in JS."
  ).withScalaJSCompiler.withScalaJSJUnitPlugin.dependsOn(
      jUnitRuntime % "test", testBridge % "test", jUnitAsyncJS % "test"
  )


  lazy val jUnitTestOutputsJVM: MultiScalaProject = MultiScalaProject(
      id = "jUnitTestOutputsJVM", base = file("junit-test/output-jvm")
  ).settings(
      commonJUnitTestOutputsSettings,
      name := "Tests for Scala.js JUnit output in JVM.",
      libraryDependencies ++= Seq(
          "org.scala-sbt" % "test-interface" % "1.0" % "test",
          "com.novocode" % "junit-interface" % "0.11" % "test"
      )
  ).dependsOn(
       jUnitAsyncJVM % "test"
  )

  lazy val jUnitPlugin: MultiScalaProject = MultiScalaProject(
      id = "jUnitPlugin", base = file("junit-plugin")
  ).settings(
      commonSettings,
      publishSettings,
      fatalWarningsSettings,
      name := "Scala.js JUnit test plugin",
      crossVersion := CrossVersion.full,
      libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value,
      exportJars := true
  )

  lazy val jUnitAsyncJS: MultiScalaProject = MultiScalaProject(
      id = "jUnitAsyncJS", base = file("junit-async/js")
  ).enablePlugins(
      MyScalaJSPlugin
  ).withScalaJSCompiler.settings(
      commonSettings,
      name := "Scala.js internal JUnit async JS support",
      publishArtifact in Compile := false
  ).dependsOn(library)

  lazy val jUnitAsyncJVM: MultiScalaProject = MultiScalaProject(
      id = "jUnitAsyncJVM", base = file("junit-async/jvm")
  ).settings(
      commonSettings,
      name := "Scala.js internal JUnit async JVM support",
      publishArtifact in Compile := false
  )

  // Examples

  lazy val exampleSettings = commonSettings ++ fatalWarningsSettings ++ Def.settings(
      headerSources in Compile := Nil,
      headerSources in Test := Nil
  )

  lazy val helloworld: MultiScalaProject = MultiScalaProject(
      id = "helloworld", base = file("examples") / "helloworld"
  ).enablePlugins(
      MyScalaJSPlugin
  ).settings(
      exampleSettings,
      name := "Hello World - Scala.js example",
      moduleName := "helloworld",
      scalaJSUseMainModuleInitializer := true
  ).withScalaJSCompiler.dependsOn(library)

  lazy val reversi: MultiScalaProject = MultiScalaProject(
      id = "reversi", base = file("examples") / "reversi"
  ).enablePlugins(
      MyScalaJSPlugin
  ).settings(
      exampleSettings,
      name := "Reversi - Scala.js example",
      moduleName := "reversi",

      MyScalaJSPlugin.expectedSizes := {
        scalaVersion.value match {
          case "2.11.12" =>
            Some(ExpectedSizes(
                fastLink = 520000 to 521000,
                fullLink = 108000 to 109000,
                fastLinkGz = 66000 to 67000,
                fullLinkGz = 28000 to 29000,
            ))

          case "2.12.12" =>
            Some(ExpectedSizes(
                fastLink = 781000 to 782000,
                fullLink = 148000 to 149000,
                fastLinkGz = 91000 to 92000,
                fullLinkGz = 36000 to 37000,
            ))

          case "2.13.4" =>
            Some(ExpectedSizes(
                fastLink = 780000 to 781000,
                fullLink = 170000 to 171000,
                fastLinkGz = 98000 to 99000,
                fullLinkGz = 43000 to 44000,
            ))

          case _ =>
            None
        }
      }
  ).withScalaJSCompiler.dependsOn(library)

  lazy val testingExample: MultiScalaProject = MultiScalaProject(
      id = "testingExample", base = file("examples") / "testing"
  ).enablePlugins(
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

      // To support calls to static methods in interfaces
      scalacOptions in Test ++= {
        /* Starting from 2.11.12, scalac refuses to emit calls to static methods
         * in interfaces unless the -target:jvm-1.8 flag is given.
         * scalac 2.12+ emits JVM 8 bytecode by default, of course, so it is not
         * needed for later versions.
         */
        if (scalaVersion.value.startsWith("2.11."))
          Seq("-target:jvm-1.8")
        else
          Nil
      },

      // Need reflect for typechecking macros
      libraryDependencies +=
        "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided",

      testOptions += Tests.Argument(TestFrameworks.JUnit, "-a", "-s"),

      unmanagedSourceDirectories in Test ++= {
        val testDir = (sourceDirectory in Test).value
        val sharedTestDir =
          testDir.getParentFile.getParentFile.getParentFile / "shared/src/test"

        val javaV = javaVersion.value
        val scalaV = scalaVersion.value
        val isScalaAtLeast212 = !scalaV.startsWith("2.11.")

        List(sharedTestDir / "scala", sharedTestDir / "require-scala2") ++
        includeIf(sharedTestDir / "require-jdk11", javaV >= 11) ++
        includeIf(testDir / "require-2.12", isJSTest && isScalaAtLeast212) ++
        includeIf(testDir / "require-scala2", isJSTest)
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

  def testSuiteBootstrapSetting(testSuiteLinker: Project) = Def.settings(
      Defaults.testSettings,
      ScalaJSPlugin.testConfigSettings,

      fullLinkJS := {
        throw new MessageOnlyException("fullLinkJS is not supported in Bootstrap")
      },

      fastLinkJS := {
        val s = streams.value

        val reportFile = s.cacheDirectory / "linking-report.bin"
        val outputDir = (scalaJSLinkerOutputDirectory in fastLinkJS).value

        val linkerModule =
          (scalaJSLinkedFile in (testSuiteLinker, Compile)).value.data

        val cp = Attributed.data(fullClasspath.value)
        val cpFiles = (scalaJSIR in fastLinkJS).value.get(scalaJSSourceFiles).get

        FileFunction.cached(s.cacheDirectory, FilesInfo.lastModified,
            FilesInfo.exists) { _ =>

          def jsstr(f: File) = "\"" + escapeJS(f.getAbsolutePath) + "\""

          val cpPaths = cp.map(jsstr(_)).mkString("[", ", ", "]")

          val code = {
            s"""
              var toolsTestModule = require(${jsstr(linkerModule)});
              var linker = toolsTestModule.TestSuiteLinker;
              var result =
                linker.linkTestSuiteNode($cpPaths, ${jsstr(outputDir)}, ${jsstr(reportFile)});

              result.catch(e => {
                console.error(e);
                process.exit(1);
              });
            """
          }

          val launcherFile = crossTarget.value / "test-suite-linker.js"
          IO.write(launcherFile, code)

          val config = RunConfig().withLogger(sbtLogger2ToolsLogger(s.log))
          val input = List(Input.Script(launcherFile.toPath))

          s.log.info(s"Linking test suite with JS linker")

          IO.createDirectory(outputDir)

          val jsEnv = new NodeJSEnv(
            NodeJSEnv.Config()
              .withArgs(List("--max_old_space_size=3072"))
              .withSourceMap(false))

          val run = jsEnv.start(input, config)
          Await.result(run.future, Duration.Inf)

          IO.listFiles(outputDir).toSet + reportFile
        } ((cpFiles :+ linkerModule).toSet)

        val report = Report.deserialize(IO.readBytes(reportFile)).getOrElse {
            throw new MessageOnlyException("failed to deserialize report after " +
                "bootstrapped linking. version mismatch?")
        }

        Attributed.blank(report)
          .put(scalaJSLinkerOutputDirectory.key, outputDir)
      },

      compile := (compile in Test).value,
      fullClasspath := (fullClasspath in Test).value,
      testSuiteJSExecutionFilesSetting
  )

  def testSuiteJSExecutionFilesSetting: Setting[_] = {
    jsEnvInput := {
      val resourceDir = (resourceDirectory in Test).value
      val f = (resourceDir / "NonNativeJSTypeTestNatives.js").toPath
      Input.Script(f) +: jsEnvInput.value
    }
  }

  lazy val Bootstrap = config("bootstrap")
    .describedAs("Configuration that uses a JS linker instead of the JVM")

  lazy val testSuite: MultiScalaProject = MultiScalaProject(
      id = "testSuite", base = file("test-suite/js")
  ).enablePlugins(
      MyScalaJSPlugin
  ).configs(Bootstrap).settings(
      commonSettings,
      inConfig(Test)(testSuiteJSExecutionFilesSetting),
      testSuiteCommonSettings(isJSTest = true),
      name := "Scala.js test suite",

      unmanagedSourceDirectories in Test ++= {
        val testDir = (sourceDirectory in Test).value
        val scalaV = scalaVersion.value

        val linkerConfig = scalaJSStage.value match {
          case FastOptStage => (scalaJSLinkerConfig in (Compile, fastLinkJS)).value
          case FullOptStage => (scalaJSLinkerConfig in (Compile, fullLinkJS)).value
        }

        val moduleKind = linkerConfig.moduleKind
        val hasModules = moduleKind != ModuleKind.NoModule

        collectionsEraDependentDirectory(scalaV, testDir) ::
        includeIf(testDir / "require-modules",
            hasModules) :::
        includeIf(testDir / "require-multi-modules",
            hasModules && !linkerConfig.closureCompiler) :::
        includeIf(testDir / "require-dynamic-import",
            moduleKind == ModuleKind.ESModule) // this is an approximation that works for now
      },

      unmanagedResourceDirectories in Test ++= {
        val testDir = (sourceDirectory in Test).value

        scalaJSLinkerConfig.value.moduleKind match {
          case ModuleKind.NoModule       => Nil
          case ModuleKind.CommonJSModule => Seq(testDir / "resources-commonjs")
          case ModuleKind.ESModule       => Seq(testDir / "resources-esmodule")
        }
      },

      addScalaJSCompilerOptionInConfig(Test, "genStaticForwardersForNonTopLevelObjects"),

      scalaJSLinkerConfig ~= { _.withSemantics(TestSuiteLinkerOptions.semantics _) },
      scalaJSModuleInitializers in Test ++= TestSuiteLinkerOptions.moduleInitializers,

      /* The script that calls setExportsNamespaceForExportsTest to provide
       * ExportsTest with a loopback reference to its own exports namespace.
       * Only when using an ES module.
       * See the comment in ExportsTest for more details.
       */
      jsEnvInput in Test ++= {
        val moduleKind = (scalaJSLinkerConfig in Test).value.moduleKind
        val linkerResult = (scalaJSLinkerResult in Test).value

        val report = linkerResult.data

        val outputFile = {
          val outputDir = linkerResult.get(scalaJSLinkerOutputDirectory.key).get

          val ext = {
            val name = report.publicModules.head.jsFileName
            val dotPos = name.lastIndexOf('.')
            if (dotPos < 0) ".js" else name.substring(dotPos)
          }

          outputDir / ("export-loopback" + ext)
        }

        val setDict = {
          val dict = report.publicModules
            .map(m => s"${m.moduleID}: ${m.moduleID}")
            .mkString("{", ",", "}")

          s"main.setExportsNamespaceForExportsTest($dict);"
        }

        moduleKind match {
          case ModuleKind.ESModule =>
            /* Due to the asynchronous nature of ES module loading, there
             * exists a theoretical risk for a race condition here. It is
             * possible that tests will start running and reaching the
             * ExportsTest before this module is executed. It's quite
             * unlikely, though, given all the message passing for the com
             * and all that.
             */
            val imports = report.publicModules
              .map(m => s"""import * as ${m.moduleID} from "./${escapeJS(m.jsFileName)}";\n""")
              .mkString

            IO.write(outputFile, imports + setDict)
            List(Input.ESModule(outputFile.toPath))

          case ModuleKind.CommonJSModule =>
            val requires = report.publicModules
              .map(m => s"""var ${m.moduleID} = require("./${escapeJS(m.jsFileName)}");\n""")
              .mkString

            IO.write(outputFile, requires + setDict)
            List(Input.CommonJSModule(outputFile.toPath))

          case ModuleKind.NoModule =>
            Nil
        }
      },

      if (isGeneratingForIDE) {
        unmanagedSourceDirectories in Compile +=
          baseDirectory.value / "src/main/scala-ide-stubs"
      } else {
        sourceGenerators in Compile += Def.task {
          val stage = scalaJSStage.value

          val linkerConfig = stage match {
            case FastOptStage => (scalaJSLinkerConfig in (Compile, fastLinkJS)).value
            case FullOptStage => (scalaJSLinkerConfig in (Compile, fullLinkJS)).value
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
        }.taskValue
      },

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
      for ((key, stage) <- Seq(fastLinkJS -> FastOptStage, fullLinkJS -> FullOptStage)) yield {
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
  ).zippedSettings(testSuiteLinker)(
      l => inConfig(Bootstrap)(testSuiteBootstrapSetting(l))
  ).withScalaJSCompiler.withScalaJSJUnitPlugin.dependsOn(
      library, jUnitRuntime, testBridge % "test", jUnitAsyncJS % "test"
  )

  lazy val testSuiteJVM: MultiScalaProject = MultiScalaProject(
      id = "testSuiteJVM", base = file("test-suite/jvm")
  ).settings(
      commonSettings,
      testSuiteCommonSettings(isJSTest = false),
      name := "Scala.js test suite on JVM",

      /* Scala.js always assumes Locale.ROOT, UTF-8 and NL as line separator by
       * default. Since some of our tests rely on these defaults (notably to
       * test them), we have to force the same values on the JVM.
       */
      fork in Test := true,
      javaOptions in Test ++= Seq(
          "-Dfile.encoding=UTF-8",
          "-Duser.country=", "-Duser.language=",
          "-Dline.separator=\n"
      ),

      libraryDependencies +=
        "com.novocode" % "junit-interface" % "0.11" % "test"
  )

  /* Dummies for javalib extensions that can be implemented outside the core.
   * The dummies in this project are used in testSuiteEx to test some
   * (fortunately rare) methods implemented in the core even though they cannot
   * link without an additional javalib extension.
   *
   * Examples include:
   *
   * - java.time.Instant, referred to in java.util.Date
   *
   * The dummies are definitely not suited for general use. They work just
   * enough for our tests of other features to work. As such, they must not be
   * published.
   */
  lazy val javalibExtDummies: MultiScalaProject = MultiScalaProject(
      id = "javalibExtDummies", base = file("javalib-ext-dummies")
  ).enablePlugins(
      MyScalaJSPlugin
  ).settings(
      commonSettings,
      fatalWarningsSettings,
      name := "Java Ext Dummies library for Scala.js",
      publishArtifact in Compile := false,
      delambdafySetting,
      ensureSAMSupportSetting,
      noClassFilesSettings,
      scalaJSExternalCompileSettings,
      exportJars := true,

      /* Do not import `Predef._` so that we have a better control of when
       * we rely on the Scala library.
       */
      scalacOptions += "-Yno-predef",
      // We implement JDK classes, so we emit static forwarders for all static objects
      addScalaJSCompilerOption("genStaticForwardersForNonTopLevelObjects"),
  ).withScalaJSCompiler.dependsOn(
      library
  )

  def testSuiteExCommonSettings(isJSTest: Boolean): Seq[Setting[_]] = Def.settings(
      publishArtifact in Compile := false,

      testOptions += Tests.Argument(TestFrameworks.JUnit, "-a", "-s"),

      unmanagedSourceDirectories in Test +=
        (sourceDirectory in Test).value.getParentFile.getParentFile.getParentFile / "shared/src/test",
  )

  /* Additional test suite, for tests that should not be part of the normal
   * test suite for various reasons. There are two common reasons:
   *
   * - some tests in there "fail to fail" if they happen in the larger test
   *   suite, due to all the other code that's there (can have impact on dce,
   *   optimizations, GCC, etc.)
   * - some tests pollute the linking state at a global scale, and therefore
   *   would have an impact on the main test suite (dangerous global refs,
   *   javalib extension dummies, etc.)
   *
   * TODO Ideally, we should have a mechanism to separately compile, link and
   * test each file in this test suite, so that we're sure that do not
   * interfere with other.
   */
  lazy val testSuiteEx: MultiScalaProject = MultiScalaProject(
      id = "testSuiteEx", base = file("test-suite-ex/js")
  ).enablePlugins(
      MyScalaJSPlugin
  ).settings(
      commonSettings,
      testSuiteExCommonSettings(isJSTest = true),
      name := "Scala.js test suite ex",
      publishArtifact in Compile := false,
  ).withScalaJSCompiler.withScalaJSJUnitPlugin.dependsOn(
      library, javalibExtDummies, jUnitRuntime, testBridge % "test", testSuite
  )

  lazy val testSuiteExJVM: MultiScalaProject = MultiScalaProject(
      id = "testSuiteExJVM", base = file("test-suite-ex/jvm")
  ).settings(
      commonSettings,
      testSuiteExCommonSettings(isJSTest = false),
      name := "Scala.js test suite ex on JVM",

      /* Scala.js always assumes Locale.ROOT, UTF-8 and NL as line separator by
       * default. Since some of our tests rely on these defaults (notably to
       * test them), we have to force the same values on the JVM.
       */
      fork in Test := true,
      javaOptions in Test ++= Seq(
          "-Dfile.encoding=UTF-8",
          "-Duser.country=", "-Duser.language=",
          "-Dline.separator=\n"
      ),

      libraryDependencies +=
        "com.novocode" % "junit-interface" % "0.11" % "test",
  ).dependsOn(
      testSuiteJVM
  )

  lazy val testSuiteLinker: MultiScalaProject = MultiScalaProject(
      id = "testSuiteLinker", base = file("test-suite-linker")
  ).enablePlugins(
      MyScalaJSPlugin
  ).settings(
      exampleSettings,
      name := "Scala.js test suite linker",
      scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule)),
      sources in Compile += {
        baseDirectory.value.getParentFile.getParentFile /
          "project/TestSuiteLinkerOptions.scala"
      }
  ).withScalaJSCompiler.dependsOn(linkerJS)

  def shouldPartestSetting(partestSuite: LocalProject) = Def.settings(
      shouldPartest := {
        val testListDir = (
          (resourceDirectory in (partestSuite, Test)).value / "scala"
            / "tools" / "partest" / "scalajs" / scalaVersion.value
        )
        testListDir.exists
      },
  )

  lazy val partest: MultiScalaProject = MultiScalaProject(
      id = "partest", base = file("partest")
  ).settings(
      commonSettings,
      fatalWarningsSettings,
      name := "Partest for Scala.js",
      moduleName := "scalajs-partest",

      resolvers += Resolver.typesafeIvyRepo("releases"),

      libraryDependencies += "org.scala-js" %% "scalajs-env-nodejs" % "1.1.1",

      artifactPath in fetchScalaSource :=
        baseDirectory.value.getParentFile / "fetchedSources" / scalaVersion.value,

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
              "org.scala-sbt" % "test-interface" % "1.0",
              {
                val v = scalaVersion.value
                if (v.startsWith("2.11."))
                  "org.scala-lang.modules" %% "scala-partest" % "1.0.16"
                else
                  "org.scala-lang.modules" %% "scala-partest" % "1.1.4"
              }
          )
        } else {
          Seq()
        }
      },

      // Ignore scalastyle for this project
      scalastyleCheck := {},

      sources in Compile := {
        val s = (sources in Compile).value
        if (shouldPartest.value) s else Nil
      }
  ).zippedSettings("partestSuite")(partestSuite =>
      shouldPartestSetting(partestSuite)
  ).dependsOn(compiler, linker)

  lazy val partestSuite: MultiScalaProject = MultiScalaProject(
      id = "partestSuite", base = file("partest-suite")
  ).settings(
      commonSettings,
      fatalWarningsSettings,
      name := "Scala.js partest suite",
      NoIDEExport.noIDEExportSettings,

      fork in Test := true,
      javaOptions in Test += "-Xmx1G",

      // Override the dependency of partest - see #1889
      dependencyOverrides += "org.scala-lang" % "scala-library" % scalaVersion.value % "test",

      testFrameworks ++= {
        if (shouldPartest.value)
          Seq(new TestFramework("scala.tools.partest.scalajs.Framework"))
        else Seq()
      },
  ).zippedSettings(partest)(partest =>
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
  ).zippedSettings("partestSuite")(partestSuite =>
      shouldPartestSetting(partestSuite)
  ).dependsOn(partest % "test", library)

  lazy val scalaTestSuite: MultiScalaProject = MultiScalaProject(
      id = "scalaTestSuite", base = file("scala-test-suite")
  ).enablePlugins(
      MyScalaJSPlugin
  ).settings(
      commonSettings,
      publishArtifact in Compile := false,
      NoIDEExport.noIDEExportSettings,

      testOptions += Tests.Argument(TestFrameworks.JUnit, "-a", "-s"),
  ).zippedSettings(partest)(partest =>
      unmanagedSources in Compile ++= {
        val scalaV = scalaVersion.value
        val upstreamSrcDir = (fetchScalaSource in partest).value

        if (scalaV.startsWith("2.11.") ||
            scalaV.startsWith("2.12.")) {
          Nil
        } else {
          List(upstreamSrcDir / "src/testkit/scala/tools/testkit/AssertUtil.scala")
        }
      },
  ).zippedSettings(partest)(partest =>
      unmanagedSources in Test ++= {
        val blacklist: Set[String] = {
          val file = (resourceDirectory in Test).value / scalaVersion.value / "BlacklistedTests.txt"
          scala.io.Source.fromFile(file)
            .getLines()
            .filter(l => l.nonEmpty && !l.startsWith("#"))
            .toSet
        }

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
        val nonexistentBlacklisted = blacklist.diff(allClasses)
        if (nonexistentBlacklisted.nonEmpty) {
          throw new AssertionError(
              s"Sources not found for blacklisted tests:\n$nonexistentBlacklisted")
        }

        scalaScalaJUnitSources.collect {
          case (rel, file) if !blacklist.contains(rel) => file
        }
      }
  ).withScalaJSCompiler.withScalaJSJUnitPlugin.dependsOn(jUnitRuntime, testBridge % "test")

}
