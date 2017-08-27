import org.scalajs.core.tools.io._
import org.scalajs.sbtplugin.Loggers.sbtLogger2ToolsLogger

lazy val concurrentFakeFullOptJS = taskKey[Any]("")
lazy val concurrentUseOfLinkerTest = taskKey[Any]("")

name := "Scala.js sbt test"

version := scalaJSVersion

val versionSettings = Seq(
  version := scalaJSVersion,
  scalaVersion := "2.11.11"
)

val baseSettings = versionSettings ++ Seq(
  testOptions += Tests.Argument(
    TestFramework("com.novocode.junit.JUnitFramework"), "-v", "-a", "-s"),

  // Test that non-existent classpath entries are allowed - #2198
  fullClasspath in Compile += (baseDirectory in LocalProject("root")).value /
    "non-existent-directory-please-dont-ever-create-this"
)

val testScalaJSSourceMapAttribute = TaskKey[Unit](
  "testScalaJSSourceMapAttribute", "", KeyRanks.BTask)

lazy val referencedCrossProjectJS = ProjectRef(file("referencedCrossProject"), "referencedCrossProjectJS")
lazy val referencedCrossProjectJVM = ProjectRef(file("referencedCrossProject"), "referencedCrossProjectJVM")

lazy val root = project.in(file(".")).
  aggregate(noDOM, multiTestJS, multiTestJVM, referencedCrossProjectJS, referencedCrossProjectJVM)

lazy val noDOM = project.settings(baseSettings: _*).
  enablePlugins(ScalaJSPlugin).
  enablePlugins(ScalaJSJUnitPlugin).
  settings(
    name := "Scala.js sbt test w/o DOM",
    scalaJSUseMainModuleInitializer := true
  ).
  /* This hopefully exposes concurrent uses of the linker. If it fails/gets
   * flaky, there is a bug somewhere - #2202
   */
  settings(inConfig(Compile)(Seq(
      // A fake fullOptJS that we will run concurrently with the true fullOptJS
      concurrentFakeFullOptJS := Def.taskDyn {
        val s = (streams in fullOptJS).value
        val log = s.log
        val ir = (scalaJSIR in fullOptJS).value.data
        val moduleInitializers = scalaJSModuleInitializers.value

        Def.task {
          log.info("Fake full optimizing")
          val linker = (scalaJSLinker in fullOptJS).value
          linker.link(ir, moduleInitializers,
              WritableMemVirtualJSFile("fake-fastopt.js"),
              sbtLogger2ToolsLogger(log))
        }.tag((usesScalaJSLinkerTag in fullOptJS).value)
      }.value,

      /* Depend on both fullOptJS and concurrentFakeFullOptJS, so that they
       * are hopefully executed in parallel (potentially, but they should be
       * blocked from actually doing so by the concurrent restrictions on
       * usesScalaJSLinkerTag).
       */
      concurrentUseOfLinkerTest := {
        (fullOptJS.value, concurrentFakeFullOptJS.value)
      }
  )))

lazy val testFramework = crossProject.crossType(CrossType.Pure).
  settings(versionSettings: _*).
  settings(name := "Dummy cross JS/JVM test framework").
  jsSettings(
    libraryDependencies +=
      "org.scala-js" %% "scalajs-test-interface" % scalaJSVersion
  ).
  jvmSettings(
    libraryDependencies ++= Seq(
        "org.scala-sbt" % "test-interface" % "1.0",
        "org.scala-js" %% "scalajs-stubs" % scalaJSVersion % "provided"
    )
  )

lazy val testFrameworkJS = testFramework.js
lazy val testFrameworkJVM = testFramework.jvm

lazy val multiTest = crossProject.
  jsConfigure(_.enablePlugins(ScalaJSJUnitPlugin)).
  settings(
    testFrameworks ++= Seq(
        TestFramework("sbttest.framework.DummyFramework"),
        TestFramework("inexistent.Foo", "another.strange.Bar")
    )
  ).
  jsSettings(baseSettings: _*).
  jsSettings(
    name := "Multi test framework test JS",

    // Make FrameworkDetector resilient to other output - #1572
    jsExecutionFiles in Test := {
      val consoleWriter = FileVirtualJSFile(
          (resourceDirectory in Test).value / "consoleWriter.js")
      consoleWriter +: (jsExecutionFiles in Test).value
    },

    // Test crossPlatform (as a setting, it's evaluated when loading the build)
    crossPlatform ~= { value =>
      assert(value == JSPlatform,
          "crossPlatform should be JSPlatform in multiTestJS")
      value
    },

    // Test the scalaJSSourceMap attribute of fastOptJS and fullOptJS
    testScalaJSSourceMapAttribute in Test := {
      val fastOptFile = (fastOptJS in Test).value
      assert(fastOptFile.get(scalaJSSourceMap).exists {
        _.getPath == fastOptFile.data.getPath + ".map"
      }, "fastOptJS does not have the correct scalaJSSourceMap attribute")

      val fullOptFile = (fullOptJS in Test).value
      assert(fullOptFile.get(scalaJSSourceMap).exists {
        _.getPath == fullOptFile.data.getPath + ".map"
      }, "fullOptJS does not have the correct scalaJSSourceMap attribute")
    }
  ).
  jvmSettings(versionSettings: _*).
  jvmSettings(
    name := "Multi test framework test JVM",
    libraryDependencies +=
      "com.novocode" % "junit-interface" % "0.9" % "test",

    // Test crossPlatform (as a setting, it's evaluated when loading the build)
    crossPlatform ~= { value =>
      assert(value == JVMPlatform,
          "crossPlatform should be JVMPlatform in multiTestJVM")
      value
    }
  ).
  settings(
    // Scala cross-version support for shared source directory - #2005
    unmanagedSourceDirectories in Compile := {
      val srcDirs = (unmanagedSourceDirectories in Compile).value
      val version = scalaBinaryVersion.value
      val expected =
        (baseDirectory.value.getParentFile / "shared" / "src" / "main" / s"scala-$version").getPath
      assert(srcDirs.exists(_.getPath == expected))
      srcDirs
    }
  ).
  dependsOn(testFramework % "test")

lazy val multiTestJS = multiTest.js
lazy val multiTestJVM = multiTest.jvm

// Test %%% macro - #1331
val unusedSettings = Seq(
  libraryDependencies += "org.example" %%% "dummy" % "0.1"
)
