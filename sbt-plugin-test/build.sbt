import org.scalajs.core.tools.io._
import org.scalajs.core.tools.jsdep.ManifestFilters
import org.scalajs.jsenv.nodejs.JSDOMNodeJSEnv
import org.scalajs.sbtplugin.ScalaJSPluginInternal._
import org.scalajs.sbtplugin.Loggers.sbtLogger2ToolsLogger

lazy val concurrentFakeFullOptJS = taskKey[Any]("")
lazy val concurrentUseOfLinkerTest = taskKey[Any]("")

name := "Scala.js sbt test"

version := scalaJSVersion

val versionSettings = Seq(
  version := scalaJSVersion,
  scalaVersion := "2.11.8"
)

val baseSettings = versionSettings ++ Seq(
  testOptions += Tests.Argument(
    TestFramework("com.novocode.junit.JUnitFramework"), "-v", "-a", "-s"),

  // Test that non-existent classpath entries are allowed - #2198
  fullClasspath in Compile += (baseDirectory in "root").value /
    "non-existent-directory-please-dont-ever-create-this"
)

val regressionTestForIssue2243 = TaskKey[Unit]("regressionTestForIssue2243",
  "", KeyRanks.BTask)
val testScalaJSSourceMapAttribute = TaskKey[Unit](
  "testScalaJSSourceMapAttribute", "", KeyRanks.BTask)

def withRegretionTestForIssue2243(project: Project): Project = {
  project.settings(inConfig(Compile)(Seq(
    regressionTestForIssue2243 := {
      // Regression test for issue #2243
      val _ = Def.sequential(packageJSDependencies in Compile,
          packageMinifiedJSDependencies in Compile).value
      assert((artifactPath in(Compile, packageJSDependencies)).value.exists)
      assert((artifactPath in(Compile, packageMinifiedJSDependencies)).value.exists)
      streams.value.log.info("Regression test for issue #2243 passed")
    }
  )): _*)
}

lazy val referencedCrossProjectJS = ProjectRef(file("referencedCrossProject"), "referencedCrossProjectJS")
lazy val referencedCrossProjectJVM = ProjectRef(file("referencedCrossProject"), "referencedCrossProjectJVM")

lazy val root = project.in(file(".")).
  aggregate(noDOM, withDOM, multiTestJS, multiTestJVM, referencedCrossProjectJS, referencedCrossProjectJVM)

lazy val noDOM = project.settings(baseSettings: _*).
  enablePlugins(ScalaJSPlugin).
  enablePlugins(ScalaJSJUnitPlugin).
  settings(
    name := "Scala.js sbt test w/o DOM",
    scalaJSOutputWrapper := (
        "// Scala.js - noDOM sbt test\n//\n// Compiled with Scala.js\n",
        "// End of Scala.js generated script"),
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

lazy val withDOM = project.settings(baseSettings: _*).
  enablePlugins(ScalaJSPlugin).
  enablePlugins(ScalaJSJUnitPlugin).
  settings(
    name := "Scala.js sbt test w/ DOM",
    jsEnv := new JSDOMNodeJSEnv(),
    scalaJSOutputWrapper := (
        "// Scala.js - withDOM sbt test\n//\n// Compiled with Scala.js\n",
        "// End of Scala.js generated script"),
    scalaJSUseMainModuleInitializer := true
  )

lazy val jetty9 = project.settings(baseSettings: _*).
  enablePlugins(ScalaJSPlugin).
  settings(
    name := "Scala.js sbt test with jetty9 on classpath",
    // Use PhantomJS, allow cross domain requests
    jsEnv := PhantomJSEnv(args = Seq("--web-security=no")).value,
    Jetty9Test.runSetting
  )

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
    jsDependencies in Test += ProvidedJS / "consoleWriter.js",

    // Test isScalaJSProject (as a setting, it's evaluated when loading the build)
    isScalaJSProject ~= { value =>
      assert(value, "isScalaJSProject should be true in multiTestJS")
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

    // Test isScalaJSProject (as a setting, it's evaluated when loading the build)
    isScalaJSProject ~= { value =>
      assert(!value, "isScalaJSProject should be true in multiTestJVM")
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

lazy val jsDependenciesTestDependee = project.
  settings(versionSettings: _*).
  enablePlugins(ScalaJSPlugin).
  settings(
    // This project contains some jsDependencies to test in jsDependenciesTest
    jsDependencies ++= Seq(
        RuntimeDOM,
        // The jsDependenciesTest relies on this jQuery dependency
        // If you change it, make sure we still test properly
        "org.webjars" % "jquery" % "1.10.2" / "jquery.js"
    )
  )

lazy val jsDependenciesTest = withRegretionTestForIssue2243(
  project.settings(versionSettings: _*).
  enablePlugins(ScalaJSPlugin).
  settings(
    jsDependencies ++= Seq(
        "org.webjars" % "historyjs" % "1.8.0" / "uncompressed/history.js",
        ProvidedJS / "some-jquery-plugin.js" dependsOn "1.10.2/jquery.js",
        ProvidedJS / "js/foo.js" dependsOn "uncompressed/history.js",

        // cause a circular dependency error if both "history.js"'s are considered equal
        "org.webjars" % "historyjs" % "1.8.0" / "compressed/history.js" dependsOn "foo.js",

        // cause a duplicate commonJSName if the following are not considered equal
        "org.webjars" % "mustachejs" % "0.8.2" / "mustache.js" commonJSName "Mustache",
        "org.webjars" % "mustachejs" % "0.8.2" / "0.8.2/mustache.js" commonJSName "Mustache",

        // cause an ambiguity with the jQuery dependency from the
        // jsDependenciesTestDependee project (if we don't filter)
        ProvidedJS / "js/customJQuery/jquery.js" dependsOn "1.10.2/jquery.js",

        // Test minified dependencies
        "org.webjars" % "immutable" % "3.4.0" / "immutable.js" minified "immutable.min.js"
    ),
    jsManifestFilter := {
      ManifestFilters.reinterpretResourceNames("jsDependenciesTestDependee")(
          "jquery.js" -> "1.10.2/jquery.js")
    }
  ).
  settings(inConfig(Compile)(Seq(
    packageJSDependencies <<= packageJSDependencies.dependsOn(Def.task {
      // perform verifications on the ordering and deduplications
      val resolvedDeps = resolvedJSDependencies.value.data
      val relPaths = resolvedDeps.map(_.info.relPath)

      assert(relPaths.toSet == Set(
          "META-INF/resources/webjars/mustachejs/0.8.2/mustache.js",
          "META-INF/resources/webjars/historyjs/1.8.0/scripts/uncompressed/history.js",
          "META-INF/resources/webjars/historyjs/1.8.0/scripts/compressed/history.js",
          "META-INF/resources/webjars/jquery/1.10.2/jquery.js",
          "META-INF/resources/webjars/immutable/3.4.0/immutable.js",
          "js/foo.js",
          "js/some-jquery-plugin.js",
          "js/customJQuery/jquery.js"),
          s"Bad set of relPathes: ${relPaths.toSet}")

      val minifiedRelPaths = resolvedDeps.flatMap(_.info.relPathMinified)

      assert(minifiedRelPaths.toSet == Set(
          "META-INF/resources/webjars/immutable/3.4.0/immutable.min.js"),
          s"Bad set of minifiedRelPathes: ${minifiedRelPaths.toSet}")

      val jQueryIndex = relPaths.indexWhere(_ endsWith "1.10.2/jquery.js")
      val jQueryPluginIndex = relPaths.indexWhere(_ endsWith "/some-jquery-plugin.js")
      assert(jQueryPluginIndex > jQueryIndex,
          "the jQuery plugin appears before jQuery")

      val uncompressedHistoryIndex = relPaths.indexWhere(_ endsWith "/uncompressed/history.js")
      val fooIndex = relPaths.indexWhere(_ endsWith "/foo.js")
      val compressedHistoryIndex = relPaths.indexWhere(_ endsWith "/compressed/history.js")
      assert(fooIndex > uncompressedHistoryIndex,
          "foo.js appears before uncompressed/history.js")
      assert(compressedHistoryIndex > fooIndex,
          "compressed/history.js appears before foo.js")

      streams.value.log.info("jsDependencies resolution test passed")
    })
  )): _*).
  dependsOn(jsDependenciesTestDependee) // depends on jQuery
)

lazy val jsNoDependenciesTest = withRegretionTestForIssue2243(
  project.settings(versionSettings: _*).
  enablePlugins(ScalaJSPlugin)
)

// Test %%% macro - #1331
val unusedSettings = Seq(
  libraryDependencies += "org.example" %%% "dummy" % "0.1"
)
