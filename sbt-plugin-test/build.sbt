import org.scalajs.core.tools.jsdep.ManifestFilters

name := "Scala.js sbt test"

version := scalaJSVersion

val versionSettings = Seq(
  version := scalaJSVersion,
  scalaVersion := "2.11.7"
)

val baseSettings = versionSettings ++ Seq(
  libraryDependencies +=
    "org.scala-js" %% "scalajs-jasmine-test-framework" % scalaJSVersion % "test"
)

lazy val referencedCrossProjectJS = ProjectRef(file("referencedCrossProject"), "referencedCrossProjectJS")
lazy val referencedCrossProjectJVM = ProjectRef(file("referencedCrossProject"), "referencedCrossProjectJVM")

lazy val root = project.in(file(".")).
  aggregate(noDOM, withDOM, multiTestJS, multiTestJVM, referencedCrossProjectJS, referencedCrossProjectJVM)

lazy val noDOM = project.settings(baseSettings: _*).
  enablePlugins(ScalaJSPlugin).
  settings(
    name := "Scala.js sbt test w/o DOM",
    scalaJSOutputWrapper := (
        "// Scala.js - noDOM sbt test\n//\n// Compiled with Scala.js\n",
        "// End of Scala.js generated script")
  ).
  /* This hopefully exposes concurrent uses of the linker. If it fails/gets
   * flaky, there is a bug somewhere - #2202
   */
  settings(inConfig(Compile)(run <<= run.dependsOn(fastOptJS, loadedJSEnv)): _*)

lazy val withDOM = project.settings(baseSettings: _*).
  enablePlugins(ScalaJSPlugin).
  settings(
    name := "Scala.js sbt test w/ DOM",
    jsDependencies ++= Seq(
        RuntimeDOM,
        "org.webjars" % "jquery" % "1.10.2" / "jquery.js"),
    scalaJSOutputWrapper := (
        "// Scala.js - withDOM sbt test\n//\n// Compiled with Scala.js\n",
        "// End of Scala.js generated script")
  )

lazy val jetty9 = project.settings(baseSettings: _*).
  enablePlugins(ScalaJSPlugin).
  settings(
    name := "Scala.js sbt test with jetty9 on classpath",
    // This project also tests packageJSDependencies, although we don't use it
    jsDependencies ++= Seq(
        RuntimeDOM,
        // The jsDependenciesTest relies on this jQuery dependency
        // If you change it, make sure we still test properly
        "org.webjars" % "jquery" % "1.10.2" / "jquery.js"
    ),
    // Use PhantomJS, allow cross domain requests
    postLinkJSEnv := PhantomJSEnv(args = Seq("--web-security=no")).value,
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
        TestFramework("inexistent.Foo", "another.strange.Bar"),
        TestFramework("org.scalajs.jasminetest.JasmineFramework")
    )
  ).
  jsSettings(baseSettings: _*).
  jsSettings(
    name := "Multi test framework test JS",
    // Make FrameworkDetector resilient to other output - #1572
    jsDependencies in Test += ProvidedJS / "consoleWriter.js",
    testOptions += Tests.Argument(
      TestFramework("com.novocode.junit.JUnitFramework"), "-v", "-a")
  ).
  jvmSettings(versionSettings: _*).
  jvmSettings(
    name := "Multi test framework test JVM",
    libraryDependencies +=
      "com.novocode" % "junit-interface" % "0.9" % "test"
  ).
  dependsOn(testFramework % "test")

lazy val multiTestJS = multiTest.js
lazy val multiTestJVM = multiTest.jvm

lazy val jsDependenciesTest = project.settings(versionSettings: _*).
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

        // cause an ambiguity with jQuery dependency from jetty9 project (if we don't filter)
        ProvidedJS / "js/customJQuery/jquery.js" dependsOn "1.10.2/jquery.js",

        // Test minified dependencies
        "org.webjars" % "immutable" % "3.4.0" / "immutable.js" minified "immutable.min.js"
    ),
    jsManifestFilter := ManifestFilters.reinterpretResourceNames("jetty9")(
        "jquery.js" -> "1.10.2/jquery.js")
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
  dependsOn(jetty9) // depends on jQuery

// Test %%% macro - #1331
val unusedSettings = Seq(
  libraryDependencies += "org.example" %%% "dummy" % "0.1"
)
