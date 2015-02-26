name := "Scala.js sbt test"

version := scalaJSVersion

val versionSettings = Seq(
  version := scalaJSVersion,
  scalaVersion := "2.11.5"
)

val baseSettings = versionSettings ++ Seq(
  libraryDependencies +=
    "org.scala-js" %% "scalajs-jasmine-test-framework" % scalaJSVersion % "test"
)

lazy val root = project.in(file(".")).
  aggregate(noDOM, withDOM, multiTestJS, multiTestJVM)

lazy val noDOM = project.settings(baseSettings: _*).
  enablePlugins(ScalaJSPlugin).
  settings(
    name := "Scala.js sbt test w/o DOM"
  )

lazy val withDOM = project.settings(baseSettings: _*).
  enablePlugins(ScalaJSPlugin).
  settings(
    name := "Scala.js sbt test w/ DOM",
    jsDependencies ++= Seq(
        RuntimeDOM,
        "org.webjars" % "jquery" % "1.10.2" / "jquery.js"
    )
  )

lazy val jetty9 = project.settings(baseSettings: _*).
  enablePlugins(ScalaJSPlugin).
  settings(
    name := "Scala.js sbt test with jetty9 on classpath",
    jsDependencies ++= Seq(
        RuntimeDOM,
        "org.webjars" % "jquery" % "1.10.2" / "jquery.js"
    ),
    // A test for packageJSDependencies, although we don't use it
    skip in packageJSDependencies := false,
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
  settings(
    testFrameworks ++= Seq(
        TestFramework("sbttest.framework.DummyFramework"),
        TestFramework("inexistent.Foo", "another.strange.Bar")
    )
  ).
  jsSettings(baseSettings: _*).
  jsSettings(
    name := "Multi test framework test JS"
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
        "org.webjars" % "mustachejs" % "0.8.2" / "0.8.2/mustache.js" commonJSName "Mustache"
    )
  ).
  settings(inConfig(Compile)(Seq(
    skip in packageJSDependencies := false,
    packageJSDependencies <<= packageJSDependencies.dependsOn(Def.task {
      // perform verifications on the ordering and deduplications
      val cp = scalaJSPreLinkClasspath.value
      val relPaths = cp.jsLibs.map(_.info.relPath)

      assert(relPaths.toSet == Set(
          "META-INF/resources/webjars/mustachejs/0.8.2/mustache.js",
          "META-INF/resources/webjars/historyjs/1.8.0/scripts/uncompressed/history.js",
          "META-INF/resources/webjars/historyjs/1.8.0/scripts/compressed/history.js",
          "META-INF/resources/webjars/jquery/1.10.2/jquery.js",
          "js/foo.js",
          "js/some-jquery-plugin.js"),
          s"Bad set of relPathes: ${relPaths.toSet}")

      val jQueryIndex = relPaths.indexWhere(_ endsWith "/jquery.js")
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
