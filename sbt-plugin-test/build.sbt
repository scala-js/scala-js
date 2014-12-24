name := "Scala.js sbt test"

version := scalaJSVersion

val versionSettings = Seq(
  version := scalaJSVersion,
  scalaVersion := "2.11.2"
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

// Test %%% macro - #1331
val unusedSettings = Seq(
  libraryDependencies += "org.example" %%% "dummy" % "0.1"
)
