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
    // Use PhantomJS, allow cross domain requests
    postLinkJSEnv := PhantomJSEnv(args = Seq("--web-security=no")).value,
    Jetty9Test.runSetting
  )

val testFrameworkSettings = Seq(
  name := "Dummy cross JS/JVM test framework",
  unmanagedSourceDirectories in Compile +=
    baseDirectory.value / ".." / "src" / "main" / "scala"
)

lazy val testFrameworkJS = project.in(file("testFramework/.js")).
  enablePlugins(ScalaJSPlugin).
  settings(versionSettings: _*).
  settings(testFrameworkSettings: _*).
  settings(
    libraryDependencies +=
      "org.scala-js" %% "scalajs-test-interface" % scalaJSVersion
  )

lazy val testFrameworkJVM = project.in(file("testFramework/.jvm")).
  settings(versionSettings: _*).
  settings(testFrameworkSettings: _*).
  settings(
    libraryDependencies ++= Seq(
        "org.scala-sbt" % "test-interface" % "1.0",
        "org.scala-js" %% "scalajs-stubs" % scalaJSVersion % "provided"
    )
  )

val multiTestSettings = Seq(
  testFrameworks ++= Seq(
      TestFramework("sbttest.framework.DummyFramework"),
      TestFramework("inexistent.Foo", "another.strange.Bar")
  ),
  unmanagedSourceDirectories in Compile +=
    baseDirectory.value / ".." / "shared" / "src" / "main" / "scala",
  unmanagedSourceDirectories in Test +=
    baseDirectory.value / ".." / "shared" / "src" / "test" / "scala"
)

lazy val multiTestJS = project.in(file("multiTest/js")).
  enablePlugins(ScalaJSPlugin).
  settings(baseSettings: _*).
  settings(multiTestSettings: _*).
  settings(name := "Multi test framework test JS").
  dependsOn(testFrameworkJS % "test")

lazy val multiTestJVM = project.in(file("multiTest/jvm")).
  settings(versionSettings: _*).
  settings(multiTestSettings: _*).
  settings(
    name := "Multi test framework test JVM",
    libraryDependencies += "com.novocode" % "junit-interface" % "0.9" % "test"
  ).
  dependsOn(testFrameworkJVM % "test")

// Test %%% macro - #1331
val unusedSettings = Seq(
  libraryDependencies += "org.example" %%% "dummy" % "0.1"
)
