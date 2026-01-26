inThisBuild(version := scalaJSVersion)
inThisBuild(scalaVersion := "2.12.21")

lazy val root = project.in(file(".")).
  aggregate(multiTestJS, multiTestJVM)

lazy val testFrameworkCommonSettings = Def.settings(
    Compile / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "shared/src/main/scala"
)

lazy val testFrameworkJS = project.in(file("testFramework/js")).
  enablePlugins(ScalaJSPlugin).
  settings(
      name := "Dummy cross JS/JVM test framework JS",
      testFrameworkCommonSettings,
      // Use % with explicit suffix because scalajs-test-interface is published
      // without _sjs1, but sbt 2.x's %% adds _sjs1
      libraryDependencies +=
        "org.scala-js" % ("scalajs-test-interface_" + scalaBinaryVersion.value) % scalaJSVersion
  )

lazy val testFrameworkJVM = project.in(file("testFramework/jvm")).
  settings(
      name := "Dummy cross JS/JVM test framework JVM",
      testFrameworkCommonSettings,
      libraryDependencies += "org.scala-sbt" % "test-interface" % "1.0"
  )

lazy val multiTestCommonSettings = Def.settings(
    Compile / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "shared/src/main/scala",
    Test / unmanagedSourceDirectories +=
      baseDirectory.value.getParentFile / "shared/src/test/scala",

    testFrameworks ++= Seq(
        TestFramework("sbttest.framework.DummyFramework"),
        TestFramework("inexistent.Foo", "another.strange.Bar")
    ),

    testOptions += Tests.Argument(
      TestFramework("com.novocode.junit.JUnitFramework"), "-v", "-a", "-s")
)

lazy val multiTestJS = project.in(file("multiTest/js")).
  enablePlugins(ScalaJSPlugin).
  enablePlugins(ScalaJSJUnitPlugin).
  settings(multiTestCommonSettings).
  settings(
    name := "Multi test framework test JS",
  ).
  dependsOn(testFrameworkJS % "test")

lazy val multiTestJVM = project.in(file("multiTest/jvm")).
  settings(multiTestCommonSettings).
  settings(
    name := "Multi test framework test JVM",
    libraryDependencies +=
      "com.novocode" % "junit-interface" % "0.9" % "test",
  ).
  dependsOn(testFrameworkJVM % "test")
