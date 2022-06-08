inThisBuild(version := scalaJSVersion)
inThisBuild(scalaVersion := "2.12.16")

lazy val root = project.in(file(".")).
  aggregate(multiTestJS, multiTestJVM)

lazy val testFrameworkCommonSettings = Def.settings(
    name := "Dummy cross JS/JVM test framework",
    unmanagedSourceDirectories in Compile +=
      baseDirectory.value.getParentFile / "shared/src/main/scala"
)

lazy val testFrameworkJS = project.in(file("testFramework/js")).
  enablePlugins(ScalaJSPlugin).
  settings(
      testFrameworkCommonSettings,
      libraryDependencies +=
        "org.scala-js" %% "scalajs-test-interface" % scalaJSVersion
  )

lazy val testFrameworkJVM = project.in(file("testFramework/jvm")).
  settings(
      testFrameworkCommonSettings,
      libraryDependencies += "org.scala-sbt" % "test-interface" % "1.0"
  )

lazy val multiTestCommonSettings = Def.settings(
    unmanagedSourceDirectories in Compile +=
      baseDirectory.value.getParentFile / "shared/src/main/scala",
    unmanagedSourceDirectories in Test +=
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
