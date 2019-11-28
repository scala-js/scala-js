name := "Scala.js sbt test"

version := scalaJSVersion

val versionSettings = Seq(
  version := scalaJSVersion,
  scalaVersion := "2.11.12"
)

val baseSettings = versionSettings ++ Seq(
  testOptions += Tests.Argument(
    TestFramework("com.novocode.junit.JUnitFramework"), "-v", "-a", "-s"),

  // Test that non-existent classpath entries are allowed - #2198
  fullClasspath in Compile += (baseDirectory in LocalProject("root")).value /
    "non-existent-directory-please-dont-ever-create-this"
)

lazy val root = project.in(file(".")).
  aggregate(noDOM, multiTestJS, multiTestJVM)

lazy val noDOM = project.settings(baseSettings: _*).
  enablePlugins(ScalaJSPlugin).
  enablePlugins(ScalaJSJUnitPlugin).
  settings(
    name := "Scala.js sbt test w/o DOM",
    scalaJSUseMainModuleInitializer := true,
  )

lazy val testFrameworkCommonSettings = Def.settings(
  versionSettings,
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
    libraryDependencies ++= Seq(
        "org.scala-sbt" % "test-interface" % "1.0"
    )
  )

lazy val multiTestCommonSettings = Def.settings(
  unmanagedSourceDirectories in Compile +=
    baseDirectory.value.getParentFile / "shared/src/main/scala",
  unmanagedSourceDirectories in Test +=
    baseDirectory.value.getParentFile / "shared/src/test/scala",

  testFrameworks ++= Seq(
      TestFramework("sbttest.framework.DummyFramework"),
      TestFramework("inexistent.Foo", "another.strange.Bar")
  )
)

lazy val multiTestJS = project.in(file("multiTest/js")).
  enablePlugins(ScalaJSJUnitPlugin).
  settings(multiTestCommonSettings).
  settings(baseSettings: _*).
  settings(
    name := "Multi test framework test JS",
  ).
  dependsOn(testFrameworkJS % "test")

lazy val multiTestJVM = project.in(file("multiTest/jvm")).
  settings(multiTestCommonSettings).
  settings(versionSettings: _*).
  settings(
    name := "Multi test framework test JVM",
    libraryDependencies +=
      "com.novocode" % "junit-interface" % "0.9" % "test",
  ).
  dependsOn(testFrameworkJVM % "test")
