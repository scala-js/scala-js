inThisBuild(Def.settings(
  scalaVersion := "2.12.21",
))

lazy val sharedSettings = Def.settings(
  Compile / unmanagedSourceDirectories += baseDirectory.value.getParentFile / "shared/src/main/scala",
  Test / unmanagedSourceDirectories += baseDirectory.value.getParentFile / "shared/src/test/scala",

  Test / testOptions += Tests.Argument(TestFrameworks.JUnit, "-a", "-s", "-v"),

  envVars += "PROJECT_ENV_VAR" -> "scoped in project",
  run / envVars += "RUN_ENV_VAR" -> "scoped in project/run", // has no effect
  Compile / envVars += "COMPILE_ENV_VAR" -> "scoped in project/Compile",
  Compile / run / envVars += "COMPILE_RUN_ENV_VAR" -> "scoped in project/Compile/run",
  Test / envVars += "TEST_ENV_VAR" -> "scoped in project/Test",
  Test / test / envVars += "TEST_TEST_ENV_VAR" -> "scoped in project/Test/test", // has no effect
)

// Confidence tests on the JVM
lazy val jvmReference = project
  .in(file("jvm"))
  .settings(
    sharedSettings,
    libraryDependencies ++= Seq(
      "com.novocode" % "junit-interface" % "0.11" % "test",
      "junit" % "junit" % "4.13.2" % "test",
    ),
    fork := true,
  )

// Actual tests on JS
lazy val jsTests = project
  .in(file("js"))
  .enablePlugins(ScalaJSPlugin, ScalaJSJUnitPlugin)
  .settings(
    sharedSettings,
    scalaJSUseMainModuleInitializer := true,
  )
