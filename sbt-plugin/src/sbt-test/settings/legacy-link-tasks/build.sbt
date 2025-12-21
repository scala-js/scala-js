val checkNoClosure = taskKey[Unit]("Check that fullOptJS wasn't run with closure")

version := scalaJSVersion
scalaVersion := "2.12.21"

enablePlugins(ScalaJSPlugin)

scalaJSUseMainModuleInitializer := true

(artifactPath in fastOptJS in Compile) :=
  baseDirectory.value / "my-fast.js"

(artifactPath in fullOptJS in Compile) :=
  baseDirectory.value / "my-full.js"

(scalaJSLinkerConfig in fullOptJS in Compile) ~= (_.withClosureCompiler(false))

checkNoClosure := {
  val file = (artifactPath in fullOptJS in Compile).value
  // Check stuff wasn't renamed.
  assert(IO.read(file).contains("org_scalajs_sbtplugin_test"))
}
