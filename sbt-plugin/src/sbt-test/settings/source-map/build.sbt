import org.scalajs.linker.interface.ModuleInitializer

val check = taskKey[Unit]("Run checks of this test")

version := scalaJSVersion
scalaVersion := "2.12.11"

enablePlugins(ScalaJSPlugin)

check := {
  val fastOptFile = (fastOptJS in Test).value
  assert(fastOptFile.get(scalaJSSourceMap).exists {
    _.getPath == fastOptFile.data.getPath + ".map"
  }, "fastOptJS does not have the correct scalaJSSourceMap attribute")

  val fullOptFile = (fullOptJS in Test).value
  assert(fullOptFile.get(scalaJSSourceMap).exists {
    _.getPath == fullOptFile.data.getPath + ".map"
  }, "fullOptJS does not have the correct scalaJSSourceMap attribute")
}
