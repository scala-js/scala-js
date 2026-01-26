import org.scalajs.linker.interface.ModuleInitializer
import sbtcompat.PluginCompat

val check = taskKey[Unit]("Run checks of this test")

version := scalaJSVersion
scalaVersion := "2.12.21"

enablePlugins(ScalaJSPlugin)

def sourceMapFile[A](file: Attributed[A]): Option[java.io.File] = {
  PluginCompat.attributedGetFile(file, scalaJSSourceMap)
}

check := {
  val fastOptFile = (Test / fastOptJS).value
  assert(sourceMapFile(fastOptFile).exists {
    _.getPath == fastOptFile.data.getPath + ".map"
  }, "fastOptJS does not have the correct scalaJSSourceMap attribute")

  val fullOptFile = (Test / fullOptJS).value
  assert(sourceMapFile(fullOptFile).exists {
    _.getPath == fullOptFile.data.getPath + ".map"
  }, "fullOptJS does not have the correct scalaJSSourceMap attribute")
}
