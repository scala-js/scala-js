import sbt.internal.util.StringAttributeKey

@transient
val check = taskKey[Unit]("Run checks of this test")

version := scalaJSVersion
scalaVersion := "2.12.21"

enablePlugins(ScalaJSPlugin)

check := {
  val fastOptFile = (Test / fastOptJS).value
  val fastOptPath = fastOptFile.data
  // In sbt 2.x, scalaJSSourceMap is stored with StringAttributeKey
  val fastSourceMap = fastOptFile.get(StringAttributeKey(scalaJSSourceMap.label)).map(new java.io.File(_))
  assert(fastSourceMap.exists {
    _.getPath == fastOptPath.getPath + ".map"
  }, "fastOptJS does not have the correct scalaJSSourceMap attribute")

  val fullOptFile = (Test / fullOptJS).value
  val fullOptPath = fullOptFile.data
  val fullSourceMap = fullOptFile.get(StringAttributeKey(scalaJSSourceMap.label)).map(new java.io.File(_))
  assert(fullSourceMap.exists {
    _.getPath == fullOptPath.getPath + ".map"
  }, "fullOptJS does not have the correct scalaJSSourceMap attribute")
}
