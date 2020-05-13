import org.scalajs.sbtplugin.ScalaJSCrossVersion

val check = taskKey[Unit]("Run checks of this test")

version := scalaJSVersion
scalaVersion := "2.12.11"

lazy val js = project.enablePlugins(ScalaJSPlugin).settings(
    check := {
      val value = platformDepsCrossVersion.value
      assert(value eq ScalaJSCrossVersion.binary,
          "platformDepsCrossVersion should be ScalaJSCrossVersion.binary in js")
    }
)

lazy val jvm = project.settings(
    check := {
      val value = platformDepsCrossVersion.value
      assert(value == CrossVersion.binary,
          "platformDepsCrossVersion should be CrossVersion.binary in jvm")
    }
)
