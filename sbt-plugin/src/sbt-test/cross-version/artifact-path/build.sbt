val checkArtifactPath = taskKey[Unit]("Checks the Scala.js artifact path")

lazy val library = project
  .enablePlugins(ScalaJSPlugin)
  .settings(
    scalaVersion := "3.8.3",
    version := "0.1.0-SNAPSHOT",
    checkArtifactPath := {
      val actual = (Compile / packageBin / artifactPath).value.toString()
      val expected = "library_sjs1_3-0.1.0-SNAPSHOT.jar"

      if (!actual.endsWith(expected))
        sys.error(s"Expected path to end with $expected, got $actual")
    }
  )
