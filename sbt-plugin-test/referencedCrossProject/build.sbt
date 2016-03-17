lazy val referencedCrossProject = crossProject.
  crossType(CrossType.Pure).
  in(file(".")).
  settings(scalaVersion := "2.11.8")

lazy val referencedCrossProjectJS = referencedCrossProject.js
lazy val referencedCrossProjectJVM = referencedCrossProject.jvm
