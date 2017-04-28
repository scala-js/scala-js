lazy val referencedCrossProject = crossProject.
  crossType(CrossType.Pure).
  in(file(".")).
  settings(scalaVersion := "2.11.11")

lazy val referencedCrossProjectJS = referencedCrossProject.js
lazy val referencedCrossProjectJVM = referencedCrossProject.jvm
