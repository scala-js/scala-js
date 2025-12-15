version := scalaJSVersion
scalaVersion := "2.12.20"

enablePlugins(ScalaJSPlugin)

// Test that non-existent classpath entries are allowed - #2198
Compile / fullClasspath += baseDirectory.value /
  "non-existent-directory-please-dont-ever-create-this"

scalaJSUseMainModuleInitializer := true
