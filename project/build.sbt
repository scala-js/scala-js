libraryDependencies += "com.google.javascript" % "closure-compiler" % "v20130603"

libraryDependencies += "org.mozilla" % "rhino" % "1.7R4"

unmanagedSourceDirectories in Compile <+= baseDirectory {
  base => base.getParentFile / "sbt-plugin" / "src" / "main" / "scala"
}
