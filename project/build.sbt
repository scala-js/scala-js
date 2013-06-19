libraryDependencies += "com.google.javascript" % "closure-compiler" % "v20130603"

unmanagedSourceDirectories in Compile <+= baseDirectory {
  base => base.getParentFile / "sbt-plugin" / "src" / "main" / "scala"
}
