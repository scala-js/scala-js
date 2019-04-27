addSbtPlugin("de.heikoseeberger" % "sbt-header" % "5.0.0")

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.5")

addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "0.1.18")

addSbtPlugin("org.scalastyle" % "scalastyle-sbt-plugin" % "1.0.0")

addSbtPlugin("org.portable-scala" % "sbt-platform-deps" % "1.0.0")

libraryDependencies += "com.google.javascript" % "closure-compiler" % "v20181210"

libraryDependencies += "org.mozilla" % "rhino" % "1.7.6"

libraryDependencies += "org.webjars" % "envjs" % "1.2"

libraryDependencies += "org.eclipse.jgit" % "org.eclipse.jgit.pgm" % "3.2.0.201312181205-r"

libraryDependencies += "com.googlecode.json-simple" % "json-simple" % "1.1.1" exclude("junit", "junit")

libraryDependencies += "org.eclipse.jetty" % "jetty-websocket" % "8.1.16.v20140903"

libraryDependencies += "org.eclipse.jetty" % "jetty-server" % "8.1.16.v20140903"


unmanagedSourceDirectories in Compile ++= {
  val root = baseDirectory.value.getParentFile
  Seq(
    root / "ir/src/main/scala",
    root / "tools/shared/src/main/scala",
    root / "tools/jvm/src/main/scala",
    root / "js-envs/src/main/scala",
    root / "test-adapter/src/main/scala",
    root / "test-common/src/main/scala",
    root / "sbt-plugin/src/main/scala",
    root / "sbt-plugin/src/main/scala-sbt-0.13"
  )
}

// Add the ScalaJSEnvGenerator to the build (its in the build of the build)
sources in Compile +=
  baseDirectory.value / "project" / "ScalaJSEnvGenerator.scala"

sourceGenerators in Compile += Def.task {
  build.ScalaJSEnvGenerator.generateEnvHolder(
    baseDirectory.value.getParentFile / "tools",
    (sourceManaged in Compile).value)
}.taskValue

unmanagedResourceDirectories in Compile += {
  val root = baseDirectory.value.getParentFile
  root / "sbt-plugin/src/main/resources"
}
