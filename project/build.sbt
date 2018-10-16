addSbtPlugin("de.heikoseeberger" % "sbt-header" % "5.0.0")

addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "0.1.18")

addSbtPlugin("org.scalastyle" % "scalastyle-sbt-plugin" % "1.0.0")

addSbtPlugin("org.portable-scala" % "sbt-platform-deps" % "1.0.0")

libraryDependencies += "com.google.javascript" % "closure-compiler" % "v20160517"

libraryDependencies += "org.eclipse.jgit" % "org.eclipse.jgit.pgm" % "3.2.0.201312181205-r"

unmanagedSourceDirectories in Compile ++= {
  val root = baseDirectory.value.getParentFile
  Seq(
    root / "ir/src/main/scala",
    root / "io/shared/src/main/scala",
    root / "io/jvm/src/main/scala",
    root / "logging/shared/src/main/scala",
    root / "logging/jvm/src/main/scala",
    root / "linker/shared/src/main/scala",
    root / "linker/jvm/src/main/scala",
    root / "js-envs/src/main/scala",
    root / "nodejs-env/src/main/scala",
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
    baseDirectory.value.getParentFile,
    (sourceManaged in Compile).value)
}.taskValue

unmanagedResourceDirectories in Compile += {
  val root = baseDirectory.value.getParentFile
  root / "test-adapter/src/main/resources"
}
