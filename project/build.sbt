addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.5")

addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "0.1.18")

addSbtPlugin("org.scalastyle" % "scalastyle-sbt-plugin" % "1.0.0")

addSbtPlugin("org.portable-scala" % "sbt-platform-deps" % "1.0.0-M2")

libraryDependencies += "org.scala-js" % "closure-compiler-java-6" % "v20160517"

libraryDependencies += "org.eclipse.jgit" % "org.eclipse.jgit.pgm" % "3.2.0.201312181205-r"

unmanagedSourceDirectories in Compile ++= {
  val root = baseDirectory.value.getParentFile
  Seq(
    root / "ir/src/main/scala",
    root / "tools/shared/src/main/scala",
    root / "tools/jvm/src/main/scala",
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
    baseDirectory.value.getParentFile / "tools",
    (sourceManaged in Compile).value)
}.taskValue

unmanagedResourceDirectories in Compile += {
  val root = baseDirectory.value.getParentFile
  root / "test-adapter/src/main/resources"
}
