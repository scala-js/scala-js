addSbtPlugin("de.heikoseeberger" % "sbt-header" % "5.0.0")

addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "0.1.18")

addSbtPlugin("org.scalastyle" % "scalastyle-sbt-plugin" % "1.0.0")

addSbtPlugin("org.portable-scala" % "sbt-platform-deps" % "1.0.0")

addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.4.0")

libraryDependencies += "com.google.jimfs" % "jimfs" % "1.1"

libraryDependencies += "org.eclipse.jgit" % "org.eclipse.jgit.pgm" % "3.2.0.201312181205-r"

libraryDependencies += "org.scala-js" %% "scalajs-js-envs" % "1.1.1"
libraryDependencies += "org.scala-js" %% "scalajs-env-nodejs" % "1.1.1"

unmanagedSourceDirectories in Compile ++= {
  val root = baseDirectory.value.getParentFile
  Seq(
    root / "ir/src/main/scala",
    root / "linker-interface/shared/src/main/scala",
    root / "linker-interface/jvm/src/main/scala",
    root / "test-adapter/src/main/scala",
    root / "test-common/src/main/scala",
    root / "sbt-plugin/src/main/scala",
  )
}

unmanagedResourceDirectories in Compile += {
  val root = baseDirectory.value.getParentFile
  root / "test-adapter/src/main/resources"
}
