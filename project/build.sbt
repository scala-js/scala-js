addSbtPlugin("de.heikoseeberger" % "sbt-header" % "5.9.0")

addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "0.8.1")

addSbtPlugin("org.scalastyle" % "scalastyle-sbt-plugin" % "1.0.0")

addSbtPlugin("org.portable-scala" % "sbt-platform-deps" % "1.0.2")

addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.11.0")

addSbtPlugin("com.github.sbt" % "sbt-pgp" % "2.3.1")

libraryDependencies += "com.google.jimfs" % "jimfs" % "1.1"

libraryDependencies += "org.eclipse.jgit" % "org.eclipse.jgit.pgm" % "3.2.0.201312181205-r"

libraryDependencies += "org.scala-js" %% "scalajs-js-envs" % "1.4.0"
libraryDependencies += "org.scala-js" %% "scalajs-env-nodejs" % "1.4.0"

Compile / unmanagedSourceDirectories ++= {
  val root = baseDirectory.value.getParentFile
  Seq(
    root / "ir/shared/src/main/scala",
    root / "ir/jvm/src/main/scala",
    root / "linker-interface/shared/src/main/scala",
    root / "linker-interface/jvm/src/main/scala",
    root / "test-adapter/src/main/scala",
    root / "test-common/src/main/scala",
    root / "sbt-plugin/src/main/scala",
    root / "sbt-plugin/src/main/scala-2.12",
  )
}

Compile / unmanagedResourceDirectories += {
  val root = baseDirectory.value.getParentFile
  root / "test-adapter/src/main/resources"
}
