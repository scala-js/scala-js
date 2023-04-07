addSbtPlugin("de.heikoseeberger" % "sbt-header" % "5.9.0")

addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "0.8.1")

addSbtPlugin("org.scalastyle" % "scalastyle-sbt-plugin" % "1.0.0")

addSbtPlugin("org.portable-scala" % "sbt-platform-deps" % "1.0.1")

addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.11.0")

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
  )
}

Compile / unmanagedResourceDirectories += {
  val root = baseDirectory.value.getParentFile
  root / "test-adapter/src/main/resources"
}

/* Don't warn for using the 'in' syntax instead of the '/' syntax.
 * We cannot get rid of it in the sbt plugin, whose sources we use in the build.
 */
scalacOptions += "-Wconf:msg=method in in trait ScopingSetting is deprecated:s"
