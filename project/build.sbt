resolvers += Resolver.url(
    "bintray-sbt-plugin-releases",
    url("http://dl.bintray.com/content/sbt/sbt-plugin-releases"))(
    Resolver.ivyStylePatterns)

addSbtPlugin("me.lessis" % "bintray-sbt" % "0.1.2")

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.4")

addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "0.1.13")

addSbtPlugin("org.scalastyle" % "scalastyle-sbt-plugin" % "0.8.0")

libraryDependencies += "org.scala-js" % "closure-compiler-java-6" % "v20160517"

libraryDependencies += "io.apigee" % "rhino" % "1.7R5pre4"

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
    root / "sbt-plugin/src/main/scala"
  )
}

// Add the ScalaJSEnvGenerator to the build (its in the build of the build)
sources in Compile +=
  baseDirectory.value / "project" / "ScalaJSEnvGenerator.scala"

sourceGenerators in Compile += Def.task {
  ScalaJSEnvGenerator.generateEnvHolder(
    baseDirectory.value.getParentFile / "tools",
    (sourceManaged in Compile).value)
}.taskValue

unmanagedResourceDirectories in Compile += {
  val root = baseDirectory.value.getParentFile
  root / "sbt-plugin/src/main/resources"
}
