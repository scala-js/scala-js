resolvers += Resolver.url(
    "bintray-sbt-plugin-releases",
    url("http://dl.bintray.com/content/sbt/sbt-plugin-releases"))(
    Resolver.ivyStylePatterns)

addSbtPlugin("me.lessis" % "bintray-sbt" % "0.1.1")

libraryDependencies += "com.google.javascript" % "closure-compiler" % "v20130603"

libraryDependencies += "org.mozilla" % "rhino" % "1.7R4"

unmanagedSourceDirectories in Compile <+= baseDirectory {
  base => base.getParentFile / "sbt-plugin" / "src" / "main" / "scala"
}
