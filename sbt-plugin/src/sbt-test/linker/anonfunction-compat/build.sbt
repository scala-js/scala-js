ThisBuild / version := scalaJSVersion
ThisBuild / scalaVersion := "2.12.20"

ThisBuild / scalaJSLinkerConfig ~= { _.withCheckIR(true) }

val ScalaJSVersionBeforeTypedClosures = "1.18.2"

def rewriteDependencyVersion(artifactNamePrefix: String, v: String): Setting[_] = {
  libraryDependencies := {
    libraryDependencies.value.map { dep =>
      if (dep.name.startsWith(artifactNamePrefix))
        dep.withRevision(dep.revision.substring(0, dep.revision.indexOf('+') + 1) + v)
      else
        dep
    }
  }
}

def forceCompilerPluginVersion(v: String): Seq[Setting[_]] =
  Seq(rewriteDependencyVersion("scalajs-compiler", v))

def forceLibraryVersion(v: String): Seq[Setting[_]] = {
  Seq(
    rewriteDependencyVersion("scalajs-library", v),
    rewriteDependencyVersion("scalajs-scalalib", v)
  )
}

lazy val scala2OldCompilerOldLib = project.in(file("scala2-old-compiler-old-lib"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    forceCompilerPluginVersion(ScalaJSVersionBeforeTypedClosures),
    forceLibraryVersion(ScalaJSVersionBeforeTypedClosures),
    scalaJSUseMainModuleInitializer := true
  )

lazy val scala2OldCompilerNewLib = project.in(file("scala2-old-compiler-new-lib"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    forceCompilerPluginVersion(ScalaJSVersionBeforeTypedClosures),
    scalaJSUseMainModuleInitializer := true
  )

lazy val scala3OldCompilerOldLib = project.in(file("scala3-old-compiler-old-lib"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    scalaVersion := "3.6.3",
    forceLibraryVersion(ScalaJSVersionBeforeTypedClosures),
    scalaJSUseMainModuleInitializer := true
  )

lazy val scala3OldCompilerNewLib = project.in(file("scala3-old-compiler-new-lib"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    scalaVersion := "3.6.3",
    scalaJSUseMainModuleInitializer := true
  )
