val scala2Version = "2.12.20" // must remain pinned at 2.12.20 (last Scala version supported by Scala.js 1.18.2)
val mainBuildScala2Version = "2.12.21" // must evolve with the main build Scala version

ThisBuild / version := scalaJSVersion
ThisBuild / scalaVersion := scala2Version

ThisBuild / scalaJSLinkerConfig ~= { _.withCheckIR(true) }

val ScalaJSVersionBeforeTypedClosures = "1.18.2"

/** In libraryDependencies, replace the dependency whose `name` starts with
 *  `artifactNamePrefix` by the given `newModuleID`.
 */
def replaceDependency(artifactNamePrefix: String, newModuleID: ModuleID): Setting[_] = {
  libraryDependencies := {
    libraryDependencies.value.map { dep =>
      if (dep.name.startsWith(artifactNamePrefix))
        newModuleID
      else
        dep
    }
  }
}

def scalaJSCompilerPlugin(v: String): ModuleID =
  compilerPlugin("org.scala-js" % "scalajs-compiler" % v cross CrossVersion.full)

lazy val scala2OldCompilerOldLib = project.in(file("scala2-old-compiler-old-lib"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    replaceDependency("scalajs-compiler",
        scalaJSCompilerPlugin(ScalaJSVersionBeforeTypedClosures)),
    replaceDependency("scalajs-library",
        "org.scala-js" %% "scalajs-library" % ScalaJSVersionBeforeTypedClosures),
    replaceDependency("scalajs-scalalib",
        "org.scala-js" %% "scalajs-scalalib" % s"$scala2Version+$ScalaJSVersionBeforeTypedClosures"),
    scalaJSUseMainModuleInitializer := true
  )

lazy val scala2OldCompilerNewLib = project.in(file("scala2-old-compiler-new-lib"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    replaceDependency("scalajs-compiler",
        scalaJSCompilerPlugin(ScalaJSVersionBeforeTypedClosures)),
    replaceDependency("scalajs-scalalib",
        "org.scala-js" %% "scalajs-scalalib" % s"$mainBuildScala2Version+$scalaJSVersion"),
    scalaJSUseMainModuleInitializer := true
  )

lazy val scala3OldCompilerOldLib = project.in(file("scala3-old-compiler-old-lib"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    scalaVersion := "3.6.3",
    replaceDependency("scalajs-library",
        "org.scala-js" % "scalajs-library_2.13" % ScalaJSVersionBeforeTypedClosures),
    scalaJSUseMainModuleInitializer := true
  )

lazy val scala3OldCompilerNewLib = project.in(file("scala3-old-compiler-new-lib"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    scalaVersion := "3.6.3",
    scalaJSUseMainModuleInitializer := true
  )
