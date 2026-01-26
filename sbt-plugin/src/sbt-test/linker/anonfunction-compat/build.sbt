val scala2Version = "2.12.20" // must remain pinned at 2.12.20 (last Scala version supported by Scala.js 1.18.2)
val scala2BinaryVersion = "2.12"
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

/** We use explicit artifact names with `%` instead of `%%` or `cross CrossVersion.full`
 *  because in sbt 2.x with `platform` set, those would add the `_sjs1`
 *  to core Scala.js libraries that are published without the suffix.
 */
def scalaJSCompilerPlugin(v: String, scalaFullVersion: String): ModuleID =
  compilerPlugin("org.scala-js" % s"scalajs-compiler_$scalaFullVersion" % v)

def scalaJSCoreLib(name: String, v: String, scalaBinVersion: String): ModuleID =
  "org.scala-js" % s"${name}_$scalaBinVersion" % v

lazy val scala2OldCompilerOldLib = project.in(file("scala2-old-compiler-old-lib"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    replaceDependency("scalajs-compiler",
        scalaJSCompilerPlugin(ScalaJSVersionBeforeTypedClosures, scala2Version)),
    replaceDependency("scalajs-library",
        scalaJSCoreLib("scalajs-library", ScalaJSVersionBeforeTypedClosures, scala2BinaryVersion)),
    replaceDependency("scalajs-scalalib",
        scalaJSCoreLib("scalajs-scalalib", s"$scala2Version+$ScalaJSVersionBeforeTypedClosures", scala2BinaryVersion)),
    scalaJSUseMainModuleInitializer := true
  )

lazy val scala2OldCompilerNewLib = project.in(file("scala2-old-compiler-new-lib"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    replaceDependency("scalajs-compiler",
        scalaJSCompilerPlugin(ScalaJSVersionBeforeTypedClosures, scala2Version)),
    replaceDependency("scalajs-scalalib",
        scalaJSCoreLib("scalajs-scalalib", s"$mainBuildScala2Version+$scalaJSVersion", scala2BinaryVersion)),
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
