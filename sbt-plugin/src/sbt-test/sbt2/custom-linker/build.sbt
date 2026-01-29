/* This test reproduces one core requirement of scalajs-bundler, that of
 * getting the list of imported JS modules. To do that, it defines a custom
 * linker in a separate project, with a wrapper for the back-end. The wrapper
 * analyzes the LinkingUnit and stores the list of modules in a file. A
 * separate sbt task, scalaJSImportedModules, then loads this file to retrieve
 * the information.
 *
 * The custom linker is loaded as a custom `scalaJSLinkerImpl` and a custom
 * definition of `scalaJSLinker`.
 *
 * The `check` task makes sure that the retrieved information is correct.
 */

inThisBuild(Def.settings(
  version := scalaJSVersion,
  scalaVersion := "2.12.21",
))

lazy val check = taskKey[Unit]("")

lazy val customLinker = project.in(file("custom-linker"))
  .settings(
    name := "custom-linker-impl",
    scalaVersion := "2.13.17", // scalajs-linker is not published for Scala 3, use 2.13
    libraryDependencies += "org.scala-js" %% "scalajs-linker" % scalaJSVersion,
  )

name := "Scala.js sbt test"

Global / scalaJSLinkerImpl / fullClasspath :=
  (customLinker / Compile / fullClasspath).value

lazy val main = project
  .enablePlugins(CustomScalaJSLinkerPlugin)
  .settings(
    name := "custom-linker-main",
    scalaJSUseMainModuleInitializer := true,
  )

check := {
  val modules = (main / Compile / fastLinkJS / scalaJSImportedModules).value
  val expected = Set("foo.js", "bar.js")
  // test sizes as well to make sure that there are no duplicates in `modules`
  assert(modules.size == expected.size && modules.toSet == expected,
      s"Expected modules $expected but got $modules")
}
