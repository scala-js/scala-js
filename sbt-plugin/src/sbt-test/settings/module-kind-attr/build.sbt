import org.scalajs.linker.interface.ModuleKind

val check = taskKey[Unit]("Run checks of this test")

inThisBuild(Def.settings(
  version := scalaJSVersion,
  scalaVersion := "2.12.21",
))

def assertModuleKind(output: Attributed[File], expected: ModuleKind): Unit = {
  val decoded = readScalaJSModuleKind(output)
  assert(decoded.contains(expected), s"Expected '$expected', got '$decoded'")
}

lazy val noModule = project.in(file("no-module"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.NoModule)),
    scalaJSUseMainModuleInitializer := true,
  )

lazy val esModule = project.in(file("es-module"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.ESModule)),
    scalaJSUseMainModuleInitializer := true,
  )

lazy val commonJsModule = project.in(file("commonjs-module"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule)),
    scalaJSUseMainModuleInitializer := true,
  )

check := {
  assertModuleKind((noModule / Compile / fastOptJS).value, ModuleKind.NoModule)
  assertModuleKind((noModule / Compile / fullOptJS).value, ModuleKind.NoModule)

  assertModuleKind((esModule / Compile / fastOptJS).value, ModuleKind.ESModule)
  assertModuleKind((esModule / Compile / fullOptJS).value, ModuleKind.ESModule)

  assertModuleKind((commonJsModule / Compile / fastOptJS).value,
      ModuleKind.CommonJSModule)
  assertModuleKind((commonJsModule / Compile / fullOptJS).value,
      ModuleKind.CommonJSModule)
}
