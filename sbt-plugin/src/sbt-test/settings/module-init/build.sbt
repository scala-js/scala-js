import org.scalajs.linker.interface.ModuleInitializer

val check = taskKey[Unit]("Run checks of this test")

version := scalaJSVersion
scalaVersion := "2.12.20"

enablePlugins(ScalaJSPlugin)

scalaJSUseMainModuleInitializer := true
scalaJSModuleInitializers +=
  ModuleInitializer.mainMethod("org.scalajs.sbtplugin.test", "foo")
Test / scalaJSModuleInitializers +=
  ModuleInitializer.mainMethod("org.scalajs.sbtplugin.test", "bar")

check := {
  // Compile should have main module init and Main.foo
  assert((Compile / scalaJSModuleInitializers).value.size == 2,
      "Bad number of Compile / scalaJSModuleInitializers")

  // Test should have test module init, Main.foo and Main.bar
  assert((Test / scalaJSModuleInitializers).value.size == 3,
      "Bad number of Test / scalaJSModuleInitializers")
}
