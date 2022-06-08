import org.scalajs.linker.interface.ModuleInitializer

val check = taskKey[Unit]("Run checks of this test")

version := scalaJSVersion
scalaVersion := "2.12.16"

enablePlugins(ScalaJSPlugin)

scalaJSUseMainModuleInitializer := true
scalaJSModuleInitializers +=
  ModuleInitializer.mainMethod("org.scalajs.sbtplugin.test", "foo")
scalaJSModuleInitializers in Test +=
  ModuleInitializer.mainMethod("org.scalajs.sbtplugin.test", "bar")

check := {
  // Compile should have main module init and Main.foo
  assert((scalaJSModuleInitializers in Compile).value.size == 2,
      "Bad number of scalaJSModuleInitializers in Compile")

  // Test should have test module init, Main.foo and Main.bar
  assert((scalaJSModuleInitializers in Test).value.size == 3,
      "Bad number of scalaJSModuleInitializers in Test")
}
