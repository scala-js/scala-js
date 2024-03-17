import scala.concurrent.Await
import scala.concurrent.duration.Duration

import org.scalajs.linker.MemOutputFile
import org.scalajs.linker.interface._
import org.scalajs.sbtplugin.Loggers.sbtLogger2ToolsLogger

lazy val concurrentFakeFullOptJS = taskKey[Any]("")
lazy val concurrentUseOfLinkerTest = taskKey[Any]("")

name := "Scala.js sbt test"

version := scalaJSVersion
scalaVersion := "2.12.19"

enablePlugins(ScalaJSPlugin)

scalaJSUseMainModuleInitializer := true

/* This hopefully exposes concurrent uses of the linker. If it fails/gets
 * flaky, there is a bug somewhere - #2202
 */

// A fake fullOptJS that we will run concurrently with the true fullOptJS
concurrentFakeFullOptJS := Def.taskDyn {
  val log = streams.value.log
  val ir = (scalaJSIR in Compile).value.data
  val moduleInitializers = (scalaJSModuleInitializers in Compile).value

  Def.task {
    import scala.concurrent.ExecutionContext.Implicits.global

    log.info("Fake full optimizing")
    val linker = (scalaJSLinker in Compile in fullOptJS).value
    val output = LinkerOutput(MemOutputFile())
    Await.result(
      linker.link(ir, moduleInitializers, output, sbtLogger2ToolsLogger(log)),
      Duration.Inf)
  }.tag((usesScalaJSLinkerTag in Compile in fullOptJS).value)
}.value

/* Depend on both fullOptJS and concurrentFakeFullOptJS, so that they
 * are hopefully executed in parallel (potentially, but they should be
 * blocked from actually doing so by the concurrent restrictions on
 * usesScalaJSLinkerTag).
 */
concurrentUseOfLinkerTest := {
  (fullOptJS in Compile).value
  concurrentFakeFullOptJS.value
}
