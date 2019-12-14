package build

import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object BinaryIncompatibilities {
  val IR = Seq(
  )

  val Logging = Seq(
  )

  val Linker = Seq(
    // private[emitter], not an issue.
    exclude[DirectMissingMethodProblem](
        "org.scalajs.linker.backend.emitter.ClassEmitter.genModuleAccessor"),
    exclude[DirectMissingMethodProblem](
        "org.scalajs.linker.backend.emitter.CoreJSLib.build"),

    // private, not an issue.
    exclude[DirectMissingMethodProblem](
        "org.scalajs.linker.backend.emitter.CoreJSLib#CoreJSLibBuilder.this"),
    exclude[DirectMissingMethodProblem](
        "org.scalajs.linker.backend.emitter.Emitter#DesugaredClassCache.moduleAccessor"),
    exclude[DirectMissingMethodProblem](
        "org.scalajs.linker.backend.emitter.Emitter#State.coreJSLib"),
  )

  val LinkerInterface = Seq(
  )

  val JSEnvs = Seq(
  )

  val JSEnvsTestKit = Seq(
  )

  val SbtPlugin = Seq(
  )

  val TestCommon = Seq(
  )

  val TestAdapter = TestCommon ++ Seq(
  )

  val Library = Seq(
  )

  val TestInterface = Seq(
  )
}
