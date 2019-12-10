package build

import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object BinaryIncompatibilities {
  val IR = Seq(
  )

  val Logging = Seq(
  )

  val Linker = Seq(
  )

  val LinkerInterface = Seq(
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.interface.unstable.ModuleInitializerImpl#VoidMainMethod.moduleClassName"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.linker.interface.unstable.ModuleInitializerImpl#MainMethodWithArgs.moduleClassName"),
  )

  val JSEnvs = Seq(
  )

  val JSEnvsTestKit = Seq(
  )

  val SbtPlugin = Seq(
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.sbtplugin.ScalaJSCrossVersion.binaryScalaJSVersion"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.sbtplugin.ScalaJSCrossVersion.currentBinaryVersion"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.sbtplugin.ScalaJSCrossVersion.scalaJSMapped"),
  )

  val TestCommon = Seq(
  )

  val TestAdapter = TestCommon ++ Seq(
  )

  val Library = Seq(
      ProblemFilters.exclude[MissingClassProblem](
          "scala.scalajs.js.JSArrayOps"),
      ProblemFilters.exclude[MissingClassProblem](
          "scala.scalajs.js.JSArrayOps$"),
      ProblemFilters.exclude[ReversedMissingMethodProblem](
          "scala.scalajs.runtime.LinkingInfo.fileLevelThis"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "scala.scalajs.runtime.LinkingInfo.globalThis"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "scala.scalajs.js.special.package.globalThis"),
  )

  val TestInterface = Seq(
  )
}
