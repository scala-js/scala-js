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
  )

  val JSEnvs = Seq(
  )

  val JSEnvsTestKit = Seq(
  )

  val SbtPlugin = Seq(
      // Breaking (but still in RC).
      exclude[DirectMissingMethodProblem](
          "org.scalajs.sbtplugin.LinkerImpl.default")
  )

  val TestCommon = Seq(
  )

  val TestAdapter = TestCommon ++ Seq(
  )

  val Library = Seq(
      // Breaking (but still in RC).
      exclude[DirectMissingMethodProblem](
          "scala.scalajs.js.WrappedArray.array"),
      exclude[FinalClassProblem](
          "scala.scalajs.js.WrappedDictionary"),
      exclude[DirectMissingMethodProblem](
          "scala.scalajs.js.WrappedDictionary.dict"),
  )

  val TestInterface = Seq(
  )
}
