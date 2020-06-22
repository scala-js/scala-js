package build

import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object BinaryIncompatibilities {
  val IR = Seq(
  )

  val Linker = Seq(
  )

  val LinkerInterface = Seq(
  )

  val SbtPlugin = Seq(
  )

  val TestCommon = Seq(
  )

  val TestAdapter = TestCommon ++ Seq(
  )

  val Library = Seq(
      // Native types, not an issue.
      exclude[IncompatibleMethTypeProblem](
          "scala.scalajs.js.JSON.stringify"),
      exclude[IncompatibleResultTypeProblem](
          "scala.scalajs.js.JSON.stringify$default$3"),
  )

  val TestInterface = Seq(
  )
}
