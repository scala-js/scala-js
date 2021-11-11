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
      // private[adapter], not an issue
      ProblemFilters.exclude[IncompatibleMethTypeProblem](
          "org.scalajs.testing.adapter.JSEnvRPC.this"),
  )

  val Library = Seq(
  )

  val TestInterface = Seq(
  )
}
