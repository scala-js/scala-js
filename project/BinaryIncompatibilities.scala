package build

import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object BinaryIncompatibilities {
  val IR = Seq(
    // private, not an issue
    ProblemFilters.exclude[MissingClassProblem]("org.scalajs.ir.Serializers$Deserializer$BodyHack5Transformer$"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.ir.Serializers#Hacks.use*"),
  )

  val Linker = Seq(
  )

  val LinkerInterface = Seq(
  )

  val SbtPlugin = Seq(
  )

  val TestAdapter = Seq(
  )

  val Library = Seq(
  )

  val TestInterface = Seq(
  )

  val JUnitRuntime = Seq(
  )
}
