import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object BinaryIncompatibilities {
  val IR = Seq(
  )

  val Tools = Seq(
  )

  val JSEnvs = Seq(
  )

  val SbtPlugin = Seq(
      // private, not an issue
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.sbtplugin.ScalaJSPluginInternal.org$scalajs$sbtplugin$ScalaJSPluginInternal$$filterOutReflProxies")
  )

  val TestAdapter = Seq(
  )

  val CLI = Seq(
      // private, not an issue
      ProblemFilters.exclude[MissingTypesProblem](
          "org.scalajs.cli.Scalajsp$Options$"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.cli.Scalajsp#Options.this"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.cli.Scalajsp#Options.<init>$default$2"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.cli.Scalajsp#Options.<init>$default$3"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.cli.Scalajsp#Options.<init>$default$4"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.cli.Scalajsp#Options.apply"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.cli.Scalajsp#Options.apply$default$2"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.cli.Scalajsp#Options.apply$default$3"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.cli.Scalajsp#Options.apply$default$4"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.cli.Scalajsp#Options.copy"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.cli.Scalajsp#Options.copy$default$2"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.cli.Scalajsp#Options.copy$default$3"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.cli.Scalajsp#Options.copy$default$4"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.cli.Scalajsp#Options.showReflProxy")
  )

  val Library = Seq(
      // private[runtime], not an issue
      ProblemFilters.exclude[MissingMethodProblem](
          "scala.scalajs.runtime.RuntimeLong.TWO_PWR_16_DBL"),
      ProblemFilters.exclude[MissingMethodProblem](
          "scala.scalajs.runtime.RuntimeLong.TWO_PWR_22_DBL"),
      ProblemFilters.exclude[MissingMethodProblem](
          "scala.scalajs.runtime.RuntimeLong.TWO_PWR_31_DBL"),
      ProblemFilters.exclude[MissingMethodProblem](
          "scala.scalajs.runtime.RuntimeLong.TWO_PWR_32_DBL"),
      ProblemFilters.exclude[MissingMethodProblem](
          "scala.scalajs.runtime.RuntimeLong.TWO_PWR_44_DBL"),
      ProblemFilters.exclude[MissingMethodProblem](
          "scala.scalajs.runtime.RuntimeLong.TWO_PWR_63_DBL"),
      ProblemFilters.exclude[MissingMethodProblem](
          "scala.scalajs.runtime.RuntimeLong.TenPow9"),
      ProblemFilters.exclude[MissingMethodProblem](
          "scala.scalajs.runtime.RuntimeLong.SIGN_BIT"),
      ProblemFilters.exclude[MissingMethodProblem](
          "scala.scalajs.runtime.RuntimeLong.SIGN_BIT_VALUE"),
      ProblemFilters.exclude[MissingMethodProblem](
          "scala.scalajs.runtime.RuntimeLong.TWO_PWR_15_DBL"),

      // Synthetic methods inside RuntimeLong, not an issue
      ProblemFilters.exclude[MissingMethodProblem](
          "scala.scalajs.runtime.RuntimeLong.scala$scalajs$runtime$RuntimeLong$$isZero"),
      ProblemFilters.exclude[MissingMethodProblem](
          "scala.scalajs.runtime.RuntimeLong.scala$scalajs$runtime$RuntimeLong$/Mod"),
      ProblemFilters.exclude[MissingMethodProblem](
          "scala.scalajs.runtime.RuntimeLong.scala$scalajs$runtime$RuntimeLong$$isNegative"),
      ProblemFilters.exclude[MissingMethodProblem](
          "scala.scalajs.runtime.RuntimeLong.scala$scalajs$runtime$RuntimeLong$$setBit")
  )

  val TestInterface = Seq(
      // Private things, not an issue
      ProblemFilters.exclude[IncompatibleMethTypeProblem](
          "sbt.testing.Status.this"),

      // Inherited from parent class, not an issue
      ProblemFilters.exclude[MissingTypesProblem](
          "sbt.testing.Status"),
      ProblemFilters.exclude[MissingMethodProblem](
          "sbt.testing.Status.name"),
      ProblemFilters.exclude[MissingMethodProblem](
          "sbt.testing.Status.ordinal"),
      ProblemFilters.exclude[MissingMethodProblem](
          "sbt.testing.Status.toString")
    )
}
