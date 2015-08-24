import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object BinaryIncompatibilities {
  val IR = Seq(
      // Breaking: Trees.FieldDef has a PropertyName instead of an Ident
      ProblemFilters.exclude[IncompatibleMethTypeProblem](
          "org.scalajs.core.ir.Trees#FieldDef.this"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.core.ir.Trees#FieldDef.name"),
      ProblemFilters.exclude[IncompatibleMethTypeProblem](
          "org.scalajs.core.ir.Trees#FieldDef.copy"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.core.ir.Trees#FieldDef.copy$default$1"),
      ProblemFilters.exclude[IncompatibleMethTypeProblem](
          "org.scalajs.core.ir.Trees#FieldDef.apply")
  )

  val Tools = Seq(
      // Breaking: JSDesugar.desugarJavaScript has been brutally removed
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.core.tools.javascript.JSDesugaring.desugarJavaScript"),

      // Private things, not an issue
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.core.tools.optimizer.Analyzer#ClassInfo.isRawJSType"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.core.tools.optimizer.Analyzer#ClassInfo.isClass"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.core.tools.optimizer.Analyzer#ClassInfo.isHijackedClass"),

      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.core.tools.javascript.JSDesugaring#JSDesugar.org$scalajs$core$tools$javascript$JSDesugaring$JSDesugar$$implicitOutputMode"),
      ProblemFilters.exclude[IncompatibleMethTypeProblem](
          "org.scalajs.core.tools.javascript.JSDesugaring#JSDesugar.this"),
      ProblemFilters.exclude[IncompatibleMethTypeProblem](
          "org.scalajs.core.tools.javascript.JSDesugaring.desugarToFunction")
  )

  val JSEnvs = Seq(
  )

  val SbtPlugin = Seq(
  )

  val TestAdapter = Seq(
  )

  val CLI = Seq(
      // Private things, not an issue
      ProblemFilters.exclude[MissingTypesProblem](
          "org.scalajs.cli.Scalajsp$Options$"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.cli.Scalajsp#Options.apply$default$5"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.cli.Scalajsp#Options.apply$default$4"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.cli.Scalajsp#Options.<init>$default$5"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.cli.Scalajsp#Options.apply$default$3"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.cli.Scalajsp#Options.apply"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.cli.Scalajsp#Options.apply"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.cli.Scalajsp#Options.<init>$default$4"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.cli.Scalajsp#Options.<init>$default$3"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.cli.Scalajsp#Options.desugar"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.cli.Scalajsp#Options.copy$default$5"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.cli.Scalajsp#Options.copy"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.cli.Scalajsp#Options.copy$default$4"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.cli.Scalajsp#Options.copy$default$3"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.cli.Scalajsp#Options.this")
  )
}
