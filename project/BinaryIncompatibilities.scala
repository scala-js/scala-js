import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object BinaryIncompatibilities {
  val IR = Seq(
      // Breaking: FieldDef has new field `static`
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.core.ir.Trees#FieldDef.this"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.core.ir.Trees#FieldDef.apply"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.core.ir.Trees#FieldDef.copy"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.core.ir.Trees#FieldDef.copy$default$1"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.core.ir.Trees#FieldDef.copy$default$2"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.core.ir.Trees#FieldDef.copy$default$3"),

      // private, not an issue
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.core.ir.Infos#MethodInfo.this"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.core.ir.Infos#GenInfoTraverser.generateClassExportsInfo")
  )

  val Tools = Seq(
      // private, not an issue
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.core.tools.linker.checker.IRChecker#CheckedField.this"),

      // private, not an issue
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.core.tools.linker.analyzer.Analyzer.org$scalajs$core$tools$linker$analyzer$Analyzer$$createMissingMethodInfo$default$2"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.core.tools.linker.analyzer.Analyzer.org$scalajs$core$tools$linker$analyzer$Analyzer$$createMissingMethodInfo$default$3"),

      // private[emitter], not an issue
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.core.tools.linker.backend.emitter.ScalaJSClassEmitter.genClass"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.core.tools.linker.backend.emitter.ScalaJSClassEmitter.genClassDef")
  )

  val JSEnvs = Seq(
  )

  val JSEnvsTestKit = Seq(
  )

  val SbtPlugin = Seq(
  )

  val TestAdapter = Seq(
  )

  val CLI = Seq(
  )

  val Library = Seq(
  )

  val TestInterface = Seq(
  )
}
