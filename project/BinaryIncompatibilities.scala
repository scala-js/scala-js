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

      // Breaking: PropertyDef has new field `static`
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.core.ir.Trees#PropertyDef.this"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.core.ir.Trees#PropertyDef.apply"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.core.ir.Trees#PropertyDef.copy"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.core.ir.Trees#PropertyDef.copy$default$1"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.core.ir.Trees#PropertyDef.copy$default$2"),

      // Breaking: TopLevelExportDef has been renamed to TopLevelMethodExportDef
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.core.ir.Trees$TopLevelExportDef"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.core.ir.Trees$TopLevelExportDef$"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.core.ir.Tags.TagTopLevelExportDef"),

      // Breaking: PropertyName.{name -> encodedName}
      ProblemFilters.exclude[ReversedMissingMethodProblem](
          "org.scalajs.core.ir.Trees#PropertyName.encodedName"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.core.ir.Trees#StringLiteral.name"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.core.ir.Trees#PropertyName.name"),

      // private, not an issue
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.core.ir.Infos#MethodInfo.this"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.core.ir.Infos#GenInfoTraverser.generateClassExportsInfo")
  )

  val Tools = Seq(
      // Breaking. Remove PropertyName.name
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.core.tools.javascript.Trees#PropertyName.name"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.core.tools.javascript.Trees#StringLiteral.name"),

      // private, not an issue
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.core.tools.sem.Semantics.this"),

      // private, not an issue
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.core.tools.linker.checker.IRChecker#CheckedField.this"),

      // private, not an issue
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.core.tools.linker.analyzer.Analyzer.org$scalajs$core$tools$linker$analyzer$Analyzer$$createMissingMethodInfo$default$2"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
          "org.scalajs.core.tools.linker.analyzer.Analyzer.org$scalajs$core$tools$linker$analyzer$Analyzer$$createMissingMethodInfo$default$3"),

      // private[closure], not an issue
      ProblemFilters.exclude[IncompatibleMethTypeProblem](
          "org.scalajs.core.tools.linker.backend.closure.ClosureAstTransformer.transformString"),

      // private[emitter], not an issue
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.core.tools.linker.backend.emitter.ScalaJSClassEmitter$"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.core.tools.linker.backend.emitter.ScalaJSClassEmitter"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.core.tools.linker.backend.emitter.JSDesugaring"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.core.tools.linker.backend.emitter.JSDesugaring$DesugarException"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.core.tools.linker.backend.emitter.JSDesugaring$MyTreeOps"),

      // private, not an issue
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.core.tools.linker.backend.emitter.JSDesugaring$"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.core.tools.linker.backend.emitter.JSDesugaring$JSDesugar"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.core.tools.linker.backend.emitter.JSDesugaring$JSDesugar$RecordFieldVarRef$"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.core.tools.linker.backend.emitter.JSDesugaring$JSDesugar$RecordVarRef$"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.core.tools.linker.backend.emitter.JSDesugaring$JSDesugar$RecordAwareEnv"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.core.tools.linker.backend.emitter.JSDesugaring$Lhs"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.core.tools.linker.backend.emitter.JSDesugaring$Lhs$"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.core.tools.linker.backend.emitter.JSDesugaring$Lhs$Assign"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.core.tools.linker.backend.emitter.JSDesugaring$Lhs$Assign$"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.core.tools.linker.backend.emitter.JSDesugaring$Lhs$Return"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.core.tools.linker.backend.emitter.JSDesugaring$Lhs$Return$"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.core.tools.linker.backend.emitter.JSDesugaring$Lhs$Discard$"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.core.tools.linker.backend.emitter.JSDesugaring$Lhs$VarDef$"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.core.tools.linker.backend.emitter.JSDesugaring$Lhs$VarDef"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.core.tools.linker.backend.emitter.JSDesugaring$Env"),
      ProblemFilters.exclude[MissingClassProblem](
          "org.scalajs.core.tools.linker.backend.emitter.JSDesugaring$Env$")
  )

  val JSEnvs = Seq(
  )

  val JSEnvsTestKit = Seq(
  )

  val SbtPlugin = Seq(
    // private[scalajs], not an issue.
    ProblemFilters.exclude[DirectMissingMethodProblem](
        "org.scalajs.sbtplugin.HTMLRunnerTemplate.render")
  )

  val TestAdapter = Seq(
  )

  val CLI = Seq(
  )

  val Library = Seq(
      // Relaxed typing (js.Iterable instead of js.Array) in js.Promise.
      // Not a compatibility issue (due to JS land).
      ProblemFilters.exclude[IncompatibleMethTypeProblem](
          "scala.scalajs.js.Promise.race"),
      ProblemFilters.exclude[IncompatibleMethTypeProblem](
          "scala.scalajs.js.Promise.all"),

      // New member in non-sealed trait (for low prio implicits).
      // Theoretically breaking.
      ProblemFilters.exclude[InheritedNewAbstractMethodProblem](
          "scala.scalajs.js.LowestPrioAnyImplicits.iterableOps"),

      // New members of an @js.native trait in `runtime`, not an issue
      ProblemFilters.exclude[ReversedMissingMethodProblem](
          "scala.scalajs.runtime.LinkingInfo#Semantics.arrayIndexOutOfBounds"),
      ProblemFilters.exclude[ReversedMissingMethodProblem](
          "scala.scalajs.runtime.LinkingInfo#Semantics.scala$scalajs$runtime$LinkingInfo$Semantics$_setter_$arrayIndexOutOfBounds_=")
  )

  val TestInterface = Seq(
  )
}
