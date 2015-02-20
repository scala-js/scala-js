import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object BinaryIncompatibilities {
  val Tools = Seq(
      // Breaking changes: LinkedClass is not a case class anymore
      ProblemFilters.exclude[MissingTypesProblem](
          "org.scalajs.core.tools.optimizer.LinkedClass$"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.core.tools.optimizer.LinkedClass.unapply"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.core.tools.optimizer.LinkedClass.apply"),
      ProblemFilters.exclude[MissingTypesProblem](
          "org.scalajs.core.tools.optimizer.LinkedClass"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.core.tools.optimizer.LinkedClass.productElement"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.core.tools.optimizer.LinkedClass.productArity"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.core.tools.optimizer.LinkedClass.canEqual"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.core.tools.optimizer.LinkedClass.productIterator"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.core.tools.optimizer.LinkedClass.productPrefix"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.core.tools.optimizer.LinkedClass.toString"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.core.tools.optimizer.LinkedClass.equals"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.core.tools.optimizer.LinkedClass.hashCode"),

      // Breaking changes: new field in LinkedClass
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.core.tools.optimizer.LinkedClass.copy"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem](
          "org.scalajs.core.tools.optimizer.LinkedClass.copy$default$18"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.core.tools.optimizer.LinkedClass.this"),

      /* In theory, this is a problem, but no one but us is going to
       * *implement* Analysis.ClassInfo.
       */
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.core.tools.optimizer.Analysis#ClassInfo.areInstanceTestsUsed"),

      // Private things, not an issue
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.core.tools.optimizer.Emitter#OneTimeCache.this"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.core.tools.optimizer.Emitter#DesugaredClassCache.this")
  )
}
