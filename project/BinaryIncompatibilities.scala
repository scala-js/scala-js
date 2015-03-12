import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object BinaryIncompatibilities {
  val Tools = Seq(
      // Private things, not an issue
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.core.tools.javascript.JSDesugaring.genCallHelper"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.core.tools.javascript.JSDesugaring.encodeClassField"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.core.tools.javascript.JSDesugaring.org$scalajs$core$tools$javascript$JSDesugaring$$genAsInstanceOf"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.core.tools.javascript.JSDesugaring.encodeClassVar"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.core.tools.javascript.JSDesugaring.envField"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.core.tools.javascript.JSDesugaring.genIsInstanceOf"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.core.tools.javascript.JSDesugaring#JSDesugar.this"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.core.tools.corelib.CoreJSLibs.org$scalajs$core$tools$corelib$CoreJSLibs$$getOption$1"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.core.tools.corelib.CoreJSLibs.org$scalajs$core$tools$corelib$CoreJSLibs$$makeLib")
  )
}
