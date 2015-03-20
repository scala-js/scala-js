import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object BinaryIncompatibilities {
  val Tools = Seq(
      // Private, not an issue
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.core.tools.sem.Semantics.this")
  )
}
