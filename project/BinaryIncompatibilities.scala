import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object BinaryIncompatibilities {
  val IR = Seq(
      // Breaking: ParamDef has an additinal `rest` parameter
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.core.ir.Trees#ParamDef.apply"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.core.ir.Trees#ParamDef.copy"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.core.ir.Trees#ParamDef.this")
  )

  val Tools = Seq(
      // Private, not an issue
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.core.tools.sem.Semantics.this"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.core.tools.optimizer.IRChecker#Env.withArgumentsVar")
  )

  val TestAdapter = Seq(
      // Private things, not an issue
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.testadapter.ScalaJSRunner.org$scalajs$testadapter$ScalaJSRunner$$awaitSlaveRunner"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.testadapter.ScalaJSRunner.org$scalajs$testadapter$ScalaJSRunner$$launchNewSlave"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.testadapter.ScalaJSRunner.org$scalajs$testadapter$ScalaJSRunner$$createSlaveRunner"),
      ProblemFilters.exclude[MissingMethodProblem](
          "org.scalajs.testadapter.ScalaJSRunner.org$scalajs$testadapter$ScalaJSRunner$$createSlave")
  )
}
