import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object BinaryIncompatibilities {
  val IR = Seq(
  )

  val Tools = Seq(
    // Breaking changes (rename Linker to BaseLinker)
    ProblemFilters.exclude[MissingClassProblem](
        "org.scalajs.core.tools.optimizer.Linker"),

    // Breaking changes (remove noWarnMissing)
    ProblemFilters.exclude[MissingClassProblem](
        "org.scalajs.core.tools.optimizer.ScalaJSOptimizer$NoWarnMissing"),
    ProblemFilters.exclude[MissingClassProblem](
        "org.scalajs.core.tools.optimizer.ScalaJSOptimizer$NoWarnMissing$"),
    ProblemFilters.exclude[MissingClassProblem](
        "org.scalajs.core.tools.optimizer.ScalaJSOptimizer$NoWarnMissing$Method"),
    ProblemFilters.exclude[MissingClassProblem](
        "org.scalajs.core.tools.optimizer.ScalaJSOptimizer$NoWarnMissing$Method$"),
    ProblemFilters.exclude[MissingClassProblem](
        "org.scalajs.core.tools.optimizer.ScalaJSOptimizer$NoWarnMissing$Class"),
    ProblemFilters.exclude[MissingClassProblem](
        "org.scalajs.core.tools.optimizer.ScalaJSOptimizer$NoWarnMissing$Class$"),

    ProblemFilters.exclude[MissingMethodProblem](
        "org.scalajs.core.tools.optimizer.ScalaJSOptimizer#OptimizerConfig.noWarnMissing")
  ) ++ Seq("ScalaJSOptimizer", "ScalaJSClosureOptimizer").flatMap(cls => Seq(
    ProblemFilters.exclude[MissingMethodProblem](
        "org.scalajs.core.tools.optimizer."+cls+"#Config.noWarnMissing"),
    ProblemFilters.exclude[MissingMethodProblem](
        "org.scalajs.core.tools.optimizer."+cls+"#Config.withNoWarnMissing")

    // Breaking changes (no binary compat for ScalaJS[Closure]Optimizer.Conifg)
  )) ++ Seq(("ScalaJSOptimizer", 10), ("ScalaJSClosureOptimizer", 11)).flatMap { case (cls, defs) => Seq(
    ProblemFilters.exclude[MissingTypesProblem](
        "org.scalajs.core.tools.optimizer."+cls+"$Config"),
    ProblemFilters.exclude[MissingTypesProblem](
        "org.scalajs.core.tools.optimizer."+cls+"$Config$"),
    ProblemFilters.exclude[MissingMethodProblem](
        "org.scalajs.core.tools.optimizer."+cls+"#Config.apply"),
    ProblemFilters.exclude[MissingMethodProblem](
        "org.scalajs.core.tools.optimizer."+cls+"#Config.unapply"),
    ProblemFilters.exclude[MissingMethodProblem](
        "org.scalajs.core.tools.optimizer."+cls+"#Config.productElement"),
    ProblemFilters.exclude[MissingMethodProblem](
        "org.scalajs.core.tools.optimizer."+cls+"#Config.productArity"),
    ProblemFilters.exclude[MissingMethodProblem](
        "org.scalajs.core.tools.optimizer."+cls+"#Config.canEqual"),
    ProblemFilters.exclude[MissingMethodProblem](
        "org.scalajs.core.tools.optimizer."+cls+"#Config.copy"),
    ProblemFilters.exclude[MissingMethodProblem](
        "org.scalajs.core.tools.optimizer."+cls+"#Config.productIterator"),
    ProblemFilters.exclude[MissingMethodProblem](
        "org.scalajs.core.tools.optimizer."+cls+"#Config.productPrefix"),
    ProblemFilters.exclude[MissingMethodProblem](
        "org.scalajs.core.tools.optimizer."+cls+"#Config.toString"),
    ProblemFilters.exclude[IncompatibleMethTypeProblem](
        "org.scalajs.core.tools.optimizer."+cls+"#Config.this"),
    ProblemFilters.exclude[MissingMethodProblem](
        "org.scalajs.core.tools.optimizer."+cls+"#Config.this"),
    ProblemFilters.exclude[MissingMethodProblem](
        "org.scalajs.core.tools.optimizer."+cls+"#Config.equals"),
    ProblemFilters.exclude[MissingMethodProblem](
        "org.scalajs.core.tools.optimizer."+cls+"#Config.hashCode")
  ) ++ (1 to defs).flatMap(i => Seq(
    ProblemFilters.exclude[MissingMethodProblem](
        "org.scalajs.core.tools.optimizer."+cls+"#Config.apply$default$"+i),
    ProblemFilters.exclude[MissingMethodProblem](
        "org.scalajs.core.tools.optimizer."+cls+"#Config.<init>$default$"+i),
    ProblemFilters.exclude[MissingMethodProblem](
        "org.scalajs.core.tools.optimizer."+cls+"#Config.copy$default$"+i)
  ))} ++ Seq(

    // Private things, not an issue
    ProblemFilters.exclude[MissingClassProblem](
        "org.scalajs.core.tools.optimizer.Linker$PersistentIRFile")
  )

  val JSEnvs = Seq(
  )

  val SbtPlugin = Seq(
  )

  val TestAdapter = Seq(
  )

  val CLI = Seq(
  )
}
