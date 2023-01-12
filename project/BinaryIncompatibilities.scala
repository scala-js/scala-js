package build

import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object BinaryIncompatibilities {
  val IR = Seq(
    // Breaking, but in minor verison, so OK.
    exclude[Problem]("org.scalajs.ir.*"),
  )

  val Linker = Seq(
    // Breaking, but in minor version, so OK.
    exclude[Problem]("org.scalajs.linker.standard.*"),
  )

  val LinkerInterface = Seq(
    // Breaking, but in minor version, so OK.
    exclude[Problem]("org.scalajs.linker.interface.unstable.*"),

    // private, not an issue
    ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalajs.linker.interface.Semantics.this"),
  )

  val SbtPlugin = Seq(
  )

  val TestAdapter = Seq(
  )

  private val JSTupleUnapplyExclusion: ProblemFilter = {
    /* !!! Very delicate
     *
     * We changed the result type of `js.TupleN.unapply` from `Option` to
     * `Some`, to make them irrefutable from Scala 3's point of view. This
     * breaks binary compat, so we added a `protected` overload with the old
     * binary signature.
     *
     * Unfortunately, those do not get a *static forwarder* in the class file,
     * and hence MiMa still complains about them. Although the error message is
     * clearly about "static method"s, the *filter* to apply is
     * indistinguishable between the instance and static methods!
     *
     * Therefore, we implement here our own filter that only matches the
     * *static* `unapply` method.
     *
     * Note that even though MiMa reports potential issues with static methods,
     * these are ghost proplems. They do not exist in the .sjsir files to begin
     * with, because the companion trait is a JS trait. We only generate static
     * forwarders in Scala classes and traits. So filtering out the static
     * method incompatibilities is legit.
     */

    val JSTupleUnapplyFullNameRegex = raw"""scala\.scalajs\.js\.Tuple\d+\.unapply""".r

    { (problem: Problem) =>
      val isStaticJSTupleUnapply = problem match {
        case problem: IncompatibleResultTypeProblem =>
          problem.ref.isStatic && (problem.ref.fullName match {
            case JSTupleUnapplyFullNameRegex() => true
            case _                             => false
          })
        case _ =>
          false
      }
      !isStaticJSTupleUnapply // true to keep; false to filter out the problem
    }
  }

  val Library = Seq(
    JSTupleUnapplyExclusion,
  )

  val TestInterface = Seq(
  )

  val JUnitRuntime = Seq(
  )
}
