package scala.tools.partest.scalajs

class ScalaJSPartestOptions private (
  val testFilter: ScalaJSPartestOptions.TestFilter,
  val optMode: ScalaJSPartestOptions.OptMode,
  val showDiff: Boolean
)

object ScalaJSPartestOptions {

  sealed abstract class TestFilter {
    def descr: String
  }
  case object UnknownTests extends TestFilter {
    override def descr: String = "Unknown"
  }
  case object BlacklistedTests extends TestFilter {
    override def descr: String = "Blacklisted"
  }
  case object WhitelistedTests extends TestFilter {
    override def descr: String = "Whitelisted"
  }
  case object BuglistedTests extends TestFilter {
    override def descr: String = "Buglisted"
  }
  case class SomeTests(names: List[String]) extends TestFilter {
    override def descr: String = "Custom " + this.toString
    override def toString() =
      names.map(x => s""""$x"""").mkString("[", ", ", "]")
  }

  sealed abstract class OptMode {
    def shortStr: String
    def id: String
  }
  object OptMode {
    def fromId(id: String): OptMode = id match {
      case "none"  => NoOpt
      case "fast"  => FastOpt
      case "full"  => FullOpt
      case _       => sys.error(s"Unknown optimization mode: $id")
    }
  }
  case object NoOpt extends OptMode {
    def shortStr: String = "None"
    def id: String = "none"
  }
  case object FastOpt extends OptMode {
    def shortStr: String = "Fast"
    def id: String = "fast"
  }
  case object FullOpt extends OptMode {
    def shortStr: String = "Full"
    def id: String = "full"
  }

  def apply(args: Array[String],
      errorReporter: String => Unit): Option[ScalaJSPartestOptions] = {

    var failed = false

    var filter: Option[TestFilter] = None
    var optMode: OptMode = NoOpt
    var showDiff: Boolean = false

    def error(msg: String) = {
      failed = true
      errorReporter(msg)
    }

    def setFilter(newFilter: TestFilter) = (filter, newFilter) match {
      case (Some(SomeTests(oldNames)), SomeTests(newNames)) =>
        // Merge test names
        filter = Some(SomeTests(oldNames ++ newNames))
      case (Some(fil), newFilter) =>
        error(s"You cannot specify twice what tests to use (already specified: $fil, new: $newFilter)")
      case (None, newFilter) =>
        filter = Some(newFilter)
    }

    for (arg <- args) arg match {
      case "--fastOpt" =>
        optMode = FastOpt
      case "--noOpt" =>
        optMode = NoOpt
      case "--fullOpt" =>
        optMode = FullOpt
      case "--blacklisted" =>
        setFilter(BlacklistedTests)
      case "--buglisted" =>
        setFilter(BuglistedTests)
      case "--whitelisted" =>
        setFilter(WhitelistedTests)
      case "--unknown" =>
        setFilter(UnknownTests)
      case "--showDiff" =>
        showDiff = true
      case _ =>
        setFilter(SomeTests(arg :: Nil))
    }

    if (failed) None
    else Some {
      new ScalaJSPartestOptions(
        filter.getOrElse(WhitelistedTests), optMode, showDiff)
    }
  }

}
