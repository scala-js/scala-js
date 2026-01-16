/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.partest.scalajs

class ScalaJSPartestOptions private (
    val testFilter: ScalaJSPartestOptions.TestFilter,
    val useWasm: Boolean,
    val optMode: ScalaJSPartestOptions.OptMode,
    val showDiff: Boolean
) {
  def banner: String = {
    import org.scalajs.ir.ScalaJSVersions.{current => currentVersion}

    s"""
    |Scala.js version is: $currentVersion
    |Scala.js options are:
    |Wasm:                ${useWasm}
    |optimizer:           ${optMode.shortStr}
    |testFilter:          ${testFilter.descr}
    """.stripMargin
  }

  val targetSpecificCheckFileSuffix: String =
    if (useWasm) "-wasm"
    else "-js"
}

object ScalaJSPartestOptions {

  sealed abstract class TestFilter {
    def descr: String
  }
  case object BlacklistedTests extends TestFilter {
    override def descr: String = "Blacklisted"
  }
  case object WhitelistedTests extends TestFilter {
    override def descr: String = "Whitelisted"
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
      case "none" => NoOpt
      case "fast" => FastOpt
      case "full" => FullOpt
      case _      => throw new IllegalArgumentException(s"Unknown optimization mode: $id")
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
    var useWasm: Boolean = false
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
        error(
            s"You cannot specify twice what tests to use (already specified: $fil, new: $newFilter)")
      case (None, newFilter) =>
        filter = Some(newFilter)
    }

    for (arg <- args) arg match {
      case "--wasm" =>
        useWasm = true
      case "--fastOpt" =>
        optMode = FastOpt
      case "--noOpt" =>
        optMode = NoOpt
      case "--fullOpt" =>
        optMode = FullOpt
      case "--blacklisted" =>
        setFilter(BlacklistedTests)
      case "--whitelisted" =>
        setFilter(WhitelistedTests)
      case "--showDiff" =>
        showDiff = true
      case _ =>
        setFilter(SomeTests(arg :: Nil))
    }

    if (failed) None
    else Some {
      new ScalaJSPartestOptions(
          filter.getOrElse(WhitelistedTests), useWasm, optMode, showDiff)
    }
  }

}
