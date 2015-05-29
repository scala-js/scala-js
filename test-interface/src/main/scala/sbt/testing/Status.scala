package sbt.testing

/** Represents the status of running a test.
 *
 *  Test frameworks can decided which of these to use and what they mean, but
 *  in general, the intended meanings are:
 *
 *  - Success - a test succeeded
 *  - Error - an "error" occurred during a test
 *  - Failure - an "failure" during a test
 *  - Skipped - a test was skipped for any reason
 *  - Ignored - a test was ignored, <em>i.e.</em>, temporarily disabled with
 *    the intention of fixing it later
 *  - Canceled - a test was canceled, <em>i.e.</em>, not able to be completed
 *    because of some unmet pre-condition, such as a database being offline
 *    that the test requires
 *  - Pending - a test was declared as pending, <em>i.e.</em>, with test code
 *  and/or production code as yet unimplemented
 *
 *  The difference between errors and failures, if any, is determined by the
 *  test frameworks. JUnit and specs2 differentiate between errors and
 *  failures. ScalaTest reports everything (both assertion failures and
 *  unexpected errors) as failures. JUnit and ScalaTest support ignored tests.
 *  ScalaTest and specs2 support a notion of pending tests. ScalaTest
 *  differentiates between ignored and canceled tests, whereas specs2 only
 *  supports skipped tests, which are implemented like ScalaTest's canceled
 *  tests. TestNG uses "skipped" to report tests that were not executed because
 *  of failures in dependencies, which is also similar to canceled tests in
 *  ScalaTest.
 */
class Status private (_index: Int, _name: String) {

  def name(): String = _name
  def ordinal(): Int = _index

  override def toString(): String = name()
}

object Status {

  /** Indicates a test succeeded. */
  final val Success = new Status(0, "Success")

  /** Indicates an "error" occurred. */
  final val Error = new Status(1, "Error")

  /** Indicates a "failure" occurred. */
  final val Failure = new Status(2, "Failure")

  /** Indicates a test was skipped. */
  final val Skipped = new Status(3, "Skipped")

  /** Indicates a test was ignored. */
  final val Ignored = new Status(4, "Ignored")

  /** Indicates a test was canceled. */
  final val Canceled = new Status(5, "Canceled")

  /** Indicates a test was declared as pending. */
  final val Pending = new Status(6, "Pending")

  private[this] val _values: Array[Status] =
    Array(Success, Error, Failure, Skipped, Ignored, Canceled, Pending)

  def values(): Array[Status] = _values.clone()

  def valueOf(name: String): Status = {
    _values.find(_.name == name).getOrElse(
        throw new IllegalArgumentException("No enum const Status." + name))
  }
}
