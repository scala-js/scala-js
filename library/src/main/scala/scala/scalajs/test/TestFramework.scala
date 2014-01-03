package scala.scalajs.test

trait TestFramework {
  def runTests(testOutput: TestOutput)(tests: => Unit)
}
