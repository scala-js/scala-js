package org.scalajs.junit.utils

trait FailureFrameworkArgs {
  protected def frameworkArgss: List[List[String]] = List(
      Nil,
      List("-q"),
      List("-a"),
      List("-v"),
      List("-n"),
      List("-n", "-a"),
      List("-n", "-v"),
      List("-n", "-v", "-a"),
      List("-n", "-v", "-c"),
      List("-n", "-v", "-c", "-a"),
      List("-v", "-q"),
      List("-v", "-a"),
      List("-v", "-c")
  )
}
