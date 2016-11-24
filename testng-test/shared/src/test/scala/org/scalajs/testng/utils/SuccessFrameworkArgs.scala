package org.scalajs.junit.utils

trait SuccessFrameworkArgs {
  protected def frameworkArgss: List[List[String]] = List(
      Nil,
      List("-n"),
      List("-v"),
      List("-n", "-v"),
      List("-q"),
      List("-v", "-q")
  )
}
