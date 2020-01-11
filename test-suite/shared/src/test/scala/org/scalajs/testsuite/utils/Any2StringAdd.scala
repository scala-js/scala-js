package org.scalajs.testsuite.utils


object Any2StringAdd {
  implicit final class any2stringadd[A](private val self: A) extends AnyVal {
    def +(other: String): String = String.valueOf(self) + other
  }
}