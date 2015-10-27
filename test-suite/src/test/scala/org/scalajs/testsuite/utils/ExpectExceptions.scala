package org.scalajs.testsuite.utils

import org.scalajs.jasminetest.JasmineTest

import scala.reflect.ClassTag

trait ExpectExceptions extends JasmineTest {

  final def expectNoException(expr: => Any): Unit =
    expect(() => expr).not.toThrow

  final def expectThrows[A <: Throwable: ClassTag](expr: => Any): Unit = {
    try {
      expr
      expect(implicitly[ClassTag[A]].runtimeClass.getName).toEqual("")
    } catch {
      case _: A         => // This is expected
      case t: Throwable => throw t
    }
  }
}
