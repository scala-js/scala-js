package org.scalajs.testsuite.utils

import scala.scalajs.js
import js.annotation.JSExport
import js.JSConverters._

/** An ad-hoc but centralized way to detect tests in this test suite */
@JSExport("scalajs.TestDetector")
object TestDetector {

  private final val basePackage = "org.scalajs.testsuite"

  def detectTestNames(): List[String] = detectTestsInternal().map(_._2).toList

  @JSExport
  def loadDetectedTests(): Unit = detectTestsInternal().foreach(_._1())

  private def detectTestsInternal() = {
    val parts = basePackage.split('.')
    val base = parts.foldLeft(js.Dynamic.global)(_.selectDynamic(_))

    // We make sure to use only exported modules (not classes) by checking
    // .prototype of the exporters.
    for {
      pName    <- js.Object.properties(base)
      testName <- js.Object.properties(base.selectDynamic(pName))
      test      = base.selectDynamic(pName).selectDynamic(testName)
      if js.Object.getPrototypeOf(test.prototype.asInstanceOf[js.Object]) eq
         js.Object.asInstanceOf[js.Dynamic].prototype
    } yield (test, s"$basePackage.$pName.$testName")
  }

}
