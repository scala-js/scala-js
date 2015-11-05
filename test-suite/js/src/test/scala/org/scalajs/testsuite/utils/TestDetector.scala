package org.scalajs.testsuite.utils

import scala.scalajs.js
import js.annotation.JSExport
import js.JSConverters._

/** An ad-hoc but centralized way to detect tests in this test suite */
@JSExport("scalajs.TestDetector")
object TestDetector {

  private final val basePackage = "org.scalajs.testsuite"

  /** Exported Scala.js-defined JS classes cannot be distinguished from
   *  module accessors, so we explicitly blacklist them.
   */
  private val isBlacklisted = Set(
      "SJSDefinedExportedClass",
      "SJSDefinedAutoExportedTraitClass",
      "SJSDefinedAutoExportClass",
      "SJSDefinedAutoExportedClassClass",
      "SJSDefinedAutoExportIgnoreClass",
      "SJSDefinedAutoExportedIgnoreClassClass"
  ).map(basePackage + ".jsinterop." + _)

  def detectTestNames(): List[String] = detectTestsInternal().map(_._2).toList

  @JSExport
  def loadDetectedTests(): Unit = detectTestsInternal().foreach(_._1())

  private def detectTestsInternal(): List[(js.Dynamic, String)] = {
    def isExportedModule(item: js.Dynamic): Boolean = {
      /* We make sure to use only select exported modules (not classes) by
       * checking .prototype of the exporters.
       */
      (js.typeOf(item) == "function") && {
        js.isUndefined(item.prototype) || // happens for static methods
        (js.Object.getPrototypeOf(item.prototype.asInstanceOf[js.Object]) eq
            js.Object.asInstanceOf[js.Dynamic].prototype)
      }
    }

    def rec(item: js.Dynamic, fullName: String): List[(js.Dynamic, String)] = {
      if (isBlacklisted(fullName)) {
        Nil
      } else if (js.typeOf(item) == "object") {
        js.Object.properties(item).toList flatMap { prop =>
          rec(item.selectDynamic(prop), s"$fullName.$prop")
        }
      } else if (isExportedModule(item)) {
        List((item, fullName))
      } else {
        Nil
      }
    }

    val parts = basePackage.split('.')
    val base = parts.foldLeft(js.Dynamic.global)(_.selectDynamic(_))
    rec(base, basePackage)
  }

}
