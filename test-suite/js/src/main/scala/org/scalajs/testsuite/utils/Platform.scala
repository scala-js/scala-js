/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.utils

import scala.scalajs.js
import scala.scalajs.runtime

object Platform {

  def scalaVersion: String =
    System.getProperty("scalajs.scalaVersion")

  /** Returns `true` if and only if the code is executing on a JVM.
   *  Note: Returns `false` when executing on any JS VM.
   */
  final val executingInJVM = false

  final val executingInJVMOnJDK6 = false

  final val executingInJVMOnJDK7OrLower = false

  // Members that are only accessible from testSuite/js
  // (i.e. do no link on the JVM).

  def areTypedArraysSupported: Boolean =
    js.typeOf(js.Dynamic.global.Int32Array) != "undefined"

  def areJSSymbolsSupported: Boolean =
    js.typeOf(js.Dynamic.global.Symbol) != "undefined"

  def executingInNodeJS: Boolean = sysProp("nodejs")
  def executingInBrowser: Boolean = sysProp("browser")
  def executingInUnknownJSEnv: Boolean = sysProp("unknown-jsenv")
  def typedArrays: Boolean = sysProp("typedarray")
  def sourceMaps: Boolean = sysProp("source-maps")

  def isInFastOpt: Boolean = sysProp("fastopt-stage")
  def isInFullOpt: Boolean = sysProp("fullopt-stage")
  def isInProductionMode: Boolean = sysProp("production-mode")
  def isInDevelopmentMode: Boolean = sysProp("development-mode")

  def hasCompliantAsInstanceOfs: Boolean = sysProp("compliant-asinstanceofs")

  def hasCompliantArrayIndexOutOfBounds: Boolean =
    sysProp("compliant-arrayindexoutofbounds")

  def hasCompliantModule: Boolean = sysProp("compliant-moduleinit")
  def hasStrictFloats: Boolean = sysProp("strict-floats")

  def isNoModule: Boolean = sysProp("modulekind-nomodule")
  def isCommonJSModule: Boolean = sysProp("modulekind-commonjs")

  private def sysProp(key: String): Boolean =
    System.getProperty("scalajs." + key, "false") == "true"

  /** Runs the specified piece of code in the global context.
   *
   *  This only works on Node.js. It needs functionality from the `vm` module.
   *
   *  This method can be used to declare global let/const/classes. Any other
   *  attempt to do so (e.g., using a `require`d source file or a `js.eval`)
   *  would not expose the bindings in the global scope.
   */
  def nodejs_runInThisContext(code: String): Unit = {
    val vm = js.Dynamic.global.require("vm")
    val script = js.Dynamic.newInstance(vm.Script)(code)
    script.runInThisContext()
  }
}
