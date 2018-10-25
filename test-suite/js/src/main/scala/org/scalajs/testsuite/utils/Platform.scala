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

  final val executingInJVMOnJDK8OrLower = false

  // Members that are only accessible from testSuite/js
  // (i.e. do no link on the JVM).

  def areTypedArraysSupported: Boolean =
    runtime.Bits.areTypedArraysSupported

  def areJSSymbolsSupported: Boolean =
    !js.isUndefined(js.Dynamic.global.Symbol)

  def executingInRhino: Boolean = sysProp("rhino")
  def executingInNodeJS: Boolean = sysProp("nodejs")
  def executingInNodeJSOnJSDOM: Boolean = sysProp("nodejs.jsdom")
  def executingInPhantomJS: Boolean = sysProp("phantomjs")
  def executingInBrowser: Boolean = sysProp("browser")
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
  def isESModule: Boolean = sysProp("modulekind-esmodule")
  def isCommonJSModule: Boolean = sysProp("modulekind-commonjs")

  private def sysProp(key: String): Boolean =
    System.getProperty("scalajs." + key, "false") == "true"
}
