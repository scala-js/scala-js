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

object Platform {

  def scalaVersion: String =
    scala.util.Properties.versionNumberString

  /** Returns `true` if and only if the code is executing on a JVM.
   *  Note: Returns `false` when executing on any JS VM.
   */
  final val executingInJVM = true

  def executingInJVMOnLowerThanJDK(version: Int): Boolean = jdkVersion < version

  def executingInJVMWithJDKIn(range: Range): Boolean = range.contains(jdkVersion)

  private lazy val jdkVersion = {
    val v = System.getProperty("java.version")
    if (v.startsWith("1.")) Integer.parseInt(v.drop(2).takeWhile(_.isDigit))
    else Integer.parseInt(v.takeWhile(_.isDigit))
  }

  final val executingInWebAssembly = false

  def usesClosureCompiler: Boolean = false

  def hasMinifiedNames: Boolean = false

  def hasCompliantAsInstanceOfs: Boolean = true
  def hasCompliantArrayIndexOutOfBounds: Boolean = true
  def hasCompliantArrayStores: Boolean = true
  def hasCompliantNegativeArraySizes: Boolean = true
  def hasCompliantNullPointers: Boolean = true
  def hasCompliantStringIndexOutOfBounds: Boolean = true
  def hasCompliantModule: Boolean = true
  def hasDirectBuffers: Boolean = true

  def regexSupportsUnicodeCase: Boolean = true
  def regexSupportsUnicodeCharacterClasses: Boolean = true
  def regexSupportsLookBehinds: Boolean = true
}
