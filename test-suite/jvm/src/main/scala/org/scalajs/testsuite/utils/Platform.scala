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

  def executingInJVMOnJDK8OrLower: Boolean = jdkVersion <= 8

  def executingInJVMOnLowerThanJDK10: Boolean = jdkVersion < 10

  def executingInJVMOnLowerThanJDK13: Boolean = jdkVersion < 13

  def executingInJVMOnLowerThanJDK15: Boolean = jdkVersion < 15

  def executingInJVMOnLowerThanJDK16: Boolean = jdkVersion < 16

  def executingInJVMOnLowerThanJDK17: Boolean = jdkVersion < 17

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
  def hasStrictFloats: Boolean = true
  def hasAccurateFloats: Boolean = true

  def regexSupportsUnicodeCase: Boolean = true
  def regexSupportsUnicodeCharacterClasses: Boolean = true
  def regexSupportsLookBehinds: Boolean = true
}
