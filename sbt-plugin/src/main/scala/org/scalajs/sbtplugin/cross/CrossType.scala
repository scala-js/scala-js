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

package org.scalajs.sbtplugin.cross

import sbt._

import java.io.File

@deprecated(
    "The built-in cross-project feature of sbt-scalajs is deprecated. " +
    "Use the separate sbt plugin sbt-crossproject instead: " +
    "https://github.com/portable-scala/sbt-crossproject",
    "0.6.23")
abstract class CrossType {

  /** The base directory for a (true sbt) Project
   *  @param crossBase The base directory of the CrossProject
   *  @param projectType "jvm" or "js". Other values may be supported
   */
  def projectDir(crossBase: File, projectType: String): File

  /** The base directory for the JVM project */
  final def jvmDir(crossBase: File): File = projectDir(crossBase, "jvm")

  /** The base directory for the JS project */
  final def jsDir(crossBase: File): File = projectDir(crossBase, "js")

  /** The location of a shared source directory (if it exists)
   *  @param projectBase the base directory of a (true sbt) Project
   *  @param conf name of sub-directory for the configuration (typically "main"
   *      or "test")
   */
  def sharedSrcDir(projectBase: File, conf: String): Option[File]

}

@deprecated(
    "The built-in cross-project feature of sbt-scalajs is deprecated. " +
    "Use the separate sbt plugin sbt-crossproject instead: " +
    "https://github.com/portable-scala/sbt-crossproject",
    "0.6.23")
object CrossType {

  object Full extends CrossType {
    def projectDir(crossBase: File, projectType: String): File =
      crossBase / projectType

    def sharedSrcDir(projectBase: File, conf: String): Option[File] =
      Some(projectBase.getParentFile / "shared" / "src" / conf / "scala")
  }

  object Pure extends CrossType {
    def projectDir(crossBase: File, projectType: String): File =
      crossBase / ("." + projectType)

    def sharedSrcDir(projectBase: File, conf: String): Option[File] =
      Some(projectBase.getParentFile / "src" / conf / "scala")
  }

  object Dummy extends CrossType {
    def projectDir(crossBase: File, projectType: String): File =
      crossBase / projectType

    def sharedSrcDir(projectBase: File, conf: String): Option[File] = None
  }

}
