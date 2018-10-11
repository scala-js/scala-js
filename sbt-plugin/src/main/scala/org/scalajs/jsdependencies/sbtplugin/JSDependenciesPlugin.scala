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

package org.scalajs.jsdependencies.sbtplugin

import sbt._

import org.scalajs.sbtplugin.ScalaJSPlugin

/** Shim for [[https://github.com/scala-js/jsdependencies sbt-jsdependencies]]
 *  in Scala.js 0.6.x.
 *
 *  This sbt plugin is empty. It only serves as a source-compatible shim to
 *  be able to cross-compile builds across `sbt-scalajs` 0.6.x and
 *  `sbt-jsdependencies` 1.x.
 */
object JSDependenciesPlugin extends AutoPlugin {
  override def requires: Plugins = ScalaJSPlugin

  object autoImport

  lazy val configSettings: Seq[Setting[_]] = Nil

  lazy val compileSettings: Seq[Setting[_]] = Nil

  lazy val testSettings: Seq[Setting[_]] = Nil
}
