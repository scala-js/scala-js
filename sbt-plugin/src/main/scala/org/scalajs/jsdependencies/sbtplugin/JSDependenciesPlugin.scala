/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2017, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */

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
