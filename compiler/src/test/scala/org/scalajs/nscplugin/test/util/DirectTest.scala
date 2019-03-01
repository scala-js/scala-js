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

package org.scalajs.nscplugin.test.util

import scala.tools.nsc._
import scala.tools.nsc.plugins.Plugin
import scala.tools.nsc.reporters.ConsoleReporter

import scala.reflect.internal.util.{SourceFile, BatchSourceFile}

import org.scalajs.nscplugin.ScalaJSPlugin

import scala.collection.mutable

import java.io.File

/** This is heavily inspired by scala's partest suite's DirectTest */
abstract class DirectTest {

  /** these arguments are always added to the args passed to newSettings */
  def extraArgs: List[String] = Nil

  /** create settings objects for test from arg string */
  def newSettings(args: List[String]): Settings = {
    val s = new Settings
    s processArguments (args, true)
    s
  }

  def newScalaJSCompiler(args: String*): Global = {
    val settings = newSettings(
        List(
            "-d", testOutputPath,
            "-bootclasspath", scalaLibPath,
            "-classpath", classpath.mkString(File.pathSeparator)) ++
        extraArgs ++ args.toList)

    lazy val global: Global = new Global(settings, newReporter(settings)) {
      private implicit class PluginCompat(val plugin: Plugin) {
        def options: List[String] = {
          val prefix = plugin.name + ":"
          for {
            option <- settings.pluginOptions.value
            if option.startsWith(prefix)
          } yield {
            option.stripPrefix(prefix)
          }
        }
      }

      override lazy val plugins = {
        val scalaJSPlugin = newScalaJSPlugin(global)
        scalaJSPlugin.processOptions(scalaJSPlugin.options,
            msg => throw new IllegalArgumentException(msg))
        scalaJSPlugin :: Nil
      }
    }

    global
  }

  def newScalaJSPlugin(global: Global): ScalaJSPlugin =
    new ScalaJSPlugin(global)

  def newReporter(settings: Settings): ConsoleReporter =
    new ConsoleReporter(settings)

  private def newSources(codes: String*) = codes.toList.zipWithIndex map {
    case (src, idx) => new BatchSourceFile(s"newSource${idx + 1}.scala", src)
  }

  def withRun[T](global: Global)(f: global.Run => T): T = {
    global.reporter.reset()
    f(new global.Run)
  }

  def compileSources(global: Global)(sources: SourceFile*): Boolean = {
    withRun(global)(_ compileSources sources.toList)
    !global.reporter.hasErrors
  }

  def compileString(global: Global)(sourceCode: String): Boolean =
    compileSources(global)(newSources(sourceCode): _*)

  def compileString(sourceCode: String): Boolean =
    compileString(defaultGlobal)(sourceCode)

  // Cannot reuse global, otherwise compiler crashes with Scala >= 2.11.5
  // on following tests:
  // - org.scalajs.nscplugin.test.JSExportTest
  // - org.scalajs.nscplugin.test.JSDynamicLiteralTest
  // Filed as #1443
  def defaultGlobal: Global = newScalaJSCompiler()

  def testOutputPath: String = {
    val baseDir = System.getProperty("scala.scalajs.compiler.test.output")
    val outDir = new File(baseDir, getClass.getName)
    outDir.mkdirs()
    outDir.getAbsolutePath
  }

  def scalaJSLibPath: String =
    System.getProperty("scala.scalajs.compiler.test.scalajslib")

  def scalaLibPath: String =
    System.getProperty("scala.scalajs.compiler.test.scalalib")

  def scalaReflectPath: String =
    System.getProperty("scala.scalajs.compiler.test.scalareflect")

  def classpath: List[String] = List(scalaJSLibPath)
}
