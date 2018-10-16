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

package org.scalajs.linker.backend.emitter

import java.net.URI

import org.scalajs.ir.ScalaJSVersions

import org.scalajs.linker._

import scala.collection.immutable.Seq
import scala.collection.mutable

private[emitter] object CoreJSLibs {

  private type Config = (Semantics, ESFeatures, ModuleKind)

  private val cachedLibByConfig = mutable.HashMap.empty[Config, String]

  private val ScalaJSEnvLines =
    ScalaJSEnvHolder.scalajsenv.split("\n|\r\n?")

  private val gitHubBaseURI =
    new URI("https://raw.githubusercontent.com/scala-js/scala-js/")

  def lib(semantics: Semantics, esFeatures: ESFeatures,
      moduleKind: ModuleKind): String = {
    synchronized {
      cachedLibByConfig.getOrElseUpdate(
          (semantics, esFeatures, moduleKind),
          makeLib(semantics, esFeatures, moduleKind))
    }
  }

  def locationForSourceMap: URI = ScalaJSEnvHolder.sourceMapPath

  private def makeLib(semantics: Semantics, esFeatures: ESFeatures,
      moduleKind: ModuleKind): String = {
    // This is a basic sort-of-C-style preprocessor

    def getOption(name: String): String = name match {
      case "asInstanceOfs" =>
        semantics.asInstanceOfs.toString()
      case "arrayIndexOutOfBounds" =>
        semantics.arrayIndexOutOfBounds.toString()
      case "moduleInit" =>
        semantics.moduleInit.toString()
      case "floats" =>
        if (semantics.strictFloats) "Strict"
        else "Loose"
      case "productionMode" =>
        semantics.productionMode.toString()
      case "useECMAScript2015" =>
        esFeatures.useECMAScript2015.toString()
      case "moduleKind" =>
        moduleKind.toString()
      case "longImpl" =>
        if (esFeatures.allowBigIntsForLongs) "BigInt"
        else "RuntimeLong"
    }

    val originalLines = ScalaJSEnvLines

    var skipping = false
    var skipDepth = 0
    val lines = for (line <- originalLines) yield {
      val includeThisLine = if (skipping) {
        if (line == "//!else" && skipDepth == 1) {
          skipping = false
          skipDepth = 0
        } else if (line == "//!endif") {
          skipDepth -= 1
          if (skipDepth == 0)
            skipping = false
        } else if (line.startsWith("//!if ")) {
          skipDepth += 1
        }
        false
      } else {
        if (line.startsWith("//!")) {
          if (line.startsWith("//!if ")) {
            val Array(_, option, op, value) = line.split(" ")
            val optionValue = getOption(option)
            val success = op match {
              case "==" => optionValue == value
              case "!=" => optionValue != value
            }
            if (!success) {
              skipping = true
              skipDepth = 1
            }
          } else if (line == "//!else") {
            skipping = true
            skipDepth = 1
          } else if (line == "//!endif") {
            // nothing to do
          } else {
            throw new MatchError(line)
          }
          false
        } else {
          true
        }
      }
      if (includeThisLine) line
      else "" // blank line preserves line numbers in source maps
    }

    val content = lines.mkString("", "\n", "\n").replace(
        "{{LINKER_VERSION}}", ScalaJSVersions.current)

    if (esFeatures.useECMAScript2015)
      content
    else
      content.replaceAll(raw"\b(let|const)\b", "var")
  }
}
