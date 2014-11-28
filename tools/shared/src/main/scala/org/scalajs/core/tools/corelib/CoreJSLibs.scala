/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.corelib

import java.net.URI

import org.scalajs.core.ir.ScalaJSVersions
import org.scalajs.core.tools.io._

import org.scalajs.core.tools.sem._

import scala.collection.immutable.Seq
import scala.collection.mutable

object CoreJSLibs {

  private val cachedLibBySemantics =
    mutable.HashMap.empty[Semantics, VirtualJSFile]

  private val ScalaJSEnvLines =
    ScalaJSEnvHolder.scalajsenv.split("\n|\r\n?")

  private val gitHubBaseURI =
    new URI("https://raw.githubusercontent.com/scala-js/scala-js/")

  /** A JS expression that detects the global scope just like Scala.js */
  val jsGlobalExpr: String =
    """((typeof global === "object" && global &&
         global["Object"] === Object) ? global : this)"""

  def libs(semantics: Semantics): Seq[VirtualJSFile] = synchronized {
    Seq(cachedLibBySemantics.getOrElseUpdate(semantics, makeLib(semantics)))
  }

  private def makeLib(semantics: Semantics): VirtualJSFile = {
    new ScalaJSEnvVirtualJSFile(makeContent(semantics))
  }

  private def makeContent(semantics: Semantics): String = {
    // This is a basic sort-of-C-style preprocessor

    def getOption(name: String): String = name match {
      case "asInstanceOfs" =>
        semantics.asInstanceOfs.toString()
      case "floats" =>
        if (semantics.strictFloats) "Strict"
        else "Loose"
    }

    var skipping = false
    var skipDepth = 0
    val lines = for (line <- ScalaJSEnvLines) yield {
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

    lines.mkString("", "\n", "\n")
  }

  private class ScalaJSEnvVirtualJSFile(override val content: String) extends VirtualJSFile {
    override def path: String = "scalajsenv.js"
    override def version: Option[String] = Some("")
    override def exists: Boolean = true

    override def toURI: URI = {
      if (!ScalaJSVersions.currentIsSnapshot)
        gitHubBaseURI.resolve(s"v${ScalaJSVersions.current}/tools/$path")
      else
        super.toURI
    }
  }

}
