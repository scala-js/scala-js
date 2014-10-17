/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.tools.corelib

import java.net.URI

import scala.scalajs.ir.ScalaJSVersions
import scala.scalajs.tools.io._

import scala.scalajs.tools.sem._

import scala.collection.immutable.Seq
import scala.collection.mutable

object CoreJSLibs {

  private val cachedLibBySemantics =
    mutable.HashMap.empty[Semantics, VirtualJSFile]

  private val ScalaJSEnvLines =
    ScalaJSEnvHolder.scalajsenv.split("\n|\r\n?")

  private val gitHubBaseURI =
    new URI("https://raw.githubusercontent.com/scala-js/scala-js/")

  def libs(semantics: Semantics): Seq[VirtualJSFile] = synchronized {
    Seq(cachedLibBySemantics.getOrElseUpdate(semantics, makeLib(semantics)))
  }

  private def makeLib(semantics: Semantics): VirtualJSFile = {
    new ScalaJSEnvVirtualJSFile(makeContent(semantics))
  }

  private def makeContent(semantics: Semantics): String = {
    // This is a basic sort-of-C-style preprocessor

    def getOption(name: String): CheckedBehaviors.Behavior = name match {
      case "asInstanceOfs" => semantics.checkedBehaviors.asInstanceOfs
    }

    def parseBehavior(name: String): CheckedBehaviors.Behavior = name match {
      case "Compliant" => CheckedBehaviors.Compliant
      case "Fatal"     => CheckedBehaviors.Fatal
      case "Unchecked" => CheckedBehaviors.Unchecked
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
            val Array(_, sOpt, sOp, sValue) = line.split(" ")
            val opt = getOption(sOpt)
            val value = parseBehavior(sValue)
            val success = sOp match {
              case "==" => opt == value
              case "!=" => opt != value
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
