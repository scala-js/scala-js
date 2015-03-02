/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.classpath

import org.scalajs.core.tools.jsdep.{Origin, ResolutionInfo}

class JSLibResolveException(val problems: List[JSLibResolveException.Problem])
    extends Exception(JSLibResolveException.mkMsg(problems))

object JSLibResolveException {
  final class Problem(val resourceName: String,
      val possiblePaths: List[String], val origins: List[Origin]) {
    def isMissing: Boolean = possiblePaths.isEmpty
    def isAmbiguous: Boolean = possiblePaths.nonEmpty
  }

  private def mkMsg(problems: List[Problem]): String = {
    val msg = new StringBuilder
    msg.append("Some references to JS libraries could not be resolved:\n")
    for (p <- problems) {
      if (p.isMissing) {
        msg.append(s"- Missing JS library: ${p.resourceName}\n")
      } else {
        msg.append(s"- Ambiguous reference to a JS library: ${p.resourceName}\n")
        msg.append("  Possible paths found on the classpath:\n")
        for (relPath <- p.possiblePaths)
          msg.append(s"  - $relPath\n")
      }
      msg.append(s"  originating from: ${p.origins.mkString(", ")}\n")
    }
    msg.toString()
  }
}

@deprecated("MissingJSLibException has been replaced by JSLibResolveException.", "0.6.1")
class MissingJSLibException(val dependencies: List[ResolutionInfo])
  extends Exception(MissingJSLibException.mkMsg(dependencies))

@deprecated("MissingJSLibException has been replaced by JSLibResolveException.", "0.6.1")
object MissingJSLibException {
  private def mkMsg(deps: List[ResolutionInfo]): String = {
    val msg = new StringBuilder()
    msg.append("Missing dependencies: \n")
    for (d <- deps) {
      msg.append(s"- ${d.relPath}")
      msg.append(s" originating from: ${d.origins.mkString(", ")}\n")
    }
    msg.toString()
  }
}

class BadComplianceException(val unmet: List[ComplianceRequirement])
    extends Exception(BadComplianceException.mkMsg(unmet))

object BadComplianceException {
  private def mkMsg(unmets: List[ComplianceRequirement]): String = {
    val msg = new StringBuilder()
    msg.append("Unmet required semantic compliance(s): \n")
    for (unmet <- unmets) {
      msg.append(s"- ${unmet.semantics}")
      msg.append(s" originating from: ${unmet.origins.mkString(", ")}\n")
    }
    msg.toString
  }
}
