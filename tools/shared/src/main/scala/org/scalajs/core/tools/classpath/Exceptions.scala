/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.classpath

import org.scalajs.core.tools.jsdep.ResolutionInfo

class MissingJSLibException(val dependencies: List[ResolutionInfo])
  extends Exception(MissingJSLibException.mkMsg(dependencies))

object MissingJSLibException {
  private def mkMsg(deps: List[ResolutionInfo]): String = {
    val msg = new StringBuilder()
    msg.append("Missing dependencies: \n")
    for (d <- deps) {
      msg.append(s"- ${d.resourceName}")
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
