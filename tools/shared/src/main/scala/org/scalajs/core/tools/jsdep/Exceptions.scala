package org.scalajs.core.tools.jsdep

abstract class DependencyException(msg: String) extends Exception(msg)

class MissingDependencyException(
  val originatingLib: FlatJSDependency,
  val missingLib: String
) extends DependencyException(
  s"The JS dependency ${originatingLib.relPath} declared " +
  s"from ${originatingLib.origin} has an unmet transitive " +
  s"dependency $missingLib")

class CyclicDependencyException(
  val participants: List[ResolutionInfo]
) extends DependencyException(
  CyclicDependencyException.mkMsg(participants))

object CyclicDependencyException {
  private def mkMsg(parts: List[ResolutionInfo]) = {
    val lookup = parts.map(p => (p.relPath, p)).toMap

    val msg = new StringBuilder()
    msg.append("There is a loop in the following JS dependencies:\n")

    def str(info: ResolutionInfo) =
      s"${info.relPath} from: ${info.origins.mkString(", ")}"

    for (dep <- parts) {
      msg.append(s"  ${str(dep)} which depends on\n")
      for (name <- dep.dependencies) {
        val rdep = lookup(name)
        msg.append(s"    - ${str(rdep)}\n")
      }
    }

    msg.toString()
  }
}

class ConflictingNameException(
  val participants: List[FlatJSDependency]
) extends DependencyException(
  ConflictingNameException.mkMsg(participants))

object ConflictingNameException {
  private def mkMsg(parts: List[FlatJSDependency]) = {
    val msg = new StringBuilder()
    msg.append(s"CommonJSName conflicts in:\n")

    for (p <- parts) {
      msg.append(p)
      msg.append('\n')
    }

    msg.toString()
  }
}

class ConflictingMinifiedJSException(
  val participants: List[FlatJSDependency]
) extends DependencyException(
  ConflictingMinifiedJSException.mkMsg(participants))

object ConflictingMinifiedJSException {
  private def mkMsg(parts: List[FlatJSDependency]) = {
    val msg = new StringBuilder()
    msg.append(s"Minified JS conflicts in:\n")

    for (p <- parts) {
      msg.append(p)
      msg.append('\n')
    }

    msg.toString()
  }
}

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
