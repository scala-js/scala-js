package org.scalajs.core.tools.jsdep

abstract class DependencyException(msg: String) extends Exception(msg)

class MissingDependencyException(
  val originatingLib: FlatJSDependency,
  val missingLib: String
) extends DependencyException(
  s"The JS dependency ${originatingLib.resourceName} declared " +
  s"from ${originatingLib.origin} has an unmet transitive " +
  s"dependency $missingLib")

class CyclicDependencyException(
  val participants: List[ResolutionInfo]
) extends DependencyException(
  CyclicDependencyException.mkMsg(participants))

object CyclicDependencyException {
  private def mkMsg(parts: List[ResolutionInfo]) = {
    val lookup = parts.map(p => (p.resourceName, p)).toMap

    val msg = new StringBuilder()
    msg.append("There is a loop in the following JS dependencies:\n")

    def str(info: ResolutionInfo) =
      s"${info.resourceName} from: ${info.origins.mkString(", ")}"

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
    val resName = parts.head.resourceName

    val msg = new StringBuilder()
    msg.append(s"Name conflicts in:\n")

    for (p <- parts) {
      msg.append(p)
      msg.append('\n')
    }

    sys.error(msg.toString())
  }
}
