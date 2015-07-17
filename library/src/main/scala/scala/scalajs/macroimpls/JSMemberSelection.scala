/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-lang.org/     **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.macroimpls

/** Represents the way a member of a JS object is selected */
private[macroimpls] sealed abstract class JSMemberSelection

/** A member with statically known name */
private[macroimpls] final case class JSNamedMember(name: String)
    extends JSMemberSelection

/** Calling the object */
private[macroimpls] case object JSMemberCall extends JSMemberSelection

/** Accessing via brackets (array-like access) */
private[macroimpls] case object JSMemberBracketAccess extends JSMemberSelection

/** Accessing and calling a member via brackets (with dynamic name) */
private[macroimpls] case object JSMemberBracketCall extends JSMemberSelection
