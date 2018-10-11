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
