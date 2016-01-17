/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.linker

import org.scalajs.core.ir
import ir.Trees._
import ir.Infos

/** A MethodDef or a PropertyDef after linking.
 *
 *  Note that the [[version]] is relative to the identity of a LinkedMember.
 *  The definition of identity varies as linked members progress through the
 *  linking pipeline, but it only gets stronger, i.e., if two linked members
 *  are id-different at phase P, then they must also be id-different at phase
 *  P+1. The converse is not true. This guarantees that versions can be used
 *  reliably to determine at phase P+1 whether a linked member coming from
 *  phase P must be reprocessed.
 */
final class LinkedMember[+T <: Tree](
    val info: Infos.MethodInfo,
    val tree: T,
    val version: Option[String]
)
