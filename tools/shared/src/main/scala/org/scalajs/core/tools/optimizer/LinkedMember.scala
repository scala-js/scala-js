/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.optimizer

import org.scalajs.core.ir
import ir.Trees._
import ir.Infos

/** A MethodDef or a PropertyDef after linking */
final case class LinkedMember[+T <: Tree](
    info: Infos.MethodInfo,
    tree: T,
    version: Option[String]
)
