/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2016, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.linker.backend.emitter

private[emitter] class InternalOptions private (
    val optimizeBracketSelects: Boolean) {

  def withOptimizeBracketSelects(optimizeBracketSelects: Boolean): InternalOptions =
    copy(optimizeBracketSelects = optimizeBracketSelects)

  private def copy(
      optimizeBracketSelects: Boolean = this.optimizeBracketSelects): InternalOptions = {
    new InternalOptions(
        optimizeBracketSelects)
  }
}

private[emitter] object InternalOptions {
  def apply(): InternalOptions = {
    new InternalOptions(
        optimizeBracketSelects = true)
  }
}
