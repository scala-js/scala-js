/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.javascript

/** JavaScript output mode. */
sealed abstract class OutputMode

object OutputMode {
  /** All the available output modes.
   *  There are listed in decreasing order of "importance", as judged by
   *  whoever maintains the back-ends.
   */
  val All = List(
      ECMAScript51Isolated,
      ECMAScript51Global)

  /** Legacy output mode where everything is stored in a global ScalaJS variable.
   *  This is suited to the special Rhino interpreter.
   */
  case object ECMAScript51Global extends OutputMode

  /** Modern output mode compliant with ECMAScript 5.1 in a function scope.
   *  This is the default output mode used by fastOpt and fullOpt.
   *  The output must be enclosed in an anonymous function isolating the code
   *  in a dedicated scope.
   */
  case object ECMAScript51Isolated extends OutputMode
}
