/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.linker.backend

import org.scalajs.core.tools.javascript.ESLevel

/** JavaScript output mode. */
sealed abstract class OutputMode {
  def esLevel: ESLevel
}

object OutputMode {
  /** All the available output modes.
   *  There are listed in decreasing order of "importance", as judged by
   *  whoever maintains the back-ends.
   */
  val All = List(
      ECMAScript51Isolated,
      ECMAScript6,
      ECMAScript51Global)

  /** The default output mode. This is always the first element of [[All]] */
  val Default = All.head

  /** Legacy output mode where everything is stored in a global ScalaJS variable.
   *  This is suited to the special Rhino interpreter.
   */
  case object ECMAScript51Global extends OutputMode {
    val esLevel: ESLevel = ESLevel.ES5
  }

  /** Modern output mode compliant with ECMAScript 5.1 in a function scope.
   *  This is the default output mode used by fastOpt and fullOpt.
   *  The output must be enclosed in an anonymous function isolating the code
   *  in a dedicated scope.
   */
  case object ECMAScript51Isolated extends OutputMode {
    val esLevel: ESLevel = ESLevel.ES5
  }

  /** Experimental output mode compliant with ECMAScript 6 in a function scope.
   *
   *  This output mode assumes that the target platform supports ECMAScript 6,
   *  at least for the following aspects:
   *
   *  * Classes
   *  * let and const
   *  * Rest parameters and the spread operator (...args)
   *  * New methods in Math
   *  * Symbols and the "well-known symbol" Symbol.iterator
   *
   *  The output must be enclosed in an anonymous function isolating the code
   *  in a dedicated scope.
   */
  case object ECMAScript6 extends OutputMode {
    val esLevel: ESLevel = ESLevel.ES6
  }

  // Not binary compatible, but source compatible with deprecation
  @deprecated("Support for ES6 Strong Mode was removed. Use ECMAScript6 instead.", "0.6.8")
  lazy val ECMAScript6StrongMode: ECMAScript6.type = ECMAScript6
}
