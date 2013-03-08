/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.js

import scala.language.dynamics

class JavaScriptObject private (private val underlying: AnyRef) extends scala.Dynamic {
  @native def applyDynamic(name: String)(args: Any*): Any
  @native def selectDynamic(name: String): Any
  @native def updateDynamic(name: String)(value: Any): Unit
}

object JavaScriptObject {
  @native def window: JavaScriptObject
}
