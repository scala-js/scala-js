/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.js

import scala.language.{ dynamics, implicitConversions }

class JavaScriptObject private (private val underlying: AnyRef) extends scala.Dynamic {
  @native def applyDynamic(name: String)(args: JavaScriptObject*): JavaScriptObject
  @native def selectDynamic(name: String): JavaScriptObject
  @native def updateDynamic(name: String)(value: JavaScriptObject): Unit
  @native def apply(args: JavaScriptObject*): JavaScriptObject

  @native override def toString(): String
}

object JavaScriptObject {
  @native def window: JavaScriptObject

  @native implicit def fromBoolean(value: Boolean): JavaScriptObject
  @native implicit def fromInt(value: Int): JavaScriptObject
  @native implicit def fromString(value: String): JavaScriptObject
  @native implicit def fromObject(value: AnyRef): JavaScriptObject

  @native def toBoolean(value: JavaScriptObject): Boolean
  @native def toInt(value: JavaScriptObject): Int
  @native def toString(value: JavaScriptObject): String
  @native def toObject(value: JavaScriptObject): AnyRef
}
