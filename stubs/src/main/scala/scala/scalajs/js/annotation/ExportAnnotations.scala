/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.scalajs.js.annotation

import scala.annotation.Annotation

class JSExportAll extends scala.annotation.Annotation

class JSExportDescendentObjects(ignoreInvalidDescendants: Boolean)
    extends scala.annotation.Annotation {
  def this() = this(false)
}

class JSExportDescendentClasses(ignoreInvalidDescendants: Boolean)
    extends scala.annotation.Annotation {
  def this() = this(false)
}

class JSExportNamed extends scala.annotation.Annotation {
  def this(name: String) = this()
}

class JSExport extends scala.annotation.Annotation {
  def this(name: String) = this()
}

class JSExportTopLevel(name: String) extends scala.annotation.Annotation
