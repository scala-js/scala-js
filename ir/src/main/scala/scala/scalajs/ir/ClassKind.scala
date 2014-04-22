/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js IR                **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2014, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.ir

sealed abstract class ClassKind {
  import ClassKind._

  def isClass = this match {
    case Class | ModuleClass => true
    case _                   => false
  }

  def isType = this match {
    case TraitImpl => false
    case _         => true
  }
}

object ClassKind {
  case object Class extends ClassKind
  case object ModuleClass extends ClassKind
  case object Interface extends ClassKind
  case object RawJSType extends ClassKind
  case object HijackedClass extends ClassKind
  case object TraitImpl extends ClassKind
}
