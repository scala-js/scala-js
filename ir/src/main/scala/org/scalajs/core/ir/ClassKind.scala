/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js IR                **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2014, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.ir

import scala.annotation.switch

sealed abstract class ClassKind {
  import ClassKind._

  def isClass: Boolean = this match {
    case Class | ModuleClass => true
    case _                   => false
  }

  def isJSClass: Boolean = this match {
    case JSClass | JSModuleClass => true
    case _                       => false
  }

  def isJSType: Boolean = this match {
    case AbstractJSType | JSClass | JSModuleClass | NativeJSClass |
        NativeJSModuleClass =>
      true
    case _ =>
      false
  }

  def hasModuleAccessor: Boolean = this match {
    case ModuleClass | JSModuleClass => true
    case _                           => false
  }

  def isAnyScalaJSDefinedClass: Boolean = this match {
    case Class | ModuleClass | JSClass | JSModuleClass => true
    case _                                             => false
  }

  def withoutModuleAccessor: ClassKind = this match {
    case ModuleClass         => Class
    case JSModuleClass       => JSClass
    case NativeJSModuleClass => AbstractJSType
    case _                   => this
  }
}

object ClassKind {
  case object Class extends ClassKind
  case object ModuleClass extends ClassKind
  case object Interface extends ClassKind
  case object AbstractJSType extends ClassKind
  case object HijackedClass extends ClassKind
  case object JSClass extends ClassKind
  case object JSModuleClass extends ClassKind
  case object NativeJSClass extends ClassKind
  case object NativeJSModuleClass extends ClassKind

  private[ir] def toByte(kind: ClassKind): Byte = kind match {
    case ClassKind.Class               => 1
    case ClassKind.ModuleClass         => 2
    case ClassKind.Interface           => 3
    case ClassKind.AbstractJSType      => 4
    case ClassKind.HijackedClass       => 5
    case ClassKind.JSClass             => 6
    case ClassKind.JSModuleClass       => 7
    case ClassKind.NativeJSClass       => 8
    case ClassKind.NativeJSModuleClass => 9
  }

  private[ir] def fromByte(b: Byte): ClassKind = (b: @switch) match {
    case 1 => ClassKind.Class
    case 2 => ClassKind.ModuleClass
    case 3 => ClassKind.Interface
    case 4 => ClassKind.AbstractJSType
    case 5 => ClassKind.HijackedClass
    case 6 => ClassKind.JSClass
    case 7 => ClassKind.JSModuleClass
    case 8 => ClassKind.NativeJSClass
    case 9 => ClassKind.NativeJSModuleClass
  }
}
