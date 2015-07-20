/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.js.annotation

/** IMPLEMENTATION DETAIL: Marks the annotated member as exposed as a JS member.
 *
 *  This annotation is added automatically by the compiler to all public and
 *  protected members of a Scala.js-defined JS class. It marks the annotated
 *  member as being exposed as a JS member.
 *
 *  Do not use this annotation yourself.
 */
class ExposedJSMember extends scala.annotation.StaticAnnotation
