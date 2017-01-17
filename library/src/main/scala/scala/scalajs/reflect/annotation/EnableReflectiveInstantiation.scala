/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2017, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package scala.scalajs.reflect.annotation

/** Enables reflective instantiation for the annotated class, trait or object,
 *  and all its descendants.
 *
 *  Affected classes can be identified at run-time through methods of
 *  [[scala.scalajs.reflect.Reflect]].
 */
class EnableReflectiveInstantiation extends scala.annotation.StaticAnnotation
