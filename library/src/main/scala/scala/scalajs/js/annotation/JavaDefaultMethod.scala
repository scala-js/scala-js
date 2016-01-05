/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.js.annotation

/** Mark a concrete trait method as a Java default method.
 *
 *  This annotation can be used on concrete trait methods to mark them as
 *  Java default methods. This should be used *only* to implement interfaces
 *  of the JDK that have default methods in Java.
 *
 *  Otherwise using this annotation is unspecified.
 */
class JavaDefaultMethod extends scala.annotation.StaticAnnotation
