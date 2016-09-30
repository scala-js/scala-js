/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2016, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.js.annotation

/** Marks the annotated class or object as imported from another JS module.
 *
 *  Intuitively, this corresponds to the following ECMAScript import
 *  directive:
 *  {{{
 *  import { <name> as AnnotatedClassOrObject } from <module>
 *  }}}
 *
 *  To import the default export of a module, use `JSImport.Default` as `name`.
 *
 *  `@JSImport` is not compatible with the `jsDependencies` mechanism offered
 *  by the Scala.js sbt plugin. You are responsible for resolving and/or
 *  bundling the JavaScript modules that you are importing using other
 *  mechanisms.
 */
class JSImport(module: String, name: String)
    extends scala.annotation.StaticAnnotation {

  /** Namespace import (import the module itself).
   *
   *  The second parameter should be the singleton `JSImport.Namespace`.
   *
   *  Intuitively, this corresponds to
   *  {{{
   *  import * as AnnotatedObject from <module>
   *  }}}
   */
  def this(module: String, name: JSImport.Namespace.type) =
    this(module, null: String)
}

object JSImport {
  /** Use as the `name` of a `JSImport` to use the default import.
   *
   *  The actual value of this constant, the string `"default"`, is not
   *  arbitrary. It is the name under which a default export is registered in
   *  the ECMAScript 2015 specification.
   */
  final val Default = "default"

  /** Use as the `name` of a `JSImport` to use a namespace import.
   *
   *  Intuitively, it corresponds to `*` in an ECMAScript import:
   *  {{{
   *  import * as AnnotatedObject from <module>
   *  }}}
   */
  object Namespace
}
