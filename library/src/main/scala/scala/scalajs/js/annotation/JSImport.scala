/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.scalajs.js.annotation

/** Marks the annotated class or object as imported from another JS module.
 *
 *  Intuitively, this corresponds to ECMAScript import directives. See the
 *  documentation of the various constructors.
 *
 *  `@JSImport` is not compatible with the `jsDependencies` mechanism offered
 *  by the Scala.js sbt plugin. You are responsible for resolving and/or
 *  bundling the JavaScript modules that you are importing using other
 *  mechanisms.
 */
class JSImport private () extends scala.annotation.StaticAnnotation {

  /** Named import of a member of the module.
   *
   *  Intuitively, this corresponds to the following ECMAScript import
   *  directive:
   *  {{{
   *  import { <name> as AnnotatedClassOrObject } from <module>
   *  }}}
   *
   *  To import the default export of a module, use `JSImport.Default` as
   *  `name`.
   */
  def this(module: String, name: String) = this()

  /** Namespace import (import the module itself).
   *
   *  The second parameter should be the singleton `JSImport.Namespace`.
   *
   *  Intuitively, this corresponds to
   *  {{{
   *  import * as AnnotatedObject from <module>
   *  }}}
   */
  def this(module: String, name: JSImport.Namespace.type) = this()

  /** Named import of a member of the module, with a fallback on a global
   *  variable.
   *
   *  When linking with module support, this is equivalent to
   *  `@JSImport(module, name)`.
   *
   *  When linking without module support, this is equivalent to
   *  `@JSGlobal(globalFallback)`.
   */
  def this(module: String, name: String, globalFallback: String) = this()

  /** Namespace import (import the module itself), with a fallback on a global
   *  variable.
   *
   *  When linking with module support, this is equivalent to
   *  `@JSImport(module, name)`.
   *
   *  When linking without module support, this is equivalent to
   *  `@JSGlobal(globalFallback)`.
   */
  def this(module: String, name: JSImport.Namespace.type,
      globalFallback: String) = this()
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
