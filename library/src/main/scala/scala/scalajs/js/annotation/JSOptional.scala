/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2016, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */

package scala.scalajs.js.annotation

import scala.annotation.meta._

/** Marks a member of a JavaScript class/trait/object as an optional field.
 *
 *  `@JSOptional` can annotate a concrete `val`, `var` or `def` without
 *  parentheses in a non-native JavaScript trait, class or object.
 *  The right-hand-side of that member must be `= js.undefined`, and its type
 *  must therefore be a supertype of `js.UndefOr[Nothing]`.
 *
 *  A field annotated with `@JSOptional` is not actually created on a JavaScript
 *  class or object. Instead, accessing it will return `undefined` because the
 *  field is not present.
 *
 *  `@JSOptional` members are the only concrete members that can appear in a
 *  non-native JS trait. This is particularly useful for "option bags": JS
 *  traits that describe objects where fields are options with default values.
 */
@field @getter @setter
class JSOptional extends scala.annotation.StaticAnnotation
