/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2016, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */

package scala.scalajs.js.annotation.internal

import scala.annotation.meta._

/** IMPLEMENTATION DETAIL: Marks concrete members of Scala.js-defined JS
 *  traits, so that they can be identified by the back-end not to emit them.
 *
 *  Internally, such members are known as "optional", in reference to their
 *  primary intended use case: optional fields in configuration objects.
 *
 *  Do not use this annotation yourself.
 */
@field @getter @setter
class JSOptional extends scala.annotation.StaticAnnotation
