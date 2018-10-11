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
