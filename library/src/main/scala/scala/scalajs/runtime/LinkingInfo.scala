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

package scala.scalajs.runtime

import scala.scalajs.js

/** Information about link-time configuration of Scala.js. */
sealed trait LinkingInfo extends js.Object {
  /** Semantics configuration. */
  val semantics: LinkingInfo.Semantics

  /** Whether we are assuming ECMAScript 6 support or not. */
  val assumingES6: Boolean

  /** Version of the linker */
  val linkerVersion: String

  /** The value of the global JavaScript `this`. */
  val globalThis: Any
}

object LinkingInfo {
  /** Semantics configuration. */
  sealed trait Semantics extends js.Object {
    /** Compliance level of asInstanceOfs. */
    val asInstanceOfs: Int

    /** Compliance level of arrayIndexOutOfBounds. */
    val arrayIndexOutOfBounds: Int

    /** Compliance level of moduleInit. */
    val moduleInit: Int

    /** Whether floats have strict semantics. */
    val strictFloats: Boolean

    /** Whether we are linking in production mode. */
    val productionMode: Boolean
  }

  object Semantics {
    final val Compliant = 0
    final val Fatal = 1
    final val Unchecked = 2
  }
}
