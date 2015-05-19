package scala.scalajs.runtime

import scala.scalajs.js

/** Information about link-time configuration of Scala.js. */
trait LinkingInfo extends js.Object {
  /** Semantics configuration. */
  val semantics: LinkingInfo.Semantics = js.native

  /** Whether we are assuming ECMAScript 6 support or not. */
  val assumingES6: Boolean = js.native
}

object LinkingInfo {
  /** Semantics configuration. */
  trait Semantics extends js.Object {
    /** Compliance level of asInstanceOfs. */
    val asInstanceOfs: Int = js.native

    /** Compliance level of moduleInit. */
    val moduleInit: Int = js.native

    /** Whether floats have strict semantics. */
    val strictFloats: Boolean = js.native
  }

  object Semantics {
    final val Compliant = 0
    final val Fatal = 1
    final val Unchecked = 2
  }
}
