package scala.scalajs.runtime

import scala.scalajs.js

/** Information about link-time configuration of Scala.js. */
@js.native
trait LinkingInfo extends js.Object {
  /** Environment info. */
  val envInfo: EnvironmentInfo = js.native

  /** Semantics configuration. */
  val semantics: LinkingInfo.Semantics = js.native

  /** Whether we are assuming ECMAScript 6 support or not. */
  val assumingES6: Boolean = js.native

  /** Version of the linker */
  val linkerVersion: js.UndefOr[String] = js.native
}

object LinkingInfo {
  /** Semantics configuration. */
  @js.native
  trait Semantics extends js.Object {
    /** Compliance level of asInstanceOfs. */
    val asInstanceOfs: Int = js.native

    /** Compliance level of arrayIndexOutOfBounds. */
    val arrayIndexOutOfBounds: Int = js.native

    /** Compliance level of moduleInit. */
    val moduleInit: Int = js.native

    /** Whether floats have strict semantics. */
    val strictFloats: Boolean = js.native

    /** Whether we are linking in production mode. */
    val productionMode: Boolean = js.native
  }

  object Semantics {
    final val Compliant = 0
    final val Fatal = 1
    final val Unchecked = 2
  }
}
