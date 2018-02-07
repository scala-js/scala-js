/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js linker            **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2018, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.linker

/** ECMAScript features to use when linking to JavaScript.
 *
 *  The options in `ESFeatures` specify what features of modern versions of
 *  JavaScript are used by the Scala.js linker.
 *
 *  - Options whose name is of the form `useX` *force* the linker to use the
 *    corresponding features, guaranteeing that the specific semantics that
 *    they provide will be used.
 *  - Options whose name is of the form `allowX` *allow* the linker to use the
 *    corresponding features if it supports them. Support for such options can
 *    be dropped in any subsequent version of the linker, including patch
 *    versions.
 */
final class ESFeatures private (
    /** Whether to use ECMAScript 2015 features, such as classes and arrow
     *  functions.
     */
    val useECMAScript2015: Boolean,
    /** EXPERIMENTAL: Whether to allow using ECMAScript `BigInt`s to implement
     *  `Long`s.
     */
    val allowBigIntsForLongs: Boolean
) {
  import ESFeatures._

  private def this() = {
    this(
        useECMAScript2015 = false,
        allowBigIntsForLongs = false
    )
  }

  def withUseECMAScript2015(useECMAScript2015: Boolean): ESFeatures =
    copy(useECMAScript2015 = useECMAScript2015)

  def withAllowBigIntsForLongs(allowBigIntsForLongs: Boolean): ESFeatures =
    copy(allowBigIntsForLongs = allowBigIntsForLongs)

  override def equals(that: Any): Boolean = that match {
    case that: ESFeatures =>
      this.useECMAScript2015 == that.useECMAScript2015 &&
      this.allowBigIntsForLongs == that.allowBigIntsForLongs
    case _ =>
      false
  }

  override def hashCode(): Int = {
    import scala.util.hashing.MurmurHash3._
    var acc = HashSeed
    acc = mix(acc, useECMAScript2015.##)
    acc = mixLast(acc, allowBigIntsForLongs.##)
    finalizeHash(acc, 2)
  }

  override def toString(): String = {
    s"""ESFeatures(
       |  useECMAScript2015 = $useECMAScript2015,
       |  allowBigIntsForLongs = $allowBigIntsForLongs
       |)""".stripMargin
  }

  private def copy(
      useECMAScript2015: Boolean = this.useECMAScript2015,
      allowBigIntsForLongs: Boolean = this.allowBigIntsForLongs
  ): ESFeatures = {
    new ESFeatures(
        useECMAScript2015 = useECMAScript2015,
        allowBigIntsForLongs = allowBigIntsForLongs
    )
  }
}

object ESFeatures {
  private val HashSeed =
    scala.util.hashing.MurmurHash3.stringHash(classOf[ESFeatures].getName)

  /** Default configuration of ECMAScript features.
   *
   *  - `useECMAScript2015`: false
   *  - `allowBigIntsForLongs`: false
   */
  val Defaults: ESFeatures = new ESFeatures()
}
