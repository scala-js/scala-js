/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2017, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.js

import scala.scalajs.js
import scala.scalajs.js.annotation._

/** <span class="badge badge-ecma6" style="float: right;">ECMAScript 6</span>
 *  JavaScript Symbol.
 */
@js.native
sealed trait Symbol extends js.Any

/** <span class="badge badge-ecma6" style="float: right;">ECMAScript 6</span>
 *  Factory for [[Symbol js.Symbol]]s and well-known symbols.
 *
 *  @groupname factories Factories for unique symbols
 *  @groupprio 10
 *
 *  @groupname registry Global symbol registry
 *  @groupprio 20
 *
 *  @groupname wellknownsyms Well-known symbols
 *  @groupprio 30
 */
@js.native
object Symbol extends js.Object {
  /** Creates a new unique symbol without description.
   *
   *  @group factories
   */
  def apply(): Symbol = js.native

  /** Creates a new unique symbol with the specified description.
   *
   *  @group factories
   */
  def apply(description: String): Symbol = js.native

  /** Retrieves the symbol with the specified key in the global symbol registry.
   *
   *  The returned symbol's description is also the key.
   *
   *  Asking twice `forKey` with the same key returns the same symbol,
   *  globally.
   *
   *  @group registry
   */
  @JSName("for")
  def forKey(key: String): Symbol = js.native

  /** Retrieves the key under which the specified symbol is registered in the
   *  global symbol registry, or `undefined` if it is not registered.
   *
   *  @group registry
   */
  def keyFor(sym: Symbol): js.UndefOr[String] = js.native

  /** The well-known symbol `@@hasInstance`.
   *
   *  @group wellknownsyms
   */
  val hasInstance: Symbol = js.native

  /** The well-known symbol `@@isConcatSpreadable`.
   *
   *  @group wellknownsyms
   */
  val isConcatSpreadable: Symbol = js.native

  /** The well-known symbol `@@iterator`.
   *
   *  @group wellknownsyms
   */
  val iterator: Symbol = js.native

  /** The well-known symbol `@@match`.
   *
   *  @group wellknownsyms
   */
  val `match`: Symbol = js.native

  /** The well-known symbol `@@replace`.
   *
   *  @group wellknownsyms
   */
  val replace: Symbol = js.native

  /** The well-known symbol `@@search`.
   *
   *  @group wellknownsyms
   */
  val search: Symbol = js.native

  /** The well-known symbol `@@species`.
   *
   *  @group wellknownsyms
   */
  val species: Symbol = js.native

  /** The well-known symbol `@@split`.
   *
   *  @group wellknownsyms
   */
  val split: Symbol = js.native

  /** The well-known symbol `@@toPrimitive`.
   *
   *  @group wellknownsyms
   */
  val toPrimitive: Symbol = js.native

  /** The well-known symbol `@@toStringTag`.
   *
   *  @group wellknownsyms
   */
  val toStringTag: Symbol = js.native

  /** The well-known symbol `@@unscopables`.
   *
   *  @group wellknownsyms
   */
  val unscopables: Symbol = js.native
}
