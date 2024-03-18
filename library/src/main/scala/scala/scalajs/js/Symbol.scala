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
@JSGlobal
object Symbol extends js.Object {
  /** Creates a new unique symbol without description.
   *
   *  @group factories
   */
  def apply(): js.Symbol = js.native

  /** Creates a new unique symbol with the specified description.
   *
   *  @group factories
   */
  def apply(description: String): js.Symbol = js.native

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
  def forKey(key: String): js.Symbol = js.native

  /** Retrieves the key under which the specified symbol is registered in the
   *  global symbol registry, or `undefined` if it is not registered.
   *
   *  @group registry
   */
  def keyFor(sym: js.Symbol): js.UndefOr[String] = js.native

  /** <span class="badge badge-ecma2018" style="float: right;">ECMAScript 2018</span>
   *  The well-known symbol `@@asyncIterator`.
   *
   *  @group wellknownsyms
   */
  val asyncIterator: js.Symbol = js.native

  /** The well-known symbol `@@hasInstance`.
   *
   *  @group wellknownsyms
   */
  val hasInstance: js.Symbol = js.native

  /** The well-known symbol `@@isConcatSpreadable`.
   *
   *  @group wellknownsyms
   */
  val isConcatSpreadable: js.Symbol = js.native

  /** The well-known symbol `@@iterator`.
   *
   *  @group wellknownsyms
   */
  val iterator: js.Symbol = js.native

  /** The well-known symbol `@@match`.
   *
   *  @group wellknownsyms
   */
  val `match`: js.Symbol = js.native

  /** The well-known symbol `@@replace`.
   *
   *  @group wellknownsyms
   */
  val replace: js.Symbol = js.native

  /** The well-known symbol `@@search`.
   *
   *  @group wellknownsyms
   */
  val search: js.Symbol = js.native

  /** The well-known symbol `@@species`.
   *
   *  @group wellknownsyms
   */
  val species: js.Symbol = js.native

  /** The well-known symbol `@@split`.
   *
   *  @group wellknownsyms
   */
  val split: js.Symbol = js.native

  /** The well-known symbol `@@toPrimitive`.
   *
   *  @group wellknownsyms
   */
  val toPrimitive: js.Symbol = js.native

  /** The well-known symbol `@@toStringTag`.
   *
   *  @group wellknownsyms
   */
  val toStringTag: js.Symbol = js.native

  /** The well-known symbol `@@unscopables`.
   *
   *  @group wellknownsyms
   */
  val unscopables: js.Symbol = js.native
}
