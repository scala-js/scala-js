/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.js

import scala.scalajs.js
import js.annotation._

/** <span class="badge badge-ecma6" style="float: right;">ECMAScript 6</span>
 *  JavaScript Symbol.
 */
@js.native
sealed trait Symbol extends js.Any

object Symbol {
  @JSName("Symbol")
  @js.native
  private object NativeSymbol extends js.Object {
    def apply(): Symbol = js.native
    def apply(description: String): Symbol = js.native

    def `for`(key: String): Symbol = js.native
    def keyFor(sym: Symbol): String = js.native

    val iterator: Symbol = js.native
  }

  def apply(): Symbol = NativeSymbol()
  def apply(description: String): Symbol = NativeSymbol()

  def apply(sym: scala.Symbol): Symbol = forKey(sym.name)

  def forKey(key: String): Symbol = NativeSymbol.`for`(key)
  def keyFor(sym: Symbol): String = NativeSymbol.keyFor(sym)

  val iterator: Symbol = NativeSymbol.iterator
}
