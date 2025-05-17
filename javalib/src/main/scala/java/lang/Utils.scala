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

package java.lang

import scala.language.implicitConversions

import java.util.function._

import scala.scalajs.js
import scala.scalajs.js.annotation._

private[java] object Utils {
  @inline
  def undefined: js.UndefOr[Nothing] = ().asInstanceOf[js.UndefOr[Nothing]]

  @inline
  def isUndefined(x: Any): scala.Boolean =
    x.asInstanceOf[AnyRef] eq ().asInstanceOf[AnyRef]

  @inline
  def undefOrIsDefined[A](x: js.UndefOr[A]): scala.Boolean =
    x ne ().asInstanceOf[AnyRef]

  @inline
  def undefOrForceGet[A](x: js.UndefOr[A]): A =
    x.asInstanceOf[A]

  @inline
  def undefOrGetOrElse[A](x: js.UndefOr[A])(default: Supplier[A]): A =
    if (undefOrIsDefined(x)) undefOrForceGet(x)
    else default.get()

  @inline
  def undefOrGetOrNull[A >: Null](x: js.UndefOr[A]): A =
    if (undefOrIsDefined(x)) undefOrForceGet(x)
    else null

  @inline
  def undefOrForeach[A](x: js.UndefOr[A])(f: Consumer[A]): Unit = {
    if (undefOrIsDefined(x))
      f.accept(undefOrForceGet(x))
  }

  @inline
  def undefOrFold[A, B](x: js.UndefOr[A])(default: Supplier[B])(f: Function[A, B]): B =
    if (undefOrIsDefined(x)) f(undefOrForceGet(x))
    else default.get()

  private object Cache {
    val safeHasOwnProperty =
      js.Dynamic.global.Object.prototype.hasOwnProperty
        .asInstanceOf[js.ThisFunction1[js.Dictionary[_], String, scala.Boolean]]
  }

  @inline
  private def safeHasOwnProperty(dict: js.Dictionary[_], key: String): scala.Boolean =
    Cache.safeHasOwnProperty(dict, key)

  @js.native
  private trait DictionaryRawApply[A] extends js.Object {
    /** Reads a field of this object by its name.
     *
     *  This must not be called if the dictionary does not contain the key.
     */
    @JSBracketAccess
    def rawApply(key: String): A = js.native

    /** Writes a field of this object. */
    @JSBracketAccess
    def rawUpdate(key: String, value: A): Unit = js.native
  }

  @inline
  def dictEmpty[A](): js.Dictionary[A] =
    new js.Object().asInstanceOf[js.Dictionary[A]]

  @inline
  def dictGetOrElse[A](dict: js.Dictionary[_ <: A], key: String)(
      default: Supplier[A]): A = {
    if (dictContains(dict, key))
      dictRawApply(dict, key)
    else
      default.get()
  }

  def dictGetOrElseAndRemove[A](dict: js.Dictionary[_ <: A], key: String,
      default: A): A = {
    if (dictContains(dict, key)) {
      val result = dictRawApply(dict, key)
      js.special.delete(dict, key)
      result
    } else {
      default
    }
  }

  @inline
  def dictRawApply[A](dict: js.Dictionary[A], key: String): A =
    dict.asInstanceOf[DictionaryRawApply[A]].rawApply(key)

  def dictContains[A](dict: js.Dictionary[A], key: String): scala.Boolean = {
    /* We have to use a safe version of hasOwnProperty, because
     * "hasOwnProperty" could be a key of this dictionary.
     */
    safeHasOwnProperty(dict, key)
  }

  @inline
  def dictSet[A](dict: js.Dictionary[A], key: String, value: A): Unit =
    dict.asInstanceOf[DictionaryRawApply[A]].rawUpdate(key, value)

  @js.native
  private trait MapRaw[K, V] extends js.Object {
    def has(key: K): scala.Boolean = js.native
    def get(key: K): V = js.native
    @JSName("get") def getOrUndefined(key: K): js.UndefOr[V] = js.native
    def set(key: K, value: V): Unit = js.native
    def keys(): js.Iterator[K] = js.native
  }

  @inline
  def mapHas[K, V](map: js.Map[K, V], key: K): scala.Boolean =
    map.asInstanceOf[MapRaw[K, V]].has(key)

  @inline
  def mapGet[K, V](map: js.Map[K, V], key: K): V =
    map.asInstanceOf[MapRaw[K, V]].get(key)

  @inline
  def mapSet[K, V](map: js.Map[K, V], key: K, value: V): Unit =
    map.asInstanceOf[MapRaw[K, V]].set(key, value)

  @inline
  def mapGetOrElse[K, V](map: js.Map[K, V], key: K)(default: Supplier[V]): V = {
    val value = map.asInstanceOf[MapRaw[K, V]].getOrUndefined(key)
    if (!isUndefined(value) || mapHas(map, key)) value.asInstanceOf[V]
    else default.get()
  }

  @inline
  def mapGetOrElseUpdate[K, V](map: js.Map[K, V], key: K)(default: Supplier[V]): V = {
    mapGetOrElse(map, key) { () =>
      val value = default.get()
      mapSet(map, key, value)
      value
    }
  }

  @inline
  def forArrayElems[A](array: js.Array[A])(f: Consumer[A]): Unit = {
    val len = array.length
    var i = 0
    while (i != len) {
      f.accept(array(i))
      i += 1
    }
  }

  @inline
  def arrayRemove[A](array: js.Array[A], index: Int): Unit =
    array.splice(index, 1)

  @inline
  def arrayRemoveAndGet[A](array: js.Array[A], index: Int): A =
    array.splice(index, 1)(0)

  @inline
  def arrayExists[A](array: js.Array[A])(f: Predicate[A]): scala.Boolean = {
    // scalastyle:off return
    val len = array.length
    var i = 0
    while (i != len) {
      if (f.test(array(i)))
        return true
      i += 1
    }
    false
    // scalastyle:on return
  }

  @inline def toUint(x: scala.Double): scala.Double = {
    import js.DynamicImplicits.number2dynamic
    (x >>> 0).asInstanceOf[scala.Double]
  }

  /** Round up to a power of 2; if overflow, returns the given number. */
  @inline def roundUpToPowerOfTwo(i: Int): Int =
    if (i > (1 << 30)) i
    else ((1 << 31) >>> (Integer.numberOfLeadingZeros(i - 1)) - 1)
}
