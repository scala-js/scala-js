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

package java.util

import scala.language.implicitConversions

import scala.scalajs.js
import scala.scalajs.js.annotation.JSBracketAccess

private[java] object JSUtils {
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
  def undefOrGetOrElse[A](x: js.UndefOr[A])(default: => A): A =
    if (undefOrIsDefined(x)) x.asInstanceOf[A]
    else default

  @inline
  def undefOrGetOrNull[A >: Null](x: js.UndefOr[A]): A =
    if (undefOrIsDefined(x)) x.asInstanceOf[A]
    else null

  @inline
  def undefOrForeach[A](x: js.UndefOr[A])(f: A => Any): Unit = {
    if (undefOrIsDefined(x))
      f(undefOrForceGet(x))
  }

  @inline
  def undefOrFold[A, B](x: js.UndefOr[A])(default: => B)(f: A => B): B =
    if (undefOrIsDefined(x)) f(undefOrForceGet(x))
    else default

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
      default: => A): A = {
    if (dictContains(dict, key))
      dictRawApply(dict, key)
    else
      default
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

  @inline
  def forArrayElems[A](array: js.Array[A])(f: A => Any): Unit = {
    val len = array.length
    var i = 0
    while (i != len) {
      f(array(i))
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
  def arrayExists[A](array: js.Array[A])(f: A => Boolean): Boolean = {
    // scalastyle:off return
    val len = array.length
    var i = 0
    while (i != len) {
      if (f(array(i)))
        return true
      i += 1
    }
    false
    // scalastyle:on return
  }

  @js.native
  private trait RawMap[K, V] extends js.Object {
    def has(key: K): Boolean = js.native
    def keys(): js.Iterator[K] = js.native
    def set(key: K, value: V): js.Map[K, V] = js.native
    def get(key: K): V = js.native
  }

  @inline def mapHas[K, V](m: js.Map[K, V], key: K): Boolean =
    m.asInstanceOf[RawMap[K, V]].has(key)

  @inline def mapGet[K, V](m: js.Map[K, V], key: K): V =
    m.asInstanceOf[RawMap[K, V]].get(key)

  @inline def mapSet[K, V](m: js.Map[K, V], key: K, value: V): Unit =
    m.asInstanceOf[RawMap[K, V]].set(key, value)

  @inline def mapGetOrElse[K, V](m: js.Map[K, V], key: K)(default: => V): V =
    if (mapHas(m, key)) mapGet(m, key)
    else default

  @inline def mapGetOrElseUpdate[K, V](m: js.Map[K, V], key: K)(default: => V): V = {
    if (mapHas(m, key)) {
      mapGet(m, key)
    } else {
      val value = default
      mapSet(m, key, value)
      value
    }
  }
}
