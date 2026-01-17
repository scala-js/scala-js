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

import scala.language.implicitConversions

import scala.scalajs.js
import scala.scalajs.js.annotation._

import scala.collection.mutable
import mutable.Builder

import scala.collection.generic.CanBuildFrom

/** Wrapper to use a js.Dictionary as a scala.mutable.Map */
@inline
final class WrappedDictionary[A](private val dict: js.Dictionary[A])
    extends mutable.AbstractMap[String, A] with mutable.Map[String, A]
    with mutable.MapLike[String, A, js.WrappedDictionary[A]] {

  import WrappedDictionary._

  def get(key: String): Option[A] = {
    if (contains(key))
      Some(rawApply(key))
    else
      None
  }

  override def apply(key: String): A = {
    if (contains(key))
      rawApply(key)
    else
      throw new NoSuchElementException("key not found: " + key)
  }

  override def getOrElse[V1 >: A](key: String, default: => V1): V1 = {
    if (contains(key))
      rawApply(key)
    else
      default
  }

  override def getOrElseUpdate(key: String, op: => A): A = {
    if (contains(key)) {
      rawApply(key)
    } else {
      val v = op
      update(key, v)
      v
    }
  }

  @inline
  private def rawApply(key: String): A =
    dict.asInstanceOf[DictionaryRawApply[A]].rawApply(key)

  override def contains(key: String): Boolean = {
    /* We have to use a safe version of hasOwnProperty, because
     * "hasOwnProperty" could be a key of this dictionary.
     */
    safeHasOwnProperty(dict, key)
  }

  def -=(key: String): this.type = {
    if (contains(key))
      js.special.delete(dict, key)
    this
  }

  override def update(key: String, value: A): Unit =
    dict.asInstanceOf[DictionaryRawApply[A]].rawUpdate(key, value)

  def +=(kv: (String, A)): this.type = {
    dict(kv._1) = kv._2
    this
  }

  def iterator: scala.collection.Iterator[(String, A)] =
    new DictionaryIterator(dict)

  @inline
  override def keys: scala.collection.Iterable[String] =
    js.Object.keys(dict.asInstanceOf[js.Object])

  override def empty: js.WrappedDictionary[A] =
    new js.WrappedDictionary(Dictionary.empty)

}

object WrappedDictionary {
  // Note: We can't extend MutableMapFactory[WrappedDictionary] since
  // it requires support for any type of key

  private object Cache {
    val safeHasOwnProperty = {
      js.Dynamic.global.Object.prototype.hasOwnProperty
        .asInstanceOf[js.ThisFunction1[js.Dictionary[_], String, Boolean]]
    }
  }

  @inline
  private def safeHasOwnProperty(dict: js.Dictionary[_], key: String): Boolean =
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

  private final class DictionaryIterator[+A](
      dict: js.Dictionary[A])
      extends scala.collection.Iterator[(String, A)] {

    private[this] val keys = js.Object.keys(dict.asInstanceOf[js.Object])
    private[this] var index: Int = 0

    def hasNext: Boolean = index < keys.length

    def next(): (String, A) = {
      val key = keys(index)
      index += 1
      (key, dict(key))
    }
  }

  def empty[A]: js.WrappedDictionary[A] =
    new js.WrappedDictionary(js.Dictionary.empty)

  implicit def canBuildFrom[
      A]: CanBuildFrom[js.WrappedDictionary[_], (String, A), js.WrappedDictionary[A]] = {
    new CanBuildFrom[js.WrappedDictionary[_], (String, A), js.WrappedDictionary[A]] {
      def apply(from: js.WrappedDictionary[_]): Builder[(String, A), js.WrappedDictionary[A]] =
        new WrappedDictionaryBuilder[A]
      def apply(): Builder[(String, A), js.WrappedDictionary[A]] =
        new WrappedDictionaryBuilder[A]
    }
  }

  private final class WrappedDictionaryBuilder[A]
      extends Builder[(String, A), js.WrappedDictionary[A]] {

    private[this] var dict: js.Dictionary[A] = js.Dictionary.empty

    def +=(elem: (String, A)): this.type = {
      dict(elem._1) = elem._2
      this
    }

    def clear(): Unit =
      dict = js.Dictionary.empty

    def result(): js.WrappedDictionary[A] =
      new js.WrappedDictionary(dict)
  }

  implicit def toJSDictionary[A](wrappedDict: js.WrappedDictionary[A]): js.Dictionary[A] =
    wrappedDict.dict

}
