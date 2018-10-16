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

import scala.collection.mutable
import scala.collection.mutable.Builder
import scala.collection.View

import scala.scalajs.js
import scala.scalajs.js.annotation._

/** Wrapper to use a js.Dictionary as a scala.mutable.Map */
@inline
class WrappedDictionary[A](val dict: js.Dictionary[A])
    extends mutable.AbstractMap[String, A]
    with mutable.MapOps[String, A, mutable.Map, js.WrappedDictionary[A]] {

  import WrappedDictionary._

  protected[this] override def fromSpecificIterable(
      coll: scala.collection.Iterable[(String, A)]
  ): js.WrappedDictionary[A] = {
    val d = js.WrappedDictionary.empty[A]
    d ++= coll
    d
  }

  protected[this] override def newSpecificBuilder: Builder[(String, A), js.WrappedDictionary[A]] =
    new WrappedDictionaryBuilder[A]

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

  @inline
  private def rawApply(key: String): A =
    dict.asInstanceOf[DictionaryRawApply[A]].rawApply(key)

  override def contains(key: String): Boolean = {
    /* We have to use a safe version of hasOwnProperty, because
     * "hasOwnProperty" could be a key of this dictionary.
     */
    safeHasOwnProperty(dict, key)
  }

  def subtractOne(key: String): this.type = {
    if (contains(key))
      js.special.delete(dict, key)
    this
  }

  override def update(key: String, value: A): Unit =
    dict.asInstanceOf[DictionaryRawApply[A]].rawUpdate(key, value)

  def addOne(kv: (String, A)): this.type = {
    dict(kv._1) = kv._2
    this
  }

  def clear(): Unit =
    keysIterator.foreach(subtractOne)

  def iterator: scala.collection.Iterator[(String, A)] =
    new DictionaryIterator(dict)

  @inline
  override def keys: scala.collection.Iterable[String] =
    js.Object.keys(dict.asInstanceOf[js.Object])

  override def empty: js.WrappedDictionary[A] =
    new js.WrappedDictionary(js.Dictionary.empty)

  // Overloads to return more precise types

  def map[B](f: ((String, A)) => (String, B)): js.WrappedDictionary[B] = {
    val b = new js.WrappedDictionary.WrappedDictionaryBuilder[B]
    val it = this.iterator
    while (it.hasNext) {
      b += f(it.next())
    }
    b.result()
  }

  def flatMap[B](
      f: ((String, A)) => IterableOnce[(String, B)]): js.WrappedDictionary[B] = {
    val b = new js.WrappedDictionary.WrappedDictionaryBuilder[B]
    val it = this.iterator
    while (it.hasNext) {
      b ++= f(it.next())
    }
    b.result()
  }

  def collect[B](
      pf: PartialFunction[(String, A), (String, B)]): js.WrappedDictionary[B] = {
    flatMap { a =>
      if (pf.isDefinedAt(a)) new View.Single(pf(a))
      else View.Empty
    }
  }

}

object WrappedDictionary {
  /* Note: We can't extend MapFactory[WrappedDictionary] since it requires
   * support for any type of key.
   */

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

  private final class DictionaryIterator[+A](dict: js.Dictionary[A])
      extends scala.collection.Iterator[(String, A)] {

    private[this] val keys = js.Object.keys(dict.asInstanceOf[js.Object])
    private[this] var index: Int = 0

    def hasNext(): Boolean = index < keys.length

    def next(): (String, A) = {
      val key = keys(index)
      index += 1
      (key, dict(key))
    }
  }

  def empty[A]: js.WrappedDictionary[A] =
    new js.WrappedDictionary(js.Dictionary.empty)

  private final class WrappedDictionaryBuilder[A]
      extends Builder[(String, A), js.WrappedDictionary[A]] {

    private[this] var dict: js.Dictionary[A] = js.Dictionary.empty

    def addOne(elem: (String, A)): this.type = {
      dict(elem._1) = elem._2
      this
    }

    def clear(): Unit =
      dict = js.Dictionary.empty

    def result(): js.WrappedDictionary[A] =
      new js.WrappedDictionary(dict)
  }

}
