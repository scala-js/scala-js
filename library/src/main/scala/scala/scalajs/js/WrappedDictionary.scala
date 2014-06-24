/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-lang.org/     **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */

package scala.scalajs.js

import scala.collection.mutable
import mutable.Builder

import scala.collection.generic.CanBuildFrom

/** Wrapper to use a js.Dictionary as a scala.mutable.Map */
class WrappedDictionary[A](val dict: Dictionary[A])
    extends mutable.AbstractMap[String, A]
       with mutable.Map[String, A]
       with mutable.MapLike[String, A, WrappedDictionary[A]] {

  def get(key: String): Option[A] = {
    if (dict.hasOwnProperty(key))
      Some(dict(key))
    else
      None
  }

  def -=(key: String): this.type = {
    dict.delete(key)
    this
  }

  def +=(kv: (String, A)): this.type = {
    dict(kv._1) = kv._2
    this
  }

  def iterator: Iterator[(String, A)] = new Iterator[(String, A)] {
    private[this] val keys = Object.keys(dict)
    private[this] var index: Int = 0
    def hasNext(): Boolean = index < keys.length
    def next(): (String, A) = {
      val key = keys(index)
      index += 1
      (key, dict(key))
    }
  }

  override def empty: WrappedDictionary[A] =
    new WrappedDictionary(Dictionary.empty)

}

object WrappedDictionary {
  // Note: We can't extend MutableMapFactory[WrappedDictionary] since
  // it requires support for any type of key

  def empty[A]: WrappedDictionary[A] = new WrappedDictionary(Dictionary.empty)

  type CBF[A] = CanBuildFrom[WrappedDictionary[_], (String, A), WrappedDictionary[A]]
  implicit def canBuildFrom[A]: CBF[A] = new CBF[A] {
    def apply(from: WrappedDictionary[_]): Builder[(String, A), WrappedDictionary[A]] =
      new WrappedDictionaryBuilder[A]
    def apply(): Builder[(String, A), WrappedDictionary[A]] =
      new WrappedDictionaryBuilder[A]
  }

  class WrappedDictionaryBuilder[A]
      extends Builder[(String, A), WrappedDictionary[A]] {
    private[this] var dict: Dictionary[A] = Dictionary.empty
    def +=(elem: (String, A)): this.type = {
      dict(elem._1) = elem._2
      this
    }
    def clear(): Unit =
      dict = Dictionary.empty
    def result(): WrappedDictionary[A] =
      new WrappedDictionary(dict)
  }

}
