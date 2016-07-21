package java.util.concurrent

import java.lang.{reflect => jlr}
import java.io.Serializable
import java.util._

import scala.collection.JavaConverters._

class ConcurrentHashMap[K >: Null, V >: Null]
    extends AbstractMap[K, V] with ConcurrentMap[K, V] with Serializable { self =>

  def this(initialCapacity: Int) =
    this()

  def this(initialCapacity: Int, loadFactor: Float) =
    this()

  def this(initialCapacity: Int, loadFactor: Float, concurrencyLevel: Int) =
    this()

  def this(initialMap: java.util.Map[_ <: K, _ <: V]) = {
    this()
    putAll(initialMap)
  }

  private val inner = new scala.collection.mutable.HashMap[Box[K], V]()

  override def isEmpty(): Boolean = inner.isEmpty

  override def size(): Int = inner.size

  override def get(key: Any): V =
    inner.get(Box(key.asInstanceOf[K])).getOrElse(null)

  override def containsKey(key: Any): Boolean = {
    if (key == null)
      throw new NullPointerException()
    inner.exists { case (ik, _) => key === ik() }
  }

  override def containsValue(value: Any): Boolean = {
    if (value == null)
      throw new NullPointerException()
    inner.exists { case (_, iv) => value === iv }
  }

  override def put(key: K, value: V): V = {
    if (key != null && value != null)
      inner.put(Box(key), value).getOrElse(null)
    else
      throw new NullPointerException()
  }

  def putIfAbsent(key: K, value: V): V = {
    if (key == null || value == null)
      throw new NullPointerException()

    val bKey = Box(key)
    inner.get(bKey).getOrElse {
      inner.put(bKey, value)
      null
    }
  }

  override def putAll(m: Map[_ <: K, _ <: V]): Unit = {
    for (e <- m.entrySet().asScala)
      put(e.getKey, e.getValue)
  }

  override def remove(key: Any): V =
    inner.remove(Box(key.asInstanceOf[K])).getOrElse(null)

  override def remove(key: Any, value: Any): Boolean = {
    val old = inner(Box(key.asInstanceOf[K]))
    if (value === old)
      inner.remove(Box(key.asInstanceOf[K])).isDefined
    else
      false
  }

  override def replace(key: K, oldValue: V, newValue: V): Boolean = {
    if (key != null && oldValue != null && newValue != null) {
      val old = inner(Box(key))
      if (oldValue === old) {
        put(key, newValue)
        true
      } else {
        false
      }
    } else {
      throw new NullPointerException()
    }
  }

  override def replace(key: K, value: V): V = {
    if (key != null && value != null) {
      if (inner(Box(key)) != null) put(key, value)
      else null
    } else {
      throw new NullPointerException()
    }
  }

  override def clear(): Unit =
    inner.clear()

  override def keySet(): ConcurrentHashMap.KeySetView[K, V] =
    new ConcurrentHashMap.KeySetView[K, V](this)

  def entrySet(): Set[Map.Entry[K, V]] = {
    new AbstractSet[Map.Entry[K, V]] {
      override def size(): Int = self.size

      def iterator(): Iterator[Map.Entry[K, V]] = {
        new Iterator[Map.Entry[K, V]] {
          private val keysCopy = inner.keysIterator

          private var lastKey: Box[K] = null

          def hasNext(): Boolean = keysCopy.hasNext

          def next(): Map.Entry[K, V] = {
            val k = keysCopy.next()
            val v = inner(k)
            lastKey = k
            new AbstractMap.SimpleImmutableEntry(k(), v)
          }

          def remove(): Unit = {
            if (lastKey != null) {
              inner.remove(lastKey)
              lastKey = null
            } else {
              throw new IllegalStateException()
            }
          }
        }
      }
    }
  }

  def keys(): Enumeration[K] =
    inner.keys.iterator.map(_.inner).asJavaEnumeration

  def elements(): Enumeration[V] =
    inner.values.iterator.asJavaEnumeration
}

object ConcurrentHashMap {

  class KeySetView[K >: Null, V >: Null] private[ConcurrentHashMap] (
      chm: ConcurrentHashMap[K, V]) extends Set[K] with Serializable {

    def size(): Int = chm.size

    def isEmpty(): Boolean = chm.isEmpty

    def contains(o: Any): Boolean = chm.containsKey(o)

    def iterator(): Iterator[K] = {
      new Iterator[K] {
        val iter = chm.entrySet.iterator()

        def hasNext(): Boolean = iter.hasNext()

        def next(): K = iter.next().getKey()

        def remove(): Unit = iter.remove()
      }
    }

    def toArray(): Array[AnyRef] =
      chm.keys().asInstanceOf[Enumeration[AnyRef]].asScala.toArray[AnyRef]

    def toArray[T <: AnyRef](a: Array[T]): Array[T] = {
      val toFill: Array[T] =
        if (a.size >= size) a
        else jlr.Array.newInstance(a.getClass.getComponentType, size).asInstanceOf[Array[T]]

      val iter = iterator
      for (i <- 0 until size)
        toFill(i) = iter.next().asInstanceOf[T]
      if (toFill.size > size)
        toFill(size) = null.asInstanceOf[T]
      toFill
    }

    def add(e: K): Boolean =
      throw new UnsupportedOperationException()

    def remove(o: Any): Boolean = chm.remove(o) != null

    def containsAll(c: Collection[_]): Boolean =
      c.asScala.forall(item => chm.asScala.contains(item.asInstanceOf[K]))

    def addAll(c: Collection[_ <: K]): Boolean =
      throw new UnsupportedOperationException()

    def removeAll(c: Collection[_]): Boolean =
      removeWhere(c.contains(_))

    def retainAll(c: Collection[_]): Boolean =
      removeWhere(!c.contains(_))

    def clear(): Unit = chm.clear()

    private def removeWhere(p: Any => Boolean): Boolean = {
      val iter = chm.entrySet.iterator
      var changed = false
      while (iter.hasNext) {
        if (p(iter.next().getKey())) {
          iter.remove()
          changed = true
        }
      }
      changed
    }
  }

}
