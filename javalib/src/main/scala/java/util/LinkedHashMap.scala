package java.util

import scala.collection.mutable

class LinkedHashMap[K, V] private (inner: mutable.LinkedHashMap[Box[K], V],
    accessOrder: Boolean) extends HashMap[K, V](inner) {
  self =>

  def this() =
    this(mutable.LinkedHashMap.empty[Box[K], V], false)

  def this(initialCapacity: Int, loadFactor: Float, accessOrder: Boolean) = {
    this(mutable.LinkedHashMap.empty[Box[K], V], accessOrder)
    if (initialCapacity < 0)
      throw new IllegalArgumentException("initialCapacity < 0")
    else if (loadFactor < 0.0)
      throw new IllegalArgumentException("loadFactor <= 0.0")
  }

  def this(initialCapacity: Int, loadFactor: Float) =
    this(initialCapacity, loadFactor, false)

  def this(initialCapacity: Int) =
    this(initialCapacity, LinkedHashMap.DEFAULT_LOAD_FACTOR)

  def this(m: Map[_ <: K, _ <: V]) = {
    this()
    putAll(m)
  }

  override def get(key: scala.Any): V = {
    val value = super.get(key)
    if (accessOrder) {
      val boxedKey = Box(key.asInstanceOf[K])
      if (value != null || containsKey(boxedKey)) {
        inner.remove(boxedKey)
        inner(boxedKey) = value
      }
    }
    value
  }

  override def put(key: K, value: V): V = {
    val oldValue = {
      if (accessOrder) {
        val old = remove(key)
        super.put(key, value)
        old
      } else {
        super.put(key, value)
      }
    }
    val iter = entrySet().iterator()
    if (iter.hasNext && removeEldestEntry(iter.next()))
      iter.remove()
    oldValue
  }

  protected def removeEldestEntry(eldest: Map.Entry[K, V]): Boolean = false

  override def clone(): AnyRef = {
    new LinkedHashMap(inner.clone(), accessOrder)
  }
}

object LinkedHashMap {

  private[LinkedHashMap] final val DEFAULT_INITIAL_CAPACITY = 16
  private[LinkedHashMap] final val DEFAULT_LOAD_FACTOR = 0.75f

}
