package java.util

trait Map[K, V] {
  def size(): Int
  def isEmpty(): Boolean
  def containsKey(key: Any): Boolean
  def containsValue(value: Any): Boolean
  def get(key: Any): V
  def put(key: K, value: V): V
  def remove(key: Any): V
  def putAll(m: Map[_ <: K, _ <: V]): Unit
  def clear(): Unit
  def keySet(): Set[K]
  def values(): Collection[V]
  def entrySet(): Set[Map.Entry[K, V]]
  def equals(o: Any): Boolean
  def hashCode(): Int
}

object Map {

  trait Entry[K, V] {
    def getKey(): K
    def getValue(): V
    def setValue(value: V): V
    def equals(o: Any): Boolean
    def hashCode(): Int
  }

}
