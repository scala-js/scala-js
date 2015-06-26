package java.util

trait SortedMap[K, V] extends Map[K, V] {
  def firstKey(): K
  def comparator(): Comparator[_ >: K]
  def lastKey(): K
  def subMap(fromKey: K, toKey: K): SortedMap[K, V]
  def headMap(toKey: K): SortedMap[K, V]
  def tailMap(fromKey: K): SortedMap[K, V]
}
