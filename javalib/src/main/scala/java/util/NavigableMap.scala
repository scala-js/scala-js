package java.util

trait NavigableMap[K, V] extends SortedMap[K, V] {
  def lowerEntry(key: K): Map.Entry[K, V]
  def lowerKey(key: K): K
  def floorEntry(key: K): Map.Entry[K, V]
  def floorKey(key: K): K
  def ceilingEntry(key: K): Map.Entry[K, V]
  def ceilingKey(key: K): K
  def higherEntry(key: K): Map.Entry[K, V]
  def higherKey(key: K): K
  def firstEntry(): Map.Entry[K, V]
  def lastEntry(): Map.Entry[K, V]
  def pollFirstEntry(): Map.Entry[K, V]
  def pollLastEntry(): Map.Entry[K, V]
  def descendingMap(): NavigableMap[K, V]
  def navigableKeySet(): NavigableSet[K]
  def descendingKeySet(): NavigableSet[K]
  def subMap(fromKey: K, fromInclusive: Boolean, toKey: K, toInclusive: Boolean): NavigableMap[K, V]
  def headMap(toKey: K, inclusive: Boolean): NavigableMap[K, V]
  def tailMap(toKey: K, inclusive: Boolean): NavigableMap[K, V]
  def subMap(fromKey: K, toKey: K): SortedMap[K, V]
  def headMap(toKey: K): SortedMap[K, V]
  def tailMap(fromKey: K): SortedMap[K, V]
}
