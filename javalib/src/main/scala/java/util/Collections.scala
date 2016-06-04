package java.util

import java.{lang => jl}
import java.io.Serializable

import scala.language.implicitConversions

import scala.annotation.tailrec

import scala.collection.JavaConverters._

object Collections {

  final lazy val EMPTY_SET: Set[_] = {
    new ImmutableSet(
      new AbstractSet[Any] with Serializable {
        override def size(): Int = 0

        override def iterator(): Iterator[Any] = emptyIterator[Any]
      })
  }

  final lazy val EMPTY_LIST: List[_] = {
    new ImmutableList(
      new AbstractList[Any] with Serializable with RandomAccess {
        override def get(index: Int): Any =
          throw new IndexOutOfBoundsException(index.toString)

        override def size(): Int = 0
      })
  }

  final lazy val EMPTY_MAP: Map[_, _] = {
    new ImmutableMap(
      new AbstractMap[Any, Any] with Serializable {
        override def entrySet(): Set[Map.Entry[Any, Any]] =
          EMPTY_SET.asInstanceOf[Set[Map.Entry[Any, Any]]]
      })
  }

  private lazy val EMPTY_ITERATOR: Iterator[_] =
    new EmptyIterator

  private lazy val EMPTY_LIST_ITERATOR: ListIterator[_] =
    new EmptyListIterator

  private lazy val EMPTY_ENUMERATION: Enumeration[_] = {
    new Enumeration[Any] {
      def hasMoreElements: Boolean = false

      def nextElement(): Any =
        throw new NoSuchElementException
    }
  }

  // Differs from original type definition, original: [T <: jl.Comparable[_ >: T]]
  def sort[T <: jl.Comparable[T]](list: List[T]): Unit =
    sort(list, naturalComparator[T])

  def sort[T](list: List[T], c: Comparator[_ >: T]): Unit = {
    val sortedList = list.asScala.sorted(c).asJava
    list match {
      case list: RandomAccess => copyImpl(sortedList.iterator, list)
      case _                  => copyImpl(sortedList.iterator, list.listIterator)
    }
  }

  def binarySearch[T](list: List[_ <: jl.Comparable[_ >: T]], key: T): Int =
    binarySearchImpl(list, (elem: Comparable[_ >: T]) => elem.compareTo(key))

  def binarySearch[T](list: List[_ <: T], key: T, c: Comparator[_ >: T]): Int =
    binarySearchImpl(list, (elem: T) => c.compare(elem, key))

  @inline
  private def binarySearchImpl[E](list: List[E], compareToKey: E => Int): Int = {
    def notFound(insertionPoint: Int): Int = {
      -insertionPoint - 1
    }

    @tailrec
    def binarySearch(lo: Int, hi: Int, get: Int => E): Int = {
      if (lo < hi) {
        val mid = lo + (hi - lo) / 2
        val cmp = compareToKey(get(mid))
        if (cmp == 0) mid
        else if (cmp > 0) binarySearch(lo, mid, get)
        else binarySearch(mid + 1, hi, get)
      } else {
        notFound(lo)
      }
    }

    list match {
      case _: RandomAccess =>
        binarySearch(0, list.size, list.get(_))

      case _ =>
        def getFrom(iter: ListIterator[E])(index: Int): E = {
          val shift = index - iter.nextIndex
          if (shift > 0)
            (0 until shift).foreach(_ => iter.next())
          else
            (0 until -shift).foreach(_ => iter.previous())
          iter.next()
        }
        binarySearch(0, list.size, getFrom(list.listIterator))
    }
  }

  def reverse(list: List[_]): Unit =
    reverseImpl(list)

  @inline
  def reverseImpl[T](list: List[T]): Unit = {
    val size = list.size
    list match {
      case list: RandomAccess =>
        for (i <- 0 until size / 2) {
          val tmp = list.get(i)
          list.set(i, list.get(size - i - 1))
          list.set(size - i - 1, tmp)
        }

      case _ =>
        val it1 = list.listIterator()
        val it2 = list.listIterator(size)
        for (i <- 0 until size / 2) {
          val tmp = it1.next()
          it1.set(it2.previous())
          it2.set(tmp)
        }
    }
  }

  def shuffle(list: List[_]): Unit =
    shuffle(list, new Random)

  def shuffle(list: List[_], rnd: Random): Unit =
    shuffleImpl(list, rnd)

  @inline
  def shuffleImpl[T](list: List[T], rnd: Random): Unit = {
    val scalaRnd = scala.util.Random.javaRandomToRandom(rnd)
    val shuffledList = scalaRnd.shuffle(list.asScala.toSeq).asJava
    list match {
      case list: RandomAccess => copyImpl(shuffledList.iterator, list)
      case _                  => copyImpl(shuffledList.iterator, list.listIterator)
    }
  }

  def swap(list: List[_], i: Int, j: Int): Unit =
    swapImpl(list, i, j)

  @inline
  private def swapImpl[E](list: List[E], i: Int, j: Int): Unit = {
    list match {
      case list: RandomAccess =>
        val tmp = list.get(i)
        list.set(i, list.get(j))
        list.set(j, tmp)

      case _ =>
        val it1 = list.listIterator(i)
        val it2 = list.listIterator(j)
        if (!it1.hasNext || !it2.hasNext)
          throw new IndexOutOfBoundsException
        val tmp = it1.next()
        it1.set(it2.next())
        it2.set(tmp)
    }
  }

  def fill[T](list: List[_ >: T], obj: T): Unit = {
    list match {
      case list: RandomAccess =>
        (0 until list.size).foreach(list.set(_, obj))

      case _ =>
        val iter = list.listIterator
        while (iter.hasNext) {
          iter.next()
          iter.set(obj)
        }
    }
  }

  def copy[T](dest: List[_ >: T], src: List[_ <: T]): Unit = {
    (dest, src) match {
      case (dest: RandomAccess, src: RandomAccess) => copyImpl(src, dest)
      case (dest: RandomAccess, _)                 => copyImpl(src.iterator, dest)
      case (_, src: RandomAccess)                  => copyImpl(src, dest.listIterator)
      case (_, _)                                  => copyImpl(src.iterator, dest.listIterator)
    }
  }

  private def copyImpl[T](source: List[_ <: T] with RandomAccess,
      dest: List[T] with RandomAccess): Unit = {
    (0 until source.size).foreach(i => dest.set(i, source.get(i)))
  }

  private def copyImpl[T](source: Iterator[_ <: T], dest: List[T] with RandomAccess): Unit = {
    val destEnd = dest.size()
    var i = 0
    while (source.hasNext) {
      if (i < destEnd)
        dest.set(i, source.next())
      else
        throw new IndexOutOfBoundsException
      i += 1
    }
  }

  private def copyImpl[T](source: List[_ <: T] with RandomAccess, dest: ListIterator[T]): Unit = {
    for (i <- 0 until source.size) {
      if (dest.hasNext) {
        dest.next()
        dest.set(source.get(i))
      } else {
        throw new IndexOutOfBoundsException
      }
    }
  }

  private def copyImpl[T](source: Iterator[_ <: T], dest: ListIterator[T]): Unit = {
    while (source.hasNext) {
      if (dest.hasNext) {
        dest.next()
        dest.set(source.next())
      } else {
        throw new IndexOutOfBoundsException
      }
    }
  }

  // Differs from original type definition, original: [T <: jl.Comparable[_ >: T]]
  def min[T <: AnyRef with jl.Comparable[T]](coll: Collection[_ <: T]): T =
    min(coll, naturalComparator[T])

  def min[T](coll: Collection[_ <: T], comp: Comparator[_ >: T]): T =
    coll.asScala.min(comp)

  // Differs from original type definition, original: [T <: jl.Comparable[_ >: T]]
  def max[T <: AnyRef with jl.Comparable[T]](coll: Collection[_ <: T]): T =
    max(coll, naturalComparator[T])

  def max[T](coll: Collection[_ <: T], comp: Comparator[_ >: T]): T =
    coll.asScala.max(comp)

  def rotate(list: List[_], distance: Int): Unit =
    rotateImpl(list, distance)

  private def rotateImpl[T](list: List[T], distance: Int): Unit = {
    val listSize = list.size
    if (listSize > 1 && distance % listSize != 0) {
      def exchangeRotation(): Unit = {
        def indexModulo(i: Int): Int = modulo(i, listSize)

        @tailrec
        def rotateNext(cycleStartIndex: Int, count: Int, index: Int, value: T): Unit = {
          val nextValue = list.get(index)
          val newCount = count + 1
          list.set(index, value)
          if (index != cycleStartIndex) {
            rotateNext(cycleStartIndex, newCount, indexModulo(index + distance), nextValue)
          } else if (newCount < listSize) {
            val nextCycleStart = cycleStartIndex + 1
            rotateNext(nextCycleStart, newCount, indexModulo(nextCycleStart + distance),
                list.get(nextCycleStart))
          }
        }
        rotateNext(0, 0, indexModulo(distance), list.get(0))
      }

      def splitReverseRotation(): Unit = {
        val splitPoint = modulo(-distance, listSize)
        reverse(list.subList(0, splitPoint))
        reverse(list.subList(splitPoint, listSize))
        reverse(list)
      }

      list match {
        case _: RandomAccess    => exchangeRotation()
        case _ if listSize < 16 => exchangeRotation() // TODO benchmark and set proper limit
        case _                  => splitReverseRotation()
      }
    }
  }

  def replaceAll[T](list: List[T], oldVal: T, newVal: T): Boolean = {
    list match {
      case _: RandomAccess =>
        var modified = false
        for (i <- 0 until list.size) {
          if (list.get(i) === oldVal) {
            list.set(i, newVal)
            modified = true
          }
        }
        modified

      case _ =>
        @tailrec
        def replaceAll(iter: ListIterator[T], mod: Boolean): Boolean = {
          if (iter.hasNext) {
            val isEqual = iter.next() === oldVal
            if (isEqual)
              iter.set(newVal)
            replaceAll(iter, mod || isEqual)
          } else {
            mod
          }
        }
        replaceAll(list.listIterator(), false)
    }
  }

  def indexOfSubList(source: List[_], target: List[_]): Int =
    indexOfSubListImpl(source, target, fromStart = true)

  def lastIndexOfSubList(source: List[_], target: List[_]): Int =
    indexOfSubListImpl(source, target, fromStart = false)

  @inline
  private def indexOfSubListImpl(source: List[_], target: List[_],
      fromStart: Boolean): Int = {
    val targetSize = target.size
    if (targetSize == 0) {
      if (fromStart) 0
      else source.size
    } else {
      val indices = 0 to source.size - targetSize
      val indicesInOrder = if (fromStart) indices else indices.reverse
      indicesInOrder.find { i =>
        source.subList(i, i + target.size).equals(target)
      }.getOrElse(-1)
    }
  }

  def unmodifiableCollection[T](c: Collection[_ <: T]): Collection[T] =
    new UnmodifiableCollection[T, Collection[T]](c.asInstanceOf[Collection[T]])

  def unmodifiableSet[T](a: Set[_ <: T]): Set[T] =
    new UnmodifiableSet[T, Set[T]](a.asInstanceOf[Set[T]])

  def unmodifiableSortedSet[T](s: SortedSet[T]): SortedSet[T] =
    new UnmodifiableSortedSet[T](s)

  def unmodifiableList[T](list: List[_ <: T]): List[T] = {
    list match {
      case _: RandomAccess =>
        new UnmodifiableList[T](list.asInstanceOf[List[T]]) with RandomAccess
      case _ =>
        new UnmodifiableList[T](list.asInstanceOf[List[T]])
    }
  }

  def unmodifiableMap[K, V](m: Map[_ <: K, _ <: V]): Map[K, V] =
    new UnmodifiableMap[K, V, Map[K, V]](m.asInstanceOf[Map[K, V]])

  def unmodifiableSortedMap[K, V](m: SortedMap[K, _ <: V]): SortedMap[K, V] =
    new UnmodifiableSortedMap[K, V](m.asInstanceOf[SortedMap[K, V]])

  def synchronizedCollection[T](c: Collection[T]): Collection[T] = {
    new WrappedCollection[T, Collection[T]] {
      override protected val inner: Collection[T] = c
    }
  }

  def synchronizedSet[T](s: Set[T]): Set[T] = {
    new WrappedSet[T, Set[T]] {
      override protected val inner: Set[T] = s
    }
  }

  def synchronizedSortedSet[T](s: SortedSet[T]): SortedSet[T] = {
    new WrappedSortedSet[T] {
      override protected val inner: SortedSet[T] = s
    }
  }

  def synchronizedList[T](list: List[T]): List[T] = {
    class BasicSynchronizedList extends WrappedList[T] {
      override protected val inner: List[T] = list
    }
    list match {
      case _: RandomAccess => new BasicSynchronizedList with RandomAccess
      case _               => new BasicSynchronizedList
    }
  }

  def synchronizedMap[K, V](m: Map[K, V]): Map[K, V] = {
    new WrappedMap[K, V, Map[K, V]] {
      override protected val inner: Map[K, V] = m
    }
  }

  def synchronizedSortedMap[K, V](m: SortedMap[K, V]): SortedMap[K, V] = {
    new WrappedSortedMap[K, V] {
      override protected val inner: SortedMap[K, V] = m
    }
  }

  def checkedCollection[E](c: Collection[E], typ: Class[E]): Collection[E] =
    new CheckedCollection[E, Collection[E]](c, typ)

  def checkedSet[E](s: Set[E], typ: Class[E]): Set[E] =
    new CheckedSet[E, Set[E]](s, typ)

  def checkedSortedSet[E](s: SortedSet[E], typ: Class[E]): SortedSet[E] =
    new CheckedSortedSet[E](s, typ)

  def checkedList[E](list: List[E], typ: Class[E]): List[E] = {
    list match {
      case _: RandomAccess => new CheckedList[E](list, typ) with RandomAccess
      case _               => new CheckedList[E](list, typ)
    }
  }

  def checkedMap[K, V](m: Map[K, V], keyType: Class[K], valueType: Class[V]): Map[K, V] =
    new CheckedMap[K, V, Map[K, V]](m, keyType, valueType)

  def checkedSortedMap[K, V](m: SortedMap[K, V], keyType: Class[K], valueType: Class[V]): SortedMap[K, V] =
    new CheckedSortedMap[K, V](m, keyType, valueType)

  def emptyIterator[T](): Iterator[T] =
    EMPTY_ITERATOR.asInstanceOf[Iterator[T]]

  def emptyListIterator[T](): ListIterator[T] =
    EMPTY_LIST_ITERATOR.asInstanceOf[ListIterator[T]]

  def emptyEnumeration[T](): Enumeration[T] =
    EMPTY_ENUMERATION.asInstanceOf[Enumeration[T]]

  def emptySet[T](): Set[T] =
    EMPTY_SET.asInstanceOf[Set[T]]

  def emptyList[T](): List[T] =
    EMPTY_LIST.asInstanceOf[List[T]]

  def emptyMap[K, V](): Map[K, V] =
    EMPTY_MAP.asInstanceOf[Map[K, V]]

  def singleton[T](o: T): Set[T] = {
    new ImmutableSet(new AbstractSet[T] with Serializable {
      def size(): Int = 1

      def iterator(): Iterator[T] = {
        new Iterator[T] {
          private var _hasNext: Boolean = true

          def hasNext(): Boolean = _hasNext

          def next(): T = {
            if (!_hasNext)
              throw new NoSuchElementException
            _hasNext = false
            o
          }

          def remove(): Unit =
            throw new UnsupportedOperationException
        }
      }
    })
  }

  def singletonList[T](o: T): List[T] = {
    new ImmutableList(new AbstractList[T] with Serializable {
      def size(): Int = 1

      def get(index: Int): T =
        if (index == 0) o
        else throw new IndexOutOfBoundsException(index.toString)
    })
  }

  def singletonMap[K, V](key: K, value: V): Map[K, V] = {
    new ImmutableMap(new AbstractMap[K, V] with Serializable {
      def entrySet(): Set[Map.Entry[K, V]] =
        singleton(new AbstractMap.SimpleImmutableEntry(key, value))
    })
  }

  def nCopies[T](n: Int, o: T): List[T] = {
    if (n < 0)
      throw new IllegalArgumentException

    val inner = new AbstractList[T] with Serializable with RandomAccess {
      def size(): Int = n

      def get(index: Int): T = {
        if (index < 0 || index >= n)
          throw new IndexOutOfBoundsException
        o
      }
    }
    new ImmutableList(inner) with RandomAccess
  }

  def reverseOrder[T](): Comparator[T] = {
    new Comparator[T] with Serializable {
      def compare(o1: T, o2: T): Int = o2.asInstanceOf[Comparable[T]].compareTo(o1)
    }
  }

  def reverseOrder[T](cmp: Comparator[T]): Comparator[T] = {
    new Comparator[T] with Serializable {
      override def compare(o1: T, o2: T): Int = cmp.compare(o2, o1)
    }
  }

  def enumeration[T](c: Collection[T]): Enumeration[T] = {
    val it = c.iterator
    new Enumeration[T] {
      override def hasMoreElements: Boolean =
        it.hasNext

      override def nextElement(): T =
        it.next()
    }
  }

  def list[T](e: Enumeration[T]): ArrayList[T] = {
    val arrayList = new ArrayList[T]
    e.asScala.foreach(arrayList.add(_))
    arrayList
  }

  def frequency(c: Collection[_], o: AnyRef): Int =
    c.asScala.count(_ === o)

  def disjoint(c1: Collection[_], c2: Collection[_]): Boolean = {
    if (c1.size < c2.size)
      !c1.asScala.exists(elem => c2.contains(elem))
    else
      !c2.asScala.exists(elem => c1.contains(elem))
  }

  def addAll[T](c: Collection[_ >: T], elements: Array[AnyRef]): Boolean =
    c.addAll((elements.asInstanceOf[Array[T]]: Seq[T]).asJava)

  def newSetFromMap[E](map: Map[E, java.lang.Boolean]): Set[E] = {
    if (!map.isEmpty)
      throw new IllegalArgumentException

    new WrappedSet[E, Set[E]] {
      override protected val inner: Set[E] =
        map.keySet

      override def add(e: E): Boolean =
        map.put(e, true) == null

      override def addAll(c: Collection[_ <: E]): Boolean = {
        c.asScala.foldLeft(false) {
          (prev, elem) => map.put(elem, true) == null || prev
        }
      }
    }
  }

  @inline
  private def modulo(a: Int, b: Int): Int = ((a % b) + b) % b

  @inline
  private def naturalComparator[T <: jl.Comparable[T]]: Comparator[T] = {
    new Comparator[T] with Serializable {
      final def compare(o1: T, o2: T): Int = o1.compareTo(o2)
    }
  }

  @inline
  private implicit def comparatorToOrdering[E](cmp: Comparator[E]): Ordering[E] = {
    new Ordering[E] {
      final def compare(x: E, y: E): Int = cmp.compare(x, y)
    }
  }

  private trait WrappedEquals {
    protected def inner: AnyRef

    override def equals(obj: Any): Boolean =
      inner.equals(obj)

    override def hashCode(): Int =
      inner.hashCode
  }

  private trait WrappedCollection[E, Coll <: Collection[E]]
      extends Collection[E] with Serializable {

    protected def inner: Coll

    def size(): Int =
      inner.size

    def isEmpty: Boolean =
      inner.isEmpty

    def contains(o: Any): Boolean =
      inner.contains(o)

    def iterator(): Iterator[E] =
      inner.iterator

    def toArray: Array[AnyRef] =
      inner.toArray()

    def toArray[T <: AnyRef](a: Array[T]): Array[T] =
      inner.toArray[T](a)

    def add(e: E): Boolean =
      inner.add(e)

    def remove(o: Any): Boolean =
      inner.remove(o)

    def containsAll(c: Collection[_]): Boolean =
      inner.containsAll(c)

    def addAll(c: Collection[_ <: E]): Boolean =
      inner.addAll(c)

    def removeAll(c: Collection[_]): Boolean =
      inner.removeAll(c)

    def retainAll(c: Collection[_]): Boolean =
      inner.retainAll(c)

    def clear(): Unit =
      inner.clear()

    override def toString: String =
      inner.toString
  }

  private trait WrappedSet[E, Coll <: Set[E]]
      extends WrappedEquals with WrappedCollection[E, Coll] with Set[E]

  private trait WrappedSortedSet[E]
      extends WrappedSet[E, SortedSet[E]] with SortedSet[E] {

    def comparator(): Comparator[_ >: E] =
      inner.comparator()

    def subSet(fromElement: E, toElement: E): SortedSet[E] =
      inner.subSet(fromElement, toElement)

    def tailSet(fromElement: E): SortedSet[E] =
      inner.tailSet(fromElement)

    def headSet(toElement: E): SortedSet[E] =
      inner.headSet(toElement)

    def first(): E =
      inner.first

    def last(): E =
      inner.last
  }

  private trait WrappedList[E]
      extends WrappedEquals with WrappedCollection[E, List[E]] with List[E] {

    def addAll(index: Int, c: Collection[_ <: E]): Boolean =
      inner.addAll(index, c)

    def get(index: Int): E =
      inner.get(index)

    def set(index: Int, element: E): E =
      inner.set(index, element)

    def add(index: Int, element: E): Unit =
      inner.add(index, element)

    def remove(index: Int): E =
      inner.remove(index)

    def indexOf(o: scala.Any): Int =
      inner.indexOf(o)

    def lastIndexOf(o: scala.Any): Int =
      inner.lastIndexOf(o)

    def listIterator(): ListIterator[E] =
      inner.listIterator()

    def listIterator(index: Int): ListIterator[E] =
      inner.listIterator(index)

    def subList(fromIndex: Int, toIndex: Int): List[E] =
      inner.subList(fromIndex, toIndex)
  }

  private trait WrappedMap[K, V, M <: Map[K, V]]
      extends WrappedEquals with Map[K, V] {

    protected def inner: M

    def size(): Int =
      inner.size()

    def isEmpty: Boolean =
      inner.isEmpty

    def containsKey(key: scala.Any): Boolean =
      inner.containsKey(key)

    def containsValue(value: scala.Any): Boolean =
      inner.containsValue(value)

    def get(key: scala.Any): V =
      inner.get(key)

    def put(key: K, value: V): V =
      inner.put(key, value)

    def remove(key: scala.Any): V =
      inner.remove(key)

    def putAll(m: Map[_ <: K, _ <: V]): Unit =
      inner.putAll(m)

    def clear(): Unit =
      inner.clear()

    def keySet(): Set[K] =
      inner.keySet

    def values(): Collection[V] =
      inner.values

    def entrySet(): Set[Map.Entry[K, V]] =
      inner.entrySet.asInstanceOf[Set[Map.Entry[K, V]]]

    override def toString(): String =
      inner.toString
  }

  private trait WrappedSortedMap[K, V]
      extends WrappedMap[K, V, SortedMap[K, V]] with SortedMap[K, V] {
    def comparator(): Comparator[_ >: K] =
      inner.comparator

    def subMap(fromKey: K, toKey: K): SortedMap[K, V] =
      inner.subMap(fromKey, toKey)

    def headMap(toKey: K): SortedMap[K, V] =
      inner.headMap(toKey)

    def tailMap(fromKey: K): SortedMap[K, V] =
      inner.tailMap(fromKey)

    def firstKey(): K =
      inner.firstKey

    def lastKey(): K =
      inner.lastKey
  }

  private trait WrappedIterator[E, Iter <: Iterator[E]] extends Iterator[E] {
    protected def inner: Iter

    def hasNext(): Boolean =
      inner.hasNext

    def next(): E =
      inner.next()

    def remove(): Unit =
      inner.remove()
  }

  private trait WrappedListIterator[E]
      extends WrappedIterator[E, ListIterator[E]] with ListIterator[E] {
    def hasPrevious(): Boolean =
      inner.hasPrevious

    def previous(): E =
      inner.previous()

    def nextIndex(): Int =
      inner.nextIndex

    def previousIndex(): Int =
      inner.previousIndex

    def set(e: E): Unit =
      inner.set(e)

    def add(e: E): Unit =
      inner.add(e)
  }

  private class UnmodifiableCollection[E, Coll <: Collection[E]](
      protected val inner: Coll) extends WrappedCollection[E, Coll] {

    protected val eagerThrow: Boolean = true

    override def clear(): Unit = {
      if (eagerThrow || !isEmpty)
        throw new UnsupportedOperationException
    }

    override def iterator(): Iterator[E] =
      new UnmodifiableIterator(inner.iterator)

    override def add(e: E): Boolean =
      throw new UnsupportedOperationException

    override def remove(o: Any): Boolean =
      if (eagerThrow || contains(o)) throw new UnsupportedOperationException
      else false

    override def addAll(c: Collection[_ <: E]): Boolean =
      if (eagerThrow || !c.isEmpty) throw new UnsupportedOperationException
      else false

    override def removeAll(c: Collection[_]): Boolean = {
      if (eagerThrow) {
        throw new UnsupportedOperationException
      } else {
        val cSet = c.asInstanceOf[Collection[AnyRef]].asScala.toSet
        if (this.asScala.exists(e => cSet(e.asInstanceOf[AnyRef]))) {
          throw new UnsupportedOperationException
        } else {
          false
        }
      }
    }

    override def retainAll(c: Collection[_]): Boolean = {
      if (eagerThrow) {
        throw new UnsupportedOperationException
      } else {
        val cSet = c.asInstanceOf[Collection[AnyRef]].asScala.toSet
        if (this.asScala.exists(e => !cSet(e.asInstanceOf[AnyRef]))) {
          throw new UnsupportedOperationException
        } else {
          false
        }
      }
    }
  }

  private class UnmodifiableSet[E, Coll <: Set[E]](inner: Coll)
      extends UnmodifiableCollection[E, Coll](inner) with WrappedSet[E, Coll]

  private class ImmutableSet[E](inner: Set[E])
      extends UnmodifiableSet[E, Set[E]](inner) {
    override protected val eagerThrow: Boolean = false
  }

  private class UnmodifiableSortedSet[E](inner: SortedSet[E])
      extends UnmodifiableSet[E, SortedSet[E]](inner) with WrappedSortedSet[E]

  private class UnmodifiableList[E](inner: List[E])
      extends UnmodifiableCollection[E, List[E]](inner) with WrappedList[E] {

    override def addAll(index: Int, c: Collection[_ <: E]): Boolean =
      if (eagerThrow || !c.isEmpty) throw new UnsupportedOperationException
      else false

    override def set(index: Int, element: E): E =
      throw new UnsupportedOperationException

    override def add(index: Int, element: E): Unit =
      throw new UnsupportedOperationException

    override def remove(index: Int): E =
      throw new UnsupportedOperationException

    override def listIterator(): ListIterator[E] =
      new UnmodifiableListIterator(this.inner.listIterator())

    override def listIterator(index: Int): ListIterator[E] =
      new UnmodifiableListIterator(this.inner.listIterator(index))

    override def subList(fromIndex: Int, toIndex: Int): List[E] =
      unmodifiableList(super.subList(fromIndex, toIndex))
  }

  private class ImmutableList[E](inner: List[E])
      extends UnmodifiableList(inner) {
    override protected val eagerThrow: Boolean = false
  }

  private class UnmodifiableMap[K, V, M <: Map[K, V]](
      protected val inner: M) extends WrappedMap[K, V, M] {

    protected val eagerThrow: Boolean = true

    override def put(key: K, value: V): V =
      throw new UnsupportedOperationException

    override def remove(key: scala.Any): V = {
      if (eagerThrow || containsKey(key)) throw new UnsupportedOperationException
      else null.asInstanceOf[V]
    }

    override def putAll(m: Map[_ <: K, _ <: V]): Unit = {
      if (eagerThrow || !m.isEmpty)
        throw new UnsupportedOperationException
    }

    override def clear(): Unit = {
      if (eagerThrow || !isEmpty)
        throw new UnsupportedOperationException
    }

    override def keySet(): Set[K] =
      unmodifiableSet(super.keySet)

    override def values(): Collection[V] =
      unmodifiableCollection(super.values)

    override def entrySet(): Set[Map.Entry[K, V]] =
      unmodifiableSet(super.entrySet)
  }

  private class ImmutableMap[K, V](
      inner: Map[K, V]) extends UnmodifiableMap[K, V, Map[K, V]](inner) {
    override protected val eagerThrow: Boolean = false
  }

  private class UnmodifiableSortedMap[K, V](inner: SortedMap[K, V])
      extends UnmodifiableMap[K, V, SortedMap[K, V]](inner) with WrappedSortedMap[K, V] {

    override def subMap(fromKey: K, toKey: K): SortedMap[K, V] =
      unmodifiableSortedMap(super.subMap(fromKey, toKey))

    override def headMap(toKey: K): SortedMap[K, V] =
      unmodifiableSortedMap(super.headMap(toKey))

    override def tailMap(fromKey: K): SortedMap[K, V] =
      unmodifiableSortedMap(super.tailMap(fromKey))
  }

  private class UnmodifiableIterator[E, Iter <: Iterator[E]](protected val inner: Iter)
      extends WrappedIterator[E, Iter] {
    override def remove(): Unit = throw new UnsupportedOperationException
  }

  private class UnmodifiableListIterator[E](innerIterator: ListIterator[E])
      extends UnmodifiableIterator[E, ListIterator[E]](innerIterator)
      with WrappedListIterator[E] {
    override def set(e: E): Unit = throw new UnsupportedOperationException

    override def add(e: E): Unit = throw new UnsupportedOperationException
  }

  private final def checkClass[T](elem: T, clazz: Class[T]): Unit =
    clazz.cast(elem)

  private class CheckedCollection[E, Coll <: Collection[E]](
      protected val inner: Coll, protected val elemClazz: Class[E])
      extends WrappedCollection[E, Coll] {

    override def add(e: E): Boolean = {
      checkElem(e)
      super.add(e)
    }

    override def addAll(c: Collection[_ <: E]): Boolean = {
      c.asScala.foreach(checkElem)
      super.addAll(c)
    }

    protected final def checkElem(elem: E) =
      checkClass(elem, elemClazz)
  }

  private class CheckedSet[E, Coll <: Set[E]](inner: Coll, elemClazz: Class[E])
      extends CheckedCollection[E, Coll](inner, elemClazz) with WrappedSet[E, Coll]

  private class CheckedSortedSet[E](inner: SortedSet[E], elemClazz: Class[E])
      extends CheckedSet[E, SortedSet[E]](inner, elemClazz) with WrappedSortedSet[E] {

    override def subSet(fromElement: E, toElement: E): SortedSet[E] =
      checkedSortedSet(super.subSet(fromElement, toElement), this.elemClazz)

    override def headSet(toElement: E): SortedSet[E] =
      checkedSortedSet(super.headSet(toElement), this.elemClazz)

    override def tailSet(fromElement: E): SortedSet[E] =
      checkedSortedSet(super.tailSet(fromElement), this.elemClazz)
  }

  private class CheckedList[E](inner: List[E], elemClazz: Class[E])
      extends CheckedCollection[E, List[E]](inner, elemClazz) with WrappedList[E] {

    override def addAll(index: Int, c: Collection[_ <: E]): Boolean = {
      c.asScala.foreach(checkElem)
      super.addAll(index, c)
    }

    override def set(index: Int, element: E): E = {
      checkElem(element)
      super.set(index, element)
    }

    override def add(index: Int, element: E): Unit = {
      checkElem(element)
      super.add(index, element)
    }

    override def listIterator(): ListIterator[E] = listIterator(0)

    override def listIterator(index: Int): ListIterator[E] =
      new CheckedListIterator[E](this.inner.listIterator(index), this.elemClazz)

    override def subList(fromIndex: Int, toIndex: Int): List[E] =
      checkedList(super.subList(fromIndex, toIndex), this.elemClazz)
  }

  private class CheckedMap[K, V, M <: Map[K, V]](protected val inner: M, protected val keyClazz: Class[K],
      protected val valueClazz: Class[V]) extends WrappedMap[K, V, M] {

    override def put(key: K, value: V): V = {
      checkKeyAndValue(key, value)
      super.put(key, value)
    }

    override def putAll(m: Map[_ <: K, _ <: V]): Unit = {
      m.entrySet().asScala.foreach {
        entry => checkKeyAndValue(entry.getKey, entry.getValue)
      }
      super.putAll(m)
    }

    override def entrySet(): Set[Map.Entry[K, V]] = {
      val innerSet = super.entrySet()
      new WrappedSet[Map.Entry[K, V], Set[Map.Entry[K, V]]] {
        protected def inner: Set[Map.Entry[K, V]] = innerSet

        override def iterator(): Iterator[Map.Entry[K, V]] = {
          val innerIterator = super.iterator()
          new WrappedIterator[Map.Entry[K, V], Iterator[Map.Entry[K, V]]] {
            protected def inner: Iterator[Map.Entry[K, V]] = innerIterator

            override def next(): Map.Entry[K, V] = {
              val nextEntry = super.next()
              new Map.Entry[K, V] {
                def getKey(): K =
                  nextEntry.getKey()

                def getValue(): V =
                  nextEntry.getValue()

                def setValue(value: V): V = {
                  checkClass(value, valueClazz)
                  nextEntry.setValue(value)
                }

                override def equals(o: Any): Boolean =
                  nextEntry.equals(o)

                override def hashCode(): Int =
                  nextEntry.hashCode()
              }
            }
          }
        }
      }
    }

    protected final def checkKeyAndValue(key: K, value: V): Unit = {
      checkClass(key, keyClazz)
      checkClass(value, valueClazz)
    }
  }

  private class CheckedSortedMap[K, V](
      inner: SortedMap[K, V], keyClazz: Class[K], valueClazz: Class[V])
      extends CheckedMap[K, V, SortedMap[K, V]](inner, keyClazz, valueClazz)
      with WrappedSortedMap[K, V] {

    override def subMap(fromKey: K, toKey: K): SortedMap[K, V] =
      checkedSortedMap(super.subMap(fromKey, toKey), keyClazz, valueClazz)

    override def headMap(toKey: K): SortedMap[K, V] =
      checkedSortedMap(super.headMap(toKey), keyClazz, valueClazz)

    override def tailMap(fromKey: K): SortedMap[K, V] =
      checkedSortedMap(super.tailMap(fromKey), keyClazz, valueClazz)
  }

  private class CheckedListIterator[E](protected val inner: ListIterator[E],
      protected val elemClazz: Class[E]) extends WrappedListIterator[E] {
    override def set(e: E): Unit = {
      checkElem(e)
      super.set(e)
    }

    override def add(e: E): Unit = {
      checkElem(e)
      super.add(e)
    }

    private def checkElem(elem: E): Unit = {
      checkClass(elem, elemClazz)
    }
  }

  private class EmptyIterator extends Iterator[Any] {
    def hasNext(): Boolean = false

    def next(): Any =
      throw new NoSuchElementException

    def remove(): Unit =
      throw new IllegalStateException
  }

  private class EmptyListIterator extends EmptyIterator with ListIterator[Any] {
    def hasPrevious(): Boolean = false

    def previous(): Any =
      throw new NoSuchElementException

    def nextIndex(): Int = 0

    def previousIndex(): Int = -1

    def set(e: Any): Unit =
      throw new IllegalStateException

    def add(e: Any): Unit =
      throw new UnsupportedOperationException
  }
}
