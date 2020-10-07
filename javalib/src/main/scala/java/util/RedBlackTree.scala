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

import scala.annotation.tailrec

import scala.scalajs.js

/** The red-black tree implementation used by `TreeSet`s.
 *
 *  It could also be used by `TreeMap`s in the future.
 *
 *  This implementation was copied and adapted from
 *  `scala.collection.mutable.RedBlackTree` as found in Scala 2.13.0.
 */
private[util] object RedBlackTree {

  // ---- bounds helpers ----

  type BoundKind = Int

  /* The values of `InclusiveBound` and `ExclusiveBound` must be 0 and 1,
   * respectively. Some of the algorithms in this implementation rely on them
   * having those specific values, as they do arithmetics with them.
   */
  final val InclusiveBound = 0
  final val ExclusiveBound = 1
  final val NoBound = 2

  @inline
  def boundKindFromIsInclusive(isInclusive: Boolean): BoundKind =
    if (isInclusive) InclusiveBound
    else ExclusiveBound

  @inline
  final class Bound[A](val bound: A, val kind: BoundKind)

  def isWithinLowerBound[A](key: Any, bound: A, boundKind: BoundKind)(
      implicit comp: Comparator[_ >: A]): Boolean = {
    boundKind == NoBound || compare(key, bound) >= boundKind
  }

  def isWithinUpperBound[A](key: Any, bound: A, boundKind: BoundKind)(
      implicit comp: Comparator[_ >: A]): Boolean = {
    boundKind == NoBound || compare(key, bound) <= -boundKind
  }

  def intersectLowerBounds[A](bound1: Bound[A], bound2: Bound[A])(
      implicit comp: Comparator[_ >: A]): Bound[A] = {
    if (bound1.kind == NoBound) {
      bound2
    } else if (bound2.kind == NoBound) {
      bound1
    } else {
      val cmp = compare(bound1.bound, bound2.bound)
      if (cmp > 0 || (cmp == 0 && bound1.kind == ExclusiveBound))
        bound1
      else
        bound2
    }
  }

  def intersectUpperBounds[A](bound1: Bound[A], bound2: Bound[A])(
      implicit comp: Comparator[_ >: A]): Bound[A] = {
    if (bound1.kind == NoBound) {
      bound2
    } else if (bound2.kind == NoBound) {
      bound1
    } else {
      val cmp = compare(bound1.bound, bound2.bound)
      if (cmp < 0 || (cmp == 0 && bound1.kind == ExclusiveBound))
        bound1
      else
        bound2
    }
  }

  // ---- class structure ----

  /* For performance reasons, this implementation uses `null` references to
   * represent leaves instead of a sentinel node.
   *
   * Currently, the internal nodes do not store their subtree size - only the
   * tree object keeps track of its size. Therefore, while obtaining the size
   * of the whole tree is O(1), knowing the number of entries inside a range is
   * O(n) on the size of the range.
   */

  final class Tree[A, B](var root: Node[A, B], var size: Int) {
    def treeCopy(): Tree[A, B] = new Tree(copyTree(root), size)
  }

  object Tree {
    def empty[A, B]: Tree[A, B] = new Tree(null, 0)
  }

  final class Node[A, B](
      val key: A,
      private[RedBlackTree] var value: B,
      private[RedBlackTree] var red: Boolean,
      private[RedBlackTree] var left: Node[A, B],
      private[RedBlackTree] var right: Node[A, B],
      private[RedBlackTree] var parent: Node[A, B]
  ) extends Map.Entry[A, B] {
    def getKey(): A = key

    def getValue(): B = value

    def setValue(v: B): B = {
      val oldValue = value
      value = v
      oldValue
    }

    override def equals(that: Any): Boolean = that match {
      case that: Map.Entry[_, _] =>
        Objects.equals(getKey(), that.getKey()) &&
        Objects.equals(getValue(), that.getValue())
      case _ =>
        false
    }

    override def hashCode(): Int =
      Objects.hashCode(key) ^ Objects.hashCode(value)

    override def toString(): String =
      "" + getKey() + "=" + getValue()

    @inline private[RedBlackTree] def isRoot: Boolean =
      parent eq null

    @inline private[RedBlackTree] def isLeftChild: Boolean =
      (parent ne null) && (parent.left eq this)

    @inline private[RedBlackTree] def isRightChild: Boolean =
      (parent ne null) && (parent.right eq this)
  }

  object Node {
    @inline
    def apply[A, B](key: A, value: B, red: Boolean, left: Node[A, B],
        right: Node[A, B], parent: Node[A, B]): Node[A, B] = {
      new Node(key, value, red, left, right, parent)
    }

    @inline
    def leaf[A, B](key: A, value: B, red: Boolean, parent: Node[A, B]): Node[A, B] =
      new Node(key, value, red, null, null, parent)
  }

  // ---- comparator helper ----

  @inline
  private[this] def compare[A](key1: Any, key2: A)(
      implicit comp: Comparator[_ >: A]): Int = {
    /* The implementation of `compare` and/or its generic bridge may perform
     * monomorphic casts that can fail. This is according to spec for TreeSet
     * and TreeMap.
     */
    comp.asInstanceOf[Comparator[Any]].compare(key1, key2)
  }

  // ---- getters ----

  private def isRed(node: Node[_, _]): Boolean = (node ne null) && node.red

  private def isBlack(node: Node[_, _]): Boolean = (node eq null) || !node.red

  // ---- size ----

  private def size(node: Node[_, _]): Int =
    if (node eq null) 0
    else 1 + size(node.left) + size(node.right)

  def size(tree: Tree[_, _]): Int = tree.size

  def projectionSize[A, B](tree: Tree[A, B], lowerBound: A,
      lowerKind: BoundKind, upperBound: A, upperKind: BoundKind)(
      implicit comp: Comparator[_ >: A]): Int = {
    if (lowerKind == NoBound && upperKind == NoBound) {
      size(tree)
    } else {
      var result = 0
      val iter =
        projectionIterator(tree, lowerBound, lowerKind, upperBound, upperKind)
      while (iter.hasNext()) {
        iter.next()
        result += 1
      }
      result
    }
  }

  def isEmpty(tree: Tree[_, _]): Boolean = tree.root eq null

  def projectionIsEmpty[A](tree: Tree[A, _], lowerBound: A,
      lowerKind: BoundKind, upperBound: A, upperKind: BoundKind)(
      implicit comp: Comparator[_ >: A]): Boolean = {
    if (lowerKind == NoBound && upperKind == NoBound) {
      isEmpty(tree)
    } else {
      val node = minNodeAfter(tree, lowerBound, lowerKind)
      (node eq null) || !isWithinUpperBound(node.key, upperBound, upperKind)
    }
  }

  def clear(tree: Tree[_, _]): Unit = {
    tree.root = null
    tree.size = 0
  }

  // ---- search ----

  def get[A, B](tree: Tree[A, B], key: Any)(
      implicit comp: Comparator[_ >: A]): B = {
    nullableNodeFlatMap(getNode(tree.root, key))(_.value)
  }

  @tailrec
  private[this] def getNode[A, B](node: Node[A, B], key: Any)(
      implicit comp: Comparator[_ >: A]): Node[A, B] = {
    if (node eq null) {
      null
    } else {
      val cmp = compare(key, node.key)
      if (cmp < 0) getNode(node.left, key)
      else if (cmp > 0) getNode(node.right, key)
      else node
    }
  }

  def contains[A](tree: Tree[A, _], key: Any)(
      implicit comp: Comparator[_ >: A]): Boolean = {
    getNode(tree.root, key) ne null
  }

  def minNode[A, B](tree: Tree[A, B]): Node[A, B] =
    minNode(tree.root)

  def minKey[A](tree: Tree[A, _]): A =
    nullableNodeKey(minNode(tree.root))

  private def minNode[A, B](node: Node[A, B]): Node[A, B] =
    nullableNodeFlatMap(node)(minNodeNonNull(_))

  @tailrec
  private def minNodeNonNull[A, B](node: Node[A, B]): Node[A, B] =
    if (node.left eq null) node
    else minNodeNonNull(node.left)

  def maxNode[A, B](tree: Tree[A, B]): Node[A, B] =
    maxNode(tree.root)

  def maxKey[A](tree: Tree[A, _]): A =
    nullableNodeKey(maxNode(tree.root))

  private def maxNode[A, B](node: Node[A, B]): Node[A, B] =
    nullableNodeFlatMap(node)(maxNodeNonNull(_))

  @tailrec
  private def maxNodeNonNull[A, B](node: Node[A, B]): Node[A, B] =
    if (node.right eq null) node
    else maxNodeNonNull(node.right)

  /** Returns the first (lowest) map entry with a key (equal or) greater than
   *  `key`.
   *
   *  Returns `null` if there is no such node.
   */
  def minNodeAfter[A, B](tree: Tree[A, B], key: A, boundKind: BoundKind)(
      implicit comp: Comparator[_ >: A]): Node[A, B] = {
    minNodeAfter(tree.root, key, boundKind)
  }

  def minKeyAfter[A](tree: Tree[A, _], key: A, boundKind: BoundKind)(
      implicit comp: Comparator[_ >: A]): A = {
    nullableNodeKey(minNodeAfter(tree.root, key, boundKind))
  }

  private def minNodeAfter[A, B](node: Node[A, B], key: A, boundKind: BoundKind)(
      implicit comp: Comparator[_ >: A]): Node[A, B] = {
    if (node eq null) {
      null
    } else if (boundKind == NoBound) {
      minNodeNonNull(node)
    } else {
      @tailrec
      def minNodeAfterNonNull(node: Node[A, B]): Node[A, B] = {
        val cmp = compare(key, node.key)
        if (cmp == 0) {
          if (boundKind == InclusiveBound)
            node
          else
            successor(node)
        } else if (cmp < 0) {
          val child = node.left
          if (child eq null)
            node
          else
            minNodeAfterNonNull(child)
        } else {
          val child = node.right
          if (child eq null)
            successor(node)
          else
            minNodeAfterNonNull(child)
        }
      }

      minNodeAfterNonNull(node)
    }
  }

  /** Returns the last (highest) map entry with a key (equal or) smaller than
   *  `key`.
   *
   *  Returns `null` if there is no such node.
   */
  def maxNodeBefore[A, B](tree: Tree[A, B], key: A, boundKind: BoundKind)(
      implicit comp: Comparator[_ >: A]): Node[A, B] = {
    maxNodeBefore(tree.root, key, boundKind)
  }

  def maxKeyBefore[A](tree: Tree[A, _], key: A, boundKind: BoundKind)(
      implicit comp: Comparator[_ >: A]): A = {
    nullableNodeKey(maxNodeBefore(tree.root, key, boundKind))
  }

  private def maxNodeBefore[A, B](node: Node[A, B], key: A, boundKind: BoundKind)(
      implicit comp: Comparator[_ >: A]): Node[A, B] = {
    if (node eq null) {
      null
    } else if (boundKind == NoBound) {
      maxNodeNonNull(node)
    } else {
      @tailrec
      def maxNodeBeforeNonNull(node: Node[A, B]): Node[A, B] = {
        val cmp = compare(key, node.key)
        if (cmp == 0) {
          if (boundKind == InclusiveBound)
            node
          else
            predecessor(node)
        } else if (cmp < 0) {
          val child = node.left
          if (child eq null)
            predecessor(node)
          else
            maxNodeBeforeNonNull(child)
        } else {
          val child = node.right
          if (child eq null)
            node
          else
            maxNodeBeforeNonNull(child)
        }
      }

      maxNodeBeforeNonNull(node)
    }
  }

  // ---- insertion ----

  def insert[A, B](tree: Tree[A, B], key: A, value: B)(
      implicit comp: Comparator[_ >: A]): B = {
    /* The target node is the node with `key` if it exists, or the node under
     * which we need to insert the new `key`.
     * We use a loop here instead of a tailrec def because we need 2 results
     * from this lookup: `targetNode` and `cmp`.
     */
    var targetNode: Node[A, B] = null
    var nextNode: Node[A, B] = tree.root
    var cmp: Int = 1
    while ((nextNode ne null) && cmp != 0) {
      targetNode = nextNode
      cmp = compare(key, nextNode.key)
      nextNode = if (cmp < 0) nextNode.left else nextNode.right
    }

    if (cmp == 0) {
      // Found existing node: just update its value
      targetNode.setValue(value)
    } else {
      // Insert a new node under targetNode, then fix the RB tree
      val newNode = Node.leaf(key, value, red = true, targetNode)

      if (targetNode eq null) {
        /* Here, the key was never compared to anything. Compare it with itself
         * so that we eagerly cause the comparator to throw if it cannot handle
         * the key at all, before we put the key in the map.
         */
        compare(key, key)
        tree.root = newNode
      } else if (cmp < 0) {
        targetNode.left = newNode
      } else {
        targetNode.right = newNode
      }

      fixAfterInsert(tree, newNode)
      tree.size += 1

      null.asInstanceOf[B]
    }
  }

  @tailrec
  private[this] def fixAfterInsert[A, B](tree: Tree[A, B],
      node: Node[A, B]): Unit = {
    val parent = node.parent
    if (parent eq null) {
      // The inserted node is the root; mark it black and we're done
      node.red = false
    } else if (isBlack(parent)) {
      // The parent is black and the node is red; we're done
    } else if (parent.isLeftChild) {
      val grandParent = parent.parent
      val uncle = grandParent.right
      if (isRed(uncle)) {
        parent.red = false
        uncle.red = false
        grandParent.red = true
        fixAfterInsert(tree, grandParent)
      } else {
        if (node.isRightChild) {
          rotateLeft(tree, parent)
          // Now `parent` is the child of `node`, which is the child of `grandParent`
          node.red = false
        } else {
          parent.red = false
        }
        grandParent.red = true
        rotateRight(tree, grandParent)
        // Now the node which took the place of `grandParent` is black, so we're done
      }
    } else {
      // Symmetric cases
      val grandParent = parent.parent
      val uncle = grandParent.left
      if (isRed(uncle)) {
        parent.red = false
        uncle.red = false
        grandParent.red = true
        fixAfterInsert(tree, grandParent)
      } else {
        if (node.isLeftChild) {
          rotateRight(tree, parent)
          // Now `parent` is the child of `node`, which is the child of `grandParent`
          node.red = false
        } else {
          parent.red = false
        }
        grandParent.red = true
        rotateLeft(tree, grandParent)
        // Now the node which took the place of `grandParent` is black, so we're done
      }
    }
  }

  // ---- deletion ----

  def delete[A, B](tree: Tree[A, B], key: Any)(
      implicit comp: Comparator[_ >: A]): B = {
    nullableNodeFlatMap(getNode(tree.root, key)) { node =>
      deleteNode(tree, node)
      node.value
    }
  }

  def deleteNode[A, B](tree: Tree[A, B], node: Node[A, B]): Unit = {
    if (node.left eq null) {
      val onlyChild = node.right // can be null
      transplant(tree, node, onlyChild)
      if (!node.red)
        fixAfterDelete(tree, onlyChild, node.parent)
    } else if (node.right eq null) {
      val onlyChild = node.left
      transplant(tree, node, onlyChild)
      if (!node.red)
        fixAfterDelete(tree, onlyChild, node.parent)
    } else {
      /* We don't know how to delete a node with 2 children, so we're going to
       * find the successor `succ` of `node`, then (conceptually) swap it with
       * `node` before deleting `node`. We can do this because we know that
       * `succ` has a null `left` child.
       *
       * In fact we transplant the `onlyChildOfSucc` (originally at
       * `succ.right`) in place of `succ`, then transplant `succ` in place of
       * `node` (also acquiring its color), and finally fixing up the tree
       * around `onlyChildOfSucc`.
       *
       * Textbook red-black trees simply set the `key` and `value` of `node` to
       * be those of `succ`, then delete `succ`. We cannot do this because our
       * `key` is immutable (it *has* to be for `Node` to comply with the
       * contract of `Map.Entry`).
       */
      val succ = minNodeNonNull(node.right)
      // Assert: succ.left eq null
      val succWasRed = succ.red // conceptually, the color of `node` after the swap
      val onlyChildOfSucc = succ.right
      val newParentOfTheChild = if (succ.parent eq node) {
        succ
      } else {
        val theParent = succ.parent
        transplant(tree, succ, onlyChildOfSucc)
        succ.right = node.right
        succ.right.parent = succ
        theParent
      }
      transplant(tree, node, succ)
      succ.left = node.left
      succ.left.parent = succ
      succ.red = node.red
      // Assert: if (onlyChildOfSucc ne null) then (newParentOfTheChild eq onlyChildOfSucc.parent)
      if (!succWasRed)
        fixAfterDelete(tree, onlyChildOfSucc, newParentOfTheChild)
    }

    tree.size -= 1
  }

  /* `node` can be `null` (in which case it is black), so we have to pass
   * `parent` explicitly from above.
   */
  @tailrec
  private[this] def fixAfterDelete[A, B](tree: Tree[A, B], node: Node[A, B],
      parent: Node[A, B]): Unit = {
    if ((node ne tree.root) && isBlack(node)) {
      // `node` can *still* be null here; we cannot use `node.isLeftChild`
      if (node eq parent.left) {
        // From now on, we don't use `node` anymore; we just fix up at `parent`

        var rightChild = parent.right
        // assert(rightChild ne null)

        if (rightChild.red) {
          rightChild.red = false
          parent.red = true
          rotateLeft(tree, parent)
          rightChild = parent.right // shape changed; update `rightChild`
        }
        if (isBlack(rightChild.left) && isBlack(rightChild.right)) {
          rightChild.red = true
          fixAfterDelete(tree, parent, parent.parent)
        } else {
          if (isBlack(rightChild.right)) {
            rightChild.left.red = false
            rightChild.red = true
            rotateRight(tree, rightChild)
            rightChild = parent.right // shape changed; update `rightChild`
          }
          rightChild.red = parent.red
          parent.red = false
          rightChild.right.red = false
          rotateLeft(tree, parent)
          // we're done here
        }
      } else { // symmetric cases
        // From now on, we don't use `node` anymore; we just fix up at `parent`

        var leftChild = parent.left
        // assert(leftChild ne null)

        if (leftChild.red) {
          leftChild.red = false
          parent.red = true
          rotateRight(tree, parent)
          leftChild = parent.left // shape changed; update `leftChild`
        }
        if (isBlack(leftChild.right) && isBlack(leftChild.left)) {
          leftChild.red = true
          fixAfterDelete(tree, parent, parent.parent)
        } else {
          if (isBlack(leftChild.left)) {
            leftChild.right.red = false
            leftChild.red = true
            rotateLeft(tree, leftChild)
            leftChild = parent.left // shape changed; update `leftChild`
          }
          leftChild.red = parent.red
          parent.red = false
          leftChild.left.red = false
          rotateRight(tree, parent)
          // we're done here
        }
      }
    } else {
      // We found a red node or the root; mark it black and we're done
      if (node ne null)
        node.red = false
    }
  }

  // ---- helpers ----

  /** Returns `null.asInstanceOf[C]` if `node eq null`, otherwise `f(node)`. */
  @inline
  private def nullableNodeFlatMap[A, B, C](node: Node[A, B])(f: Node[A, B] => C): C =
    if (node eq null) null.asInstanceOf[C]
    else f(node)

  /** Returns `null.asInstanceOf[A]` if `node eq null`, otherwise `node.key`. */
  @inline
  private def nullableNodeKey[A, B](node: Node[A, B]): A =
    if (node eq null) null.asInstanceOf[A]
    else node.key

  /** Returns the node that follows `node` in an in-order tree traversal.
   *
   *  If `node` has the maximum key (and is, therefore, the last node), this
   *  method returns `null`.
   */
  private[this] def successor[A, B](node: Node[A, B]): Node[A, B] = {
    if (node.right ne null) {
      minNodeNonNull(node.right)
    } else {
      @inline @tailrec
      def closestAncestorOnTheRight(node: Node[A, B]): Node[A, B] = {
        val parent = node.parent
        if ((parent eq null) || (node eq parent.left)) parent
        else closestAncestorOnTheRight(parent)
      }
      closestAncestorOnTheRight(node)
    }
  }

  /** Returns the node that precedes `node` in an in-order tree traversal.
   *
   *  If `node` has the minimum key (and is, therefore, the first node), this
   *  method returns `null`.
   */
  private[this] def predecessor[A, B](node: Node[A, B]): Node[A, B] = {
    if (node.left ne null) {
      maxNodeNonNull(node.left)
    } else {
      @inline @tailrec
      def closestAncestorOnTheLeft(node: Node[A, B]): Node[A, B] = {
        val parent = node.parent
        if ((parent eq null) || (node eq parent.right)) parent
        else closestAncestorOnTheLeft(parent)
      }
      closestAncestorOnTheLeft(node)
    }
  }

  private[this] def rotateLeft[A, B](tree: Tree[A, B], x: Node[A, B]): Unit = {
    if (x ne null) {
      // assert(x.right ne null)
      val y = x.right
      x.right = y.left

      if (y.left ne null)
        y.left.parent = x
      y.parent = x.parent

      if (x.isRoot)
        tree.root = y
      else if (x.isLeftChild)
        x.parent.left = y
      else
        x.parent.right = y

      y.left = x
      x.parent = y
    }
  }

  private[this] def rotateRight[A, B](tree: Tree[A, B], x: Node[A, B]): Unit = {
    if (x ne null) {
      // assert(x.left ne null)
      val y = x.left
      x.left = y.right

      if (y.right ne null)
        y.right.parent = x
      y.parent = x.parent

      if (x.isRoot)
        tree.root = y
      else if (x.isRightChild)
        x.parent.right = y
      else
        x.parent.left = y

      y.right = x
      x.parent = y
    }
  }

  /** Transplant the node `from` to the place of node `to`.
   *
   *  This is done by setting `from` as a child of `to`'s previous parent and
   *  setting `from`'s parent to the `to`'s previous parent. The children of
   *  `from` are left unchanged.
   */
  private[this] def transplant[A, B](tree: Tree[A, B], to: Node[A, B],
      from: Node[A, B]): Unit = {
    if (to.isRoot)
      tree.root = from
    else if (to.isLeftChild)
      to.parent.left = from
    else
      to.parent.right = from

    if (from ne null)
      from.parent = to.parent
  }

  // ---- iterators ----

  def iterator[A, B](tree: Tree[A, B]): Iterator[Map.Entry[A, B]] =
    new EntriesIterator(tree)

  def keysIterator[A, B](tree: Tree[A, B]): Iterator[A] =
    new KeysIterator(tree)

  def valuesIterator[A, B](tree: Tree[A, B]): Iterator[B] =
    new ValuesIterator(tree)

  private[this] abstract class AbstractTreeIterator[A, B, R](tree: Tree[A, B],
      private[this] var nextNode: Node[A, B])
      extends Iterator[R] {

    private[this] var lastNode: Node[A, B] = _ // null

    protected def advance(node: Node[A, B]): Node[A, B]
    protected def nextResult(node: Node[A, B]): R

    def hasNext(): Boolean = nextNode ne null

    def next(): R = {
      val node = nextNode
      if (node eq null)
        throw new NoSuchElementException("next on empty iterator")
      lastNode = node
      nextNode = advance(node)
      nextResult(node)
    }

    override def remove(): Unit = {
      val node = lastNode
      if (node eq null)
        throw new IllegalStateException()
      deleteNode(tree, node)
      lastNode = null
    }
  }

  private[this] abstract class TreeIterator[A, B, R](tree: Tree[A, B])
      extends AbstractTreeIterator[A, B, R](tree, minNode(tree)) {

    protected final def advance(node: Node[A, B]): Node[A, B] =
      successor(node)
  }

  private[this] final class EntriesIterator[A, B](tree: Tree[A, B])
      extends TreeIterator[A, B, Map.Entry[A, B]](tree) {

    protected def nextResult(node: Node[A, B]): Map.Entry[A, B] = node
  }

  private[this] final class KeysIterator[A, B](tree: Tree[A, B])
      extends TreeIterator[A, B, A](tree) {

    protected def nextResult(node: Node[A, B]): A = node.key
  }

  private[this] final class ValuesIterator[A, B](tree: Tree[A, B])
      extends TreeIterator[A, B, B](tree) {

    protected def nextResult(node: Node[A, B]): B = node.value
  }

  // ---- projection iterators ----

  def projectionIterator[A, B](tree: Tree[A, B],
      start: A, startKind: BoundKind, end: A, endKind: BoundKind)(
      implicit comp: Comparator[_ >: A]): Iterator[Map.Entry[A, B]] = {
    new ProjectionEntriesIterator(tree, start, startKind, end, endKind)
  }

  def projectionKeysIterator[A, B](tree: Tree[A, B],
      start: A, startKind: BoundKind, end: A, endKind: BoundKind)(
      implicit comp: Comparator[_ >: A]): Iterator[A] = {
    new ProjectionKeysIterator(tree, start, startKind, end, endKind)
  }

  def projectionValuesIterator[A, B](tree: Tree[A, B],
      start: A, startKind: BoundKind, end: A, endKind: BoundKind)(
      implicit comp: Comparator[_ >: A]): Iterator[B] = {
    new ProjectionValuesIterator(tree, start, startKind, end, endKind)
  }

  private[this] abstract class ProjectionIterator[A, B, R](tree: Tree[A, B],
      start: A, startKind: BoundKind, end: A, endKind: BoundKind)(
      implicit comp: Comparator[_ >: A])
      extends AbstractTreeIterator[A, B, R](
          tree,
          ProjectionIterator.nullIfAfterEnd(
              minNodeAfter(tree.root, start, startKind), end, endKind)) {

    protected final def advance(node: Node[A, B]): Node[A, B] =
      ProjectionIterator.nullIfAfterEnd(successor(node), end, endKind)
  }

  private[this] object ProjectionIterator {
    @inline
    private def nullIfAfterEnd[A, B](node: Node[A, B], end: A,
        endKind: BoundKind)(implicit comp: Comparator[_ >: A]): Node[A, B] = {
      if (endKind != NoBound && (node ne null) &&
          !isWithinUpperBound(node.key, end, endKind)) {
        null
      } else {
        node
      }
    }
  }

  private[this] final class ProjectionEntriesIterator[A, B](tree: Tree[A, B],
      start: A, startKind: BoundKind, end: A, endKind: BoundKind)(
      implicit comp: Comparator[_ >: A])
      extends ProjectionIterator[A, B, Map.Entry[A, B]](tree, start, startKind, end, endKind) {

    def nextResult(node: Node[A, B]): Map.Entry[A, B] = node
  }

  private[this] final class ProjectionKeysIterator[A, B](tree: Tree[A, B],
      start: A, startKind: BoundKind, end: A, endKind: BoundKind)(
      implicit comp: Comparator[_ >: A])
      extends ProjectionIterator[A, B, A](tree, start, startKind, end, endKind) {

    def nextResult(node: Node[A, B]): A = node.key
  }

  private[this] final class ProjectionValuesIterator[A, B](tree: Tree[A, B],
      start: A, startKind: BoundKind, end: A, endKind: BoundKind)(
      implicit comp: Comparator[_ >: A])
      extends ProjectionIterator[A, B, B](tree, start, startKind, end, endKind) {

    def nextResult(node: Node[A, B]): B = node.value
  }

  // ---- descending iterators ----

  /* We do not have optimized iterators for descending order on
   * non-projections, as they would be of questionable value. Instead, we use
   * descending project iterators instead.
   *
   * Since we know that both bounds do not exist, we know that the comparator
   * will never be used by the algorithms in DescendingTreeIterator, so we do
   * not require one and instead push a `null` internally.
   */

  def descendingIterator[A, B](tree: Tree[A, B]): Iterator[Map.Entry[A, B]] = {
    descendingIterator(tree, null.asInstanceOf[A], NoBound,
        null.asInstanceOf[A], NoBound)(null)
  }

  def descendingKeysIterator[A, B](tree: Tree[A, B]): Iterator[A] = {
    descendingKeysIterator(tree, null.asInstanceOf[A], NoBound,
        null.asInstanceOf[A], NoBound)(null)
  }

  def descendingValuesIterator[A, B](tree: Tree[A, B]): Iterator[B] = {
    descendingValuesIterator(tree, null.asInstanceOf[A], NoBound,
        null.asInstanceOf[A], NoBound)(null)
  }

  // ---- descending projection iterators ----

  def descendingIterator[A, B](tree: Tree[A, B],
      start: A, startKind: BoundKind, end: A, endKind: BoundKind)(
      implicit comp: Comparator[_ >: A]): Iterator[Map.Entry[A, B]] = {
    new DescendingEntriesIterator(tree, start, startKind, end, endKind)
  }

  def descendingKeysIterator[A, B](tree: Tree[A, B],
      start: A, startKind: BoundKind, end: A, endKind: BoundKind)(
      implicit comp: Comparator[_ >: A]): Iterator[A] = {
    new DescendingKeysIterator(tree, start, startKind, end, endKind)
  }

  def descendingValuesIterator[A, B](tree: Tree[A, B],
      start: A, startKind: BoundKind, end: A, endKind: BoundKind)(
      implicit comp: Comparator[_ >: A]): Iterator[B] = {
    new DescendingValuesIterator(tree, start, startKind, end, endKind)
  }

  private[this] abstract class DescendingTreeIterator[A, B, R](tree: Tree[A, B],
      start: A, startKind: BoundKind, end: A, endKind: BoundKind)(
      implicit comp: Comparator[_ >: A])
      extends AbstractTreeIterator[A, B, R](
          tree,
          DescendingTreeIterator.nullIfBeforeEnd(
              maxNodeBefore(tree.root, start, startKind), end, endKind)) {

    protected final def advance(node: Node[A, B]): Node[A, B] =
      DescendingTreeIterator.nullIfBeforeEnd(predecessor(node), end, endKind)
  }

  private[this] object DescendingTreeIterator {
    @inline
    private def nullIfBeforeEnd[A, B](node: Node[A, B], end: A,
        endKind: BoundKind)(implicit comp: Comparator[_ >: A]): Node[A, B] = {
      if (endKind != NoBound && (node ne null) &&
          !isWithinLowerBound(node.key, end, endKind)) {
        null
      } else {
        node
      }
    }
  }

  private[this] final class DescendingEntriesIterator[A, B](tree: Tree[A, B],
      start: A, startKind: BoundKind, end: A, endKind: BoundKind)(
      implicit comp: Comparator[_ >: A])
      extends DescendingTreeIterator[A, B, Map.Entry[A, B]](tree, start, startKind, end, endKind) {

    def nextResult(node: Node[A, B]): Map.Entry[A, B] = node
  }

  private[this] final class DescendingKeysIterator[A, B](tree: Tree[A, B],
      start: A, startKind: BoundKind, end: A, endKind: BoundKind)(
      implicit comp: Comparator[_ >: A])
      extends DescendingTreeIterator[A, B, A](tree, start, startKind, end, endKind) {

    def nextResult(node: Node[A, B]): A = node.key
  }

  private[this] final class DescendingValuesIterator[A, B](tree: Tree[A, B],
      start: A, startKind: BoundKind, end: A, endKind: BoundKind)(
      implicit comp: Comparator[_ >: A])
      extends DescendingTreeIterator[A, B, B](tree, start, startKind, end, endKind) {

    def nextResult(node: Node[A, B]): B = node.value
  }

  // building

  /** Common implementation of `fromOrderedKeys` and `fromOrderedEntries`. */
  @noinline
  def fromOrdered[A, B, C](xs: Iterator[A], size: Int,
      keyOf: js.Function1[A, B], valueOf: js.Function1[A, C]): Tree[B, C] = {
    // maximum depth of non-leaf nodes == floor(log2(size))
    val maxUsedDepth = 32 - Integer.numberOfLeadingZeros(size)

    @noinline
    def createSubTree(level: Int, size: Int): Node[B, C] = size match {
      case 0 =>
        null
      case 1 =>
        /* Nodes on the last level must be red, because the last level might
         * not be full. If they were black, then the paths from the root to the
         * last level would have 1 more black node than those ending at the
         * second-to-last level.
         * Exception: when the only node is the root, because the root must be
         * black.
         */
        val item = xs.next()
        val red = level == maxUsedDepth && level != 1
        Node.leaf(keyOf(item), valueOf(item), red, null)
      case _ =>
        val leftSize = (size - 1) >> 1 // opt: (size - 1) / 2 with size > 0
        val left = createSubTree(level + 1, leftSize)
        val item = xs.next()
        val right = createSubTree(level + 1, size - 1 - leftSize)
        val node = Node(keyOf(item), valueOf(item), false, left, right, null)
        if (left ne null)
          left.parent = node
        right.parent = node
        node
    }

    new Tree(createSubTree(1, size), size)
  }

  /** Build a Tree suitable for a TreeSet from an ordered sequence of keys.
   *
   *  All values will be `()`.
   */
  def fromOrderedKeys[A](xs: Iterator[A], size: Int): Tree[A, Any] =
    fromOrdered(xs, size, (x: A) => x, (_: A) => ())

  /** Build a Tree suitable for a TreeMap from an ordered sequence of key/value
   *  pairs.
   */
  def fromOrderedEntries[A, B](xs: Iterator[Map.Entry[A, B]], size: Int): Tree[A, B] =
    fromOrdered(xs, size, (x: Map.Entry[A, B]) => x.getKey(), (x: Map.Entry[A, B]) => x.getValue())

  private def copyTree[A, B](n: Node[A, B]): Node[A, B] = {
    if (n eq null) {
      null
    } else {
      val c = Node(n.key, n.value, n.red, copyTree(n.left), copyTree(n.right), null)
      if (c.left != null)
        c.left.parent = c
      if (c.right != null)
        c.right.parent = c
      c
    }
  }
}
