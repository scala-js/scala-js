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

package java.util.regex

import scala.annotation.{tailrec, switch}

import scala.scalajs.js

/** The goal of a `GroupStartMap` is to retrieve the start position of each
 *  group of a matching regular expression where only the strings of the
 *  matched groups are known.
 *  For that, we use the following observation:
 *  If the regex /A(B)\1/ matches a string at a given index,
 *  then         /(A)(B)\2/ matches the same string at the same index.
 *  However, in the second regex, we can use the length of the first group (A)
 *  to retrieve the start position of the second group (B).
 *  Note that the back-references in the second regex are shifted, but this
 *  does not change the matched strings.
 *
 *  Implementation details:
 *  - It parses the regular expression into a tree of type `Node` that contains:
 *    + Either modifiers (e.g. +, *, (?=....)), or an original group number
 *    + Either a raw regular expression, or a list of children Node
 *  - It converts this Node to a regex string, such that every sub-part of the
 *    regex which was not yet in a group now belongs to a group
 *  - The new regex matches the original string at the original position
 *  - It propagates the matched strings of all groups into the Node
 *  - It computes the start of every group thanks to the groups before it
 *  - It builds and returns the mapping of previous group number -> start
 *
 *  @author Mikaël Mayer
 */
private[regex] object GroupStartMap {
  def apply(string: String, start: Int, pattern: Pattern): Int => Int =
    new GroupStartMap(string, start, pattern).mapping

  /** Parentheses aspect of the regex. */
  private sealed trait OriginalRegex {
    @inline
    def fold[A](numberF: Int => A, leftRightF: (String, String) => A): A = {
      this match {
        case OriginallyGroupped(number)     => numberF(number)
        case OriginallyWrapped(left, right) => leftRightF(left, right)
      }
    }
  }

  /** The regex had originally a group number.
   *
   *  That is, with simple parentheses non-grouping parentheses (?: ...) are
   *  always added.
   */
  private final case class OriginallyGroupped(number: Int) extends OriginalRegex

  /** The regex was not originally grouped with parentheses.
   *
   *  In that case, `left` and `right` are modifiers (e.g., `*`, `+`, `*?`,
   *  `+?`, `?`, `(?= ...)`, `(?! ...)`).
   */
  private final case class OriginallyWrapped(left: String, right: String)
      extends OriginalRegex

  private object OriginallyWrapped {
    def isRepeatModifier(modifier: String): Boolean = {
      modifier == "?" || modifier == "??" || modifier == "*" ||
      modifier == "+" || modifier == "*?" || modifier == "+?" ||
      modifier.startsWith("{")
    }

    object Repeater {
      def unapply(e: OriginallyWrapped): Option[String] =
        if (e.left == "" && isRepeatModifier(e.right)) Some(e.right)
        else None
    }

    object Absolute {
      def unapply(e: OriginallyWrapped): Boolean =
        (e.left == "(?!" || e.left == "(?=") && e.right == ")"
    }
  }

  /** Type of the node, a regex or a sequence of nodes. */
  private sealed trait NodeType {
    @inline
    def fold[A](regexF: String => A, childrenF: Seq[Node] => A,
        childrenD: Seq[Node] => A): A = {
      this match {
        case RegexLeaf(regex)       => regexF(regex)
        case ParentNode(children)   => childrenF(children)
        case DisjunctNode(children) => childrenD(children)
      }
    }
  }

  /** A leaf regex, without any subgroups/ */
  private final case class RegexLeaf(regex: String) extends NodeType

  /** A parent grouping, with children nodes.
   *
   *  Modifiers to this group, such as star or plus, can be added using the
   *  field of `Node` `originalGroup`.
   */
  private final case class ParentNode(children: Seq[Node]) extends NodeType

  /** A parent grouping with children nodes which are in a disjunction. */
  private final case class DisjunctNode(children: Seq[Node]) extends NodeType

  /** Creates a ParentNode, but in the presence of disjunctions, split at the
   *  disjunctions to create a DisjunctNode.
   */
  private object CreateParentNode {
    @tailrec
    private def splitWhereAux[A](in: List[A], splitter: A => Boolean,
        res: List[List[A]], acc: List[A]): List[List[A]] = {
      in match {
        case Nil =>
          (acc.reverse :: res).reverse
        case head :: tail =>
          if (splitter(head))
            splitWhereAux(tail, splitter, acc.reverse :: res, Nil)
          else
            splitWhereAux(tail, splitter, res, head :: acc)
      }
    }

    def splitWhere[A](in: List[A], splitter: A => Boolean): List[List[A]] =
      splitWhereAux(in, splitter, Nil, Nil)

    def apply(children: Seq[Node]): NodeType = {
      val disjuncts = splitWhere(children.toList,
          (c: Node) => c.nodeType == RegexLeaf("|"))
      if (disjuncts.length == 1)
        ParentNode(children)
      else
        DisjunctNode(disjuncts.map(Node(_)))
    }
  }

  private def isBackReference(r: String): Boolean =
    r.length >= 2 && r(0) == '\\' && r.tail.forall(_.isDigit)

  private object BackReferenceLeaf {
    def unapply(leaf: RegexLeaf): Option[Int] = {
      val r = leaf.regex
      if (isBackReference(r)) Some(r.substring(1).toInt)
      else None
    }
  }

  private object Node {
    def apply(originalGroup: OriginalRegex, nodeType: NodeType): Node =
      new Node(originalGroup, nodeType)

    def apply(regex: String): Node =
      new Node(OriginallyWrapped("", ""), RegexLeaf(regex))

    def apply(nodes: Seq[Node]): Node =
      new Node(OriginallyWrapped("", ""), CreateParentNode(nodes))

    def disjunct(nodes: Seq[Node]): Node =
      new Node(OriginallyWrapped("", ""), DisjunctNode(nodes))

    def unapply(n: Node): Option[(OriginalRegex, NodeType)] =
      Some((n.originalGroup, n.nodeType))
  }

  private object UnwrappedRegexLeaf {
    def unapply(n: Node): Option[String] = n match {
      case Node(OriginallyWrapped("", ""), RegexLeaf(r)) => Some(r)
      case _                                             => None
    }
  }

  /** Node of the regex tree.
   *
   *  @param originalGroup
   *    Either the 1-based index of the original group that this node encloses,
   *    or the modifiers which are present before and after (ex: `?`, `*?`,
   *    `(?=...)`, `(?!..)`).
   *  @param nodeType
   *    The type of the node (Regexleaf, or ParentNode).
   *    This is a var because we need to shift the backreference groups.
   */
  private final class Node(val originalGroup: OriginalRegex,
      var nodeType: NodeType) {

    var newGroup: Int = 0    // Assigned later after the tree of nodes is built
    var matched: String = "" // Assigned later after the new regexp matches
    var start: Int = 0       // Assigned later after recovering the tree matches

    override def toString(): String =
      "Node(" + originalGroup + ", " + nodeType + ")"

    def transformGroupNumber(mapping: Map[Int, Int]): this.type = {
      nodeType match {
        case BackReferenceLeaf(reference) =>
          nodeType = RegexLeaf("\\" + mapping.get(reference).getOrElse(0))
        case RegexLeaf(regex) =>
          // do nothing
        case ParentNode(children) =>
          children.foreach(_.transformGroupNumber(mapping))
        case DisjunctNode(children) =>
          children.foreach(_.transformGroupNumber(mapping))
      }
      this
    }

    /** Assigns consecutive group numbers starting from newGroupIndex to the
     *  nodes in this subtree, in a pre-order walk.
     *
     *  @return 1 plus the largest assigned group number.
     */
    def setNewGroup(index: Int): Int = {
      newGroup = index
      nodeType.fold(
          r => index + 1,
          children => children.foldLeft(index + 1) {
            case (newIndex, child) => child.setNewGroup(newIndex)
          },
          children => children.foldLeft(index + 1) {
            case (newIndex, child) => child.setNewGroup(newIndex)
          }
      )
    }

    /** Recursively matched groups from a match of this regular expression to
     *  the field matched of each node.
     *
     *  If a group did not match, stringForGroupNumber can return null.
     *  This is consistent with JVM regexs.
     */
    def setMatch(stringForGroupNumber: Int => String): Unit = {
      this.matched = stringForGroupNumber(newGroup)
      nodeType.fold(
          r => (),
          children => children.foreach(_.setMatch(stringForGroupNumber)),
          children => children.foreach(_.setMatch(stringForGroupNumber))
      )
    }

    /* When assigning group positions. I could not choose between assigning
     * group numbers from left to right or from right to left, because there
     * both failed in one case each. Normally, both directions give the same
     * result. But there are corner cases.
     *
     * Consider the following regex matching `abbbbbbc`
     *
     *     (?=ab*(c))ab
     *
     * After conversion, this becomes:
     *
     *     (?=(ab*)(c))(ab)
     *
     * To recover the position of the group (c), we cannot do anything but
     * compute it from the length of (ab*), that is, propagate the start,
     * compute the length, and return the end, and this, recursively. This is
     * what we need to do in a forward-matching regex.
     *
     * However, consider the following regex matching `abcbdbe`
     *
     *     a(b.)*
     *
     * After conversion, it is transformed to:
     *
     *     (a)((b.)*)
     *
     * The semantics of group matching under a star are that the last match is
     * kept. Therefore, we cannot obtain the start position of (b.) from
     * propagating lengths from left to right. What we first do is to get the
     * start, then the end, of the group `((b.)*)`, and then we propagate this
     * end to the inner group.
     *
     * Note that when javascript will support back-matching `(?<= )` and
     * `(?<! )` (hopefully one day), we will be able to implement the length
     * transfer using the `setEndReturnStart` method, because we have no clue
     * on where the match started (this is different from the `start` position
     * because it can extend before it).
     *
     * `Absolute()` means a marker of the type `(?= )` or `(?! )`, they do not
     * count to propagate the length on the right or on the left.
     *
     * `Repeater()` designates all the repeat-transformer which have more or
     * less the semantics of `*`. Every group having a repeater uses the
     * semantics of propagating the end to the start.
     */

    /** Propagates the start position of this node to its descendants. */
    def propagateStart(): Unit = {
      nodeType.fold(
          regex => (),
          children => children.foldLeft(start) {
            case (newStart, child) => child.setStartReturnEnd(newStart)
          },
          children => children.foreach {
            case child => child.setStartReturnEnd(start)
          }
      )
    }

    /** Propagates the end position of this node to its descendants. */
    def propagateEnd(): Unit = {
      nodeType.fold(
          regex => (),
          children => children.foldRight(end) {
            case (child, newEnd) => child.setEndReturnStart(newEnd)
          },
          children => children.foreach {
            case child => child.setEndReturnStart(end)
          }
      )
    }

    def setEndReturnStart(newEnd: Int): Int = {
      start = originalGroup match {
        case OriginallyWrapped.Absolute() =>
          newEnd
        case _ =>
          if (matched == null) -1
          else newEnd - matched.length
      }
      propagateStart()
      start
    }

    def setStartReturnEnd(offset: Int): Int = {
      start =
        if (matched == null) -1
        else offset

      originalGroup match {
        case OriginallyWrapped.Repeater(_) => propagateEnd()
        case _                             => propagateStart()
      }
      originalGroup match {
        case OriginallyWrapped.Absolute() => offset
        case _                            => end
      }
    }

    def end: Int = start + (if (matched == null) 0 else matched.length)

    def buildRegex: String = {
      val leftRegex = originalGroup.fold(
          groupNum => "(",
          (left: String, right: String) => "((?:" + left
      )
      val middleRegex = nodeType.fold(
          regex => regex,
          children => "(?:" + children.map(_.buildRegex).mkString + ")",
          children => "(?:" + children.map(_.buildRegex).mkString("|") + ")"
      )
      val rightRegex = originalGroup.fold(
          groupNum => ")",
          (left: String, right: String) => ")" + right + ")"
      )
      leftRegex + middleRegex + rightRegex
    }

    def getGroupNodeMap: Map[Int, Node] = {
      val thisGroupNodeMap = originalGroup.fold(
          groupNum => Map(groupNum -> this),
          (_, _) => Map[Int, Node]()
      )
      val childGroupNodeMap = nodeType.fold(
          regex => Map[Int, Node](),
          children => children.foldLeft(Map[Int, Node]()) {
            case (mapping, child) => mapping ++ child.getGroupNodeMap
          },
          children => children.foldLeft(Map[Int, Node]()) {
            case (mapping, child) => mapping ++ child.getGroupNodeMap
          }
      )
      thisGroupNodeMap ++ childGroupNodeMap
    }

    def simplify: Node = {
      this match {
        case Node(OriginallyGroupped(nextGroupIndex),
            ParentNode(Seq(UnwrappedRegexLeaf(regex)))) =>
          Node(OriginallyGroupped(nextGroupIndex), RegexLeaf(regex))
        case _ =>
          this
      }
    }
  }
}

private[regex] class GroupStartMap(string: String, start: Int, pattern: Pattern) {
  import GroupStartMap._
  import Pattern.{CASE_INSENSITIVE, MULTILINE}

  val mapping: Int => Int = {
    val node = parseRegex(pattern.jsPattern)
    val flags = pattern.jsFlags
    node.setNewGroup(1)
    val groupNodeMap = node.getGroupNodeMap
    node.transformGroupNumber(groupNodeMap.map(kv => (kv._1, kv._2.newGroup)))
    val allMatchingRegexStr = node.buildRegex
    val allMatchingRegex = new js.RegExp(allMatchingRegexStr, flags)
    allMatchingRegex.lastIndex = start
    val allMatchResult = allMatchingRegex.exec(string)
    if (allMatchResult == null) {
      throw new Exception(
          s"[Internal error] Executed '$allMatchingRegex' on " +
          s"'$string' at position $start, got an error.\n" +
          s"Original pattern '${pattern}' did match however.")
    }
    node.setMatch((x: Int) => allMatchResult(x).getOrElse(null))
    node.setStartReturnEnd(start)
    groupNodeMap.map(kv => (kv._1, kv._2.start))
  }

  /** Wraps every consecutive chars and simple expressions in the regexp in a
   *  group to find the intermediate positions.
   *
   *  Same for subgroups.
   *
   *  Input example:  (?:AB)C
   *  Output example: A node roughly representing (?:(AB))(?:(C))
   *  Input example:  (A(B))C(D)
   *  Output example: A node roughly representing (((A)(B))(C)(D))
   */
  private def parseRegex(pattern: String): Node = {
    def parseClosingParenthesis(pIndex: Int) = pIndex + 1

    // Returns the position just after the next ending brace
    @tailrec def positionEndNextBrace(pIndex: Int): Int = {
      if (pattern.length <= pIndex) pIndex
      else if (pattern(pIndex) == '}') pIndex + 1
      else positionEndNextBrace(pIndex + 1)
    }

    // Returns the position just after the next ending square bracket
    @tailrec def positionEndSquareBracket(pIndex: Int): Int = {
      if (pattern.length <= pIndex)
        pIndex
      else if (pattern(pIndex) == '\\' && pattern.length > 1)
        positionEndSquareBracket(pIndex + 2)
      else if (pattern(pIndex) == ']')
        pIndex + 1
      else
        positionEndSquareBracket(pIndex + 1)
    }

    @tailrec def positionAfterLastDigit(pIndex: Int): Int = {
      if (pIndex < pattern.length && pattern(pIndex).isDigit)
        positionAfterLastDigit(pIndex + 1)
      else
        pIndex
    }

    /* Returns a sequence of nodes, the remaining non-parsed, and the next
     * group index.
     * - Takes care of escaped parentheses \( and \).
     * - Takes care of non-group parenthesis (?:).
     */
    def parse(pIndex: Int, nextGroupIndex: Int): (Seq[Node], Int, Int) = {
      if (pIndex >= pattern.length) {
        (Seq(), pIndex, nextGroupIndex)
      } else {
        def simplify(nodes: Seq[Node]): Seq[Node] = nodes match {
          case UnwrappedRegexLeaf(r1) +: UnwrappedRegexLeaf(r2) +: tail
              if !isBackReference(r1) && r1 != "|" && !isBackReference(r2) && r2 != "|" =>
            simplify(Node(r1 + r2) +: tail)

          case n +: Node(OriginallyWrapped.Repeater(modifier), RegexLeaf("")) +: tail =>
            n match {
              case Node(OriginallyWrapped(left, right), m) =>
                simplify(Node(OriginallyWrapped(left, right + modifier), m) +: tail)
              case n @ Node(OriginallyGroupped(_), m) =>
                simplify(Node(OriginallyWrapped("", modifier), ParentNode(Seq(n))) +: tail)
            }

          case _ =>
            nodes
        }

        def addSiblings(node: Node, remaining: Int,
            nextGroupIndex: Int): (Seq[Node], Int, Int) = {
          val (siblings, finalRemaining, finalGroupIndex) =
            parse(remaining, nextGroupIndex)
          (simplify(node +: siblings), finalRemaining, finalGroupIndex)
        }

        def default = {
          val e = pattern(pIndex)
          addSiblings(Node("" + e), pIndex + 1, nextGroupIndex)
        }

        (pattern(pIndex): @switch) match {
          case '(' =>
            if (pattern.length >= pIndex + 3 && pattern(pIndex + 1) == '?' &&
                (pattern(pIndex + 2) == '=' || pattern(pIndex + 2) == '!')) {
              // Non-capturing test group
              val (parsed, remaining, newNextGroupIndex) =
                parse(pIndex + 3, nextGroupIndex)
              val remaining1 = parseClosingParenthesis(remaining)
              addSiblings(
                  Node(OriginallyWrapped("(?" + pattern(pIndex + 2), ")"),
                      CreateParentNode(parsed)),
                  remaining1, newNextGroupIndex)
            } else if (pattern.length < pIndex + 3 ||
                pattern(pIndex + 1) != '?' ||pattern(pIndex + 2) != ':') {
              // Capturing group
              val (parsed, remaining, newNextGroupIndex) =
                parse(pIndex + 1, nextGroupIndex + 1)
              val remaining1 = parseClosingParenthesis(remaining)
              addSiblings(
                  Node(OriginallyGroupped(nextGroupIndex),
                      CreateParentNode(parsed)).simplify,
                  remaining1, newNextGroupIndex)
            } else if (pattern.length >= pIndex + 3 &&
                pattern(pIndex + 1) == '?' && pattern(pIndex + 2) == ':') {
              // Non-capturing group
              val (parsedNodes, remaining, newNextGroupIndex) =
                parse(pIndex + 3, nextGroupIndex)
              val remaining1 = parseClosingParenthesis(remaining)
              addSiblings(Node(parsedNodes).simplify, remaining1,
                  newNextGroupIndex)
            } else {
              // Should not happen
              default
            }

          case ')' =>
            (Seq(), pIndex, nextGroupIndex)

          case '\\' =>
            if (pattern.length >= pIndex + 2) {
              val nextIndex =
                if (pattern(pIndex + 1).isDigit) positionAfterLastDigit(pIndex + 1)
                else pIndex + 2
              val regexPart = pattern.substring(pIndex, nextIndex)
              addSiblings(Node(regexPart), nextIndex, nextGroupIndex)
            } else {
              // No escaped char, but this should not be called
              default
            }

          case '+' | '*' | '?' => // greedy or not greedy
            val nextIndex =
              if (pattern.length >= pIndex + 2 && pattern(pIndex + 1) == '?') pIndex + 2
              else pIndex + 1
            val repeater = pattern.substring(pIndex, nextIndex)
            addSiblings(
                Node(OriginallyWrapped("", repeater), RegexLeaf("")),
                nextIndex, nextGroupIndex)

          case '{' =>
            // parse until end of occurrence
            val nextIndex = positionEndNextBrace(pIndex + 1)
            val repeater = pattern.substring(pIndex, nextIndex)
            addSiblings(
                Node(OriginallyWrapped("", repeater), RegexLeaf("")),
                nextIndex, nextGroupIndex)

          case '[' =>
            val remaining = positionEndSquareBracket(pIndex + 1)
            val inside = pattern.substring(pIndex, remaining)
            addSiblings(Node(inside), remaining, nextGroupIndex)

          case _ =>
            default
        }
      }
    }

    parse(0, 1)._1 match {
      case Nil      => Node("")
      case Seq(n)   => n
      case children => Node(children)
    }
  }
}
