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

import java.util.HashMap

import scala.scalajs.js

/** The goal of a `GroupStartMapper` is to retrieve the start position of each
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
 *  - It parses the regular expression into a tree of type `Node`
 *  - It converts this Node to a regex string, such that every sub-part of the
 *    regex which was not yet in a group now belongs to a group
 *  - The new regex matches the original string at the original position
 *  - It propagates the matched strings of all groups into the Node
 *  - It computes the start of every group thanks to the groups before it
 *  - It builds and returns the mapping of previous group number -> start
 *
 *  @author Mikaël Mayer
 */
private[regex] class GroupStartMapper private (pattern: String, flags: String,
    node: GroupStartMapper.Node, groupCount: Int, allMatchingRegex: js.RegExp) {

  import GroupStartMapper._

  def apply(string: String, start: Int): js.Array[Int] = {
    allMatchingRegex.lastIndex = start
    val allMatchResult = allMatchingRegex.exec(string)
    if (allMatchResult == null) {
      throw new AssertionError(
          s"[Internal error] Executed '$allMatchingRegex' on " +
            s"'$string' at position $start, got an error.\n" +
            s"Original pattern '$pattern' with flags '$flags' did match however.")
    }

    // Prepare a `groupStartMap` array with the correct length filled with -1
    val len = groupCount + 1 // index 0 is not used
    val groupStartMap = new js.Array[Int](len)
    var i = 0
    while (i != len) {
      groupStartMap(i) = -1
      i += 1
    }

    node.propagateFromStart(allMatchResult, groupStartMap, start)

    groupStartMap
  }
}

private[regex] object GroupStartMapper {
  def apply(pattern: String, flags: String): GroupStartMapper = {
    val parser = new Parser(pattern)
    val node = parser.parseTopLevel()
    node.setNewGroup(1)
    val allMatchingRegex =
      new js.RegExp(node.buildRegex(parser.groupNodeMap), flags)
    new GroupStartMapper(pattern, flags, node, parser.parsedGroupCount, allMatchingRegex)
  }

  /** Node of the regex tree. */
  private abstract class Node {
    var newGroup: Int = _ // Assigned later after the tree of nodes is built

    /** Assigns consecutive group numbers starting from newGroupIndex to the
     *  nodes in this subtree, in a pre-order walk.
     *
     *  @return 1 plus the largest assigned group number.
     */
    def setNewGroup(newGroupIndex: Int): Int = {
      newGroup = newGroupIndex
      newGroupIndex + 1
    }

    def buildRegex(groupNodeMap: js.Array[Node]): String

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
     * Note that when JavaScript will support back-matching `(?<= )` and
     * `(?<! )` (hopefully one day), we will be able to implement the length
     * transfer using the `propagateFromEnd` method, because we have no clue on
     * where the match started (this is different from the `start` position
     * because it can extend before it).
     *
     * Zero-length test nodes of the type `(?= )` or `(?! )` do not
     * count to propagate the length on the right or on the left.
     *
     * `RepeatedNode`s use the semantics of propagating the end to the start.
     */

    /** Propagates the start or end position of this node to its descendants.
     *
     *  According to the big comment above, `RepeatedNode`s propagate the
     *  `end`, while other nodes propagate the `start`.
     */
    def propagate(matchResult: js.RegExp.ExecResult, groupStartMap: js.Array[Int], start: Int,
        end: Int): Unit

    /** Propagates the appropriate positions to the descendants of this node
     *  from its end position.
     *
     *  @return the start position of this node
     */
    final def propagateFromEnd(matchResult: js.RegExp.ExecResult, groupStartMap: js.Array[Int],
        end: Int): Int = {

      val start = matchResult(newGroup).fold(-1)(matched => end - matched.length)
      propagate(matchResult, groupStartMap, start, end)
      start
    }

    /** Propagates the appropriate positions to the descendants of this node
     *  from its start position.
     *
     *  @return the end position of this node
     */
    final def propagateFromStart(matchResult: js.RegExp.ExecResult, groupStartMap: js.Array[Int],
        start: Int): Int = {

      val end = matchResult(newGroup).fold(-1)(matched => start + matched.length)
      propagate(matchResult, groupStartMap, start, end)
      end
    }
  }

  /** A numbered group. */
  private final class GroupNode(val number: Int, val inner: Node) extends Node {
    override def setNewGroup(newGroupIndex: Int): Int =
      inner.setNewGroup(super.setNewGroup(newGroupIndex))

    def buildRegex(groupNodeMap: js.Array[Node]): String =
      "(" + inner.buildRegex(groupNodeMap) + ")"

    def propagate(matchResult: js.RegExp.ExecResult, groupStartMap: js.Array[Int], start: Int,
        end: Int): Unit = {
      /* #3901: A GroupNode within a negative look-ahead node may receive
       * `start != -1` from above, yet not match anything itself. We must
       * always keep the default `-1` if this group node does not match
       * anything.
       */
      if (matchResult(newGroup).isDefined)
        groupStartMap(number) = start
      inner.propagateFromStart(matchResult, groupStartMap, start)
    }
  }

  /** A zero-length test of the form `(?= )` or `(?! )`. */
  private final class ZeroLengthTestNode(val indicator: String, val inner: Node) extends Node {

    override def setNewGroup(newGroupIndex: Int): Int =
      inner.setNewGroup(super.setNewGroup(newGroupIndex))

    def buildRegex(groupNodeMap: js.Array[Node]): String =
      "((" + indicator + inner.buildRegex(groupNodeMap) + "))"

    def propagate(matchResult: js.RegExp.ExecResult, groupStartMap: js.Array[Int], start: Int,
        end: Int): Unit = {
      inner.propagateFromStart(matchResult, groupStartMap, start)
    }
  }

  /** A repeated node. */
  private final class RepeatedNode(val inner: Node, val repeater: String) extends Node {

    override def setNewGroup(newGroupIndex: Int): Int =
      inner.setNewGroup(super.setNewGroup(newGroupIndex))

    def buildRegex(groupNodeMap: js.Array[Node]): String =
      "(" + inner.buildRegex(groupNodeMap) + repeater + ")"

    def propagate(matchResult: js.RegExp.ExecResult, groupStartMap: js.Array[Int], start: Int,
        end: Int): Unit = {
      inner.propagateFromEnd(matchResult, groupStartMap, end)
    }
  }

  /** A leaf regex, without any subgroups. */
  private final class LeafRegexNode(val regex: String) extends Node {
    def buildRegex(groupNodeMap: js.Array[Node]): String =
      "(" + regex + ")"

    def propagate(matchResult: js.RegExp.ExecResult, groupStartMap: js.Array[Int], start: Int,
        end: Int): Unit = {
      // nothing to do
    }
  }

  /** A back reference. */
  private final class BackReferenceNode(val groupNumber: Int) extends Node {
    def buildRegex(groupNodeMap: js.Array[Node]): String = {
      val newGroupNumber =
        if (groupNumber >= groupNodeMap.length) 0
        else groupNodeMap(groupNumber).newGroup
      "(\\" + newGroupNumber + ")"
    }

    def propagate(matchResult: js.RegExp.ExecResult, groupStartMap: js.Array[Int], start: Int,
        end: Int): Unit = {
      // nothing to do
    }
  }

  /** A sequence of consecutive nodes. */
  private final class SequenceNode(val sequence: js.Array[Node]) extends Node {
    override def setNewGroup(newGroupIndex: Int): Int = {
      var nextIndex = super.setNewGroup(newGroupIndex)
      val len = sequence.length
      var i = 0
      while (i != len) {
        nextIndex = sequence(i).setNewGroup(nextIndex)
        i += 1
      }
      nextIndex
    }

    def buildRegex(groupNodeMap: js.Array[Node]): String = {
      var result = "("
      val len = sequence.length
      var i = 0
      while (i != len) {
        result += sequence(i).buildRegex(groupNodeMap)
        i += 1
      }
      result + ")"
    }

    def propagate(matchResult: js.RegExp.ExecResult, groupStartMap: js.Array[Int], start: Int,
        end: Int): Unit = {
      val len = sequence.length
      var i = 0
      var nextStart = start
      while (i != len) {
        nextStart = sequence(i).propagateFromStart(matchResult, groupStartMap, nextStart)
        i += 1
      }
    }
  }

  /** An alternatives node such as `ab|cd`. */
  private final class AlternativesNode(val alternatives: js.Array[Node]) extends Node {

    override def setNewGroup(newGroupIndex: Int): Int = {
      var nextIndex = super.setNewGroup(newGroupIndex)
      val len = alternatives.length
      var i = 0
      while (i != len) {
        nextIndex = alternatives(i).setNewGroup(nextIndex)
        i += 1
      }
      nextIndex
    }

    def buildRegex(groupNodeMap: js.Array[Node]): String = {
      var result = "("
      val len = alternatives.length
      var i = 0
      while (i != len) {
        if (i != 0)
          result += "|"
        result += alternatives(i).buildRegex(groupNodeMap)
        i += 1
      }
      result + ")"
    }

    def propagate(matchResult: js.RegExp.ExecResult, groupStartMap: js.Array[Int], start: Int,
        end: Int): Unit = {
      val len = alternatives.length
      var i = 0
      while (i != len) {
        alternatives(i).propagateFromStart(matchResult, groupStartMap, start)
        i += 1
      }
    }
  }

  private final class Parser(pattern0: String) {
    /* Use a null-terminated string so that we don't have to check
     * `pIndex < pattern.length` all the time.
     */
    private[this] val pattern: String = pattern0 + ')'

    private[this] var pIndex: Int = 0

    val groupNodeMap = js.Array[Node](null) // index 0 is not used

    def parsedGroupCount: Int = groupNodeMap.length - 1

    def parseTopLevel(): Node =
      parseInsideParensAndClosingParen()

    private def parseInsideParensAndClosingParen(): Node = {
      // scalastyle:off return
      val alternatives = js.Array[Node]() // completed alternatives
      var sequence = js.Array[Node]() // current sequence

      // Explicitly take the sequence, otherwise we capture a `var`
      def completeSequence(sequence: js.Array[Node]): Node = {
        sequence.length match {
          case 0 => new LeafRegexNode("")
          case 1 => sequence(0)
          case _ => new SequenceNode(sequence)
        }
      }

      while (true) {
        val baseNode = (pattern.charAt(pIndex): @switch) match {
          case '|' =>
            // Complete one alternative
            alternatives.push(completeSequence(sequence))
            sequence = js.Array[Node]()
            pIndex += 1
            null

          case ')' =>
            // Complete the last alternative
            pIndex += 1 // go past the closing paren
            val lastAlternative = completeSequence(sequence)
            if (alternatives.length == 0) {
              return lastAlternative
            } else {
              alternatives.push(lastAlternative)
              return new AlternativesNode(alternatives)
            }

          case '(' =>
            val indicator = pattern.substring(pIndex + 1, pIndex + 3)
            if (indicator == "?=" || indicator == "?!") {
              // Non-capturing test group
              pIndex += 3
              val inner = parseInsideParensAndClosingParen()
              new ZeroLengthTestNode(indicator, inner)
            } else if (indicator == "?:") {
              // Non-capturing group
              pIndex += 3
              val inner = parseInsideParensAndClosingParen()
              inner
            } else {
              // Capturing group
              pIndex += 1
              val groupIndex = groupNodeMap.length
              groupNodeMap.push(null) // reserve slot before parsing inner
              val inner = parseInsideParensAndClosingParen()
              val groupNode = new GroupNode(groupIndex, inner)
              groupNodeMap(groupIndex) = groupNode
              groupNode
            }

          case '\\' =>
            @inline
            def isDigit(c: Char): Boolean = c >= '0' && c <= '9'

            if (isDigit(pattern.charAt(pIndex + 1))) {
              // it is a back reference; parse all following digits
              val startIndex = pIndex
              pIndex += 2
              while (isDigit(pattern.charAt(pIndex)))
                pIndex += 1
              new BackReferenceNode(Integer.parseInt(pattern.substring(startIndex + 1, pIndex)))
            } else {
              // it is a character escape
              val e = pattern.substring(pIndex, pIndex + 2)
              pIndex += 2
              new LeafRegexNode(e)
            }

          case '[' =>
            // parse until the corresponding ']'
            @tailrec def loop(pIndex: Int): Int = {
              pattern.charAt(pIndex) match {
                case '\\' => loop(pIndex + 2)
                case ']'  => pIndex + 1
                case _    => loop(pIndex + 1)
              }
            }

            val startIndex = pIndex
            pIndex = loop(startIndex + 1)
            val regex = pattern.substring(startIndex, pIndex)
            new LeafRegexNode(regex)

          case _ =>
            val e = pattern.substring(pIndex, pIndex + 1)
            pIndex += 1
            new LeafRegexNode(e)
        }

        if (baseNode ne null) { // null if we just completed an alternative
          (pattern.charAt(pIndex): @switch) match {
            case '+' | '*' | '?' =>
              val startIndex = pIndex
              if (pattern.charAt(startIndex + 1) == '?') // non-greedy mark
                pIndex += 2
              else
                pIndex += 1

              val repeater = pattern.substring(startIndex, pIndex)
              sequence.push(new RepeatedNode(baseNode, repeater))

            case '{' =>
              // parse until end of occurrence
              val startIndex = pIndex
              pIndex = pattern.indexOf("}", startIndex + 1) + 1
              if (pattern.charAt(pIndex) == '?') // non-greedy mark
                pIndex += 1
              val repeater = pattern.substring(startIndex, pIndex)
              sequence.push(new RepeatedNode(baseNode, repeater))

            case _ =>
              val sequenceLen = sequence.length
              if (sequenceLen != 0 && baseNode.isInstanceOf[LeafRegexNode] &&
                  sequence(sequenceLen - 1).isInstanceOf[LeafRegexNode]) {
                val fused = new LeafRegexNode(
                    sequence(sequenceLen - 1).asInstanceOf[LeafRegexNode].regex +
                      baseNode.asInstanceOf[LeafRegexNode].regex)
                sequence(sequenceLen - 1) = fused
              } else {
                sequence.push(baseNode)
              }
          }
        }
      }

      throw null // unreachable
      // scalastyle:on return
    }
  }
}
