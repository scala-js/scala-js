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

import java.lang.Utils._

import scala.scalajs.js
import scala.scalajs.js.JSStringOps._

import Pattern.IndicesArray

/** The goal of an `IndicesBuilder` is to retrieve the start and end positions
 *  of each group of a matching regular expression.
 *
 *  This is essentially a polyfill for the 'd' flag of `js.RegExp`, which is
 *  a Stage 4 proposal scheduled for inclusion in ECMAScript 2022. Without that
 *  flag, `js.RegExp` only provides the substrings matched by capturing groups,
 *  but not their positions. We implement the positions on top of that.
 *
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
 *  The `pattern` that is parsed by `IndicesBuilder` is the *compiled* JS
 *  pattern produced by `PatternCompiler`, not the original Java pattern. This
 *  means that we can simplify a number of things with the knowledge that:
 *
 *  - the pattern is well-formed,
 *  - it contains no named group or named back references, and
 *  - a '\' is always followed by an ASCII character that is:
 *    - a digit, for a back reference,
 *    - one of `^ $ \ . * + ? ( ) [ ] { } |`, for an escape,
 *    - 'b' or 'B' for a word boundary,
 *    - 'd' or 'D' for a digit character class (used in `[\d\D]` for any code point), or
 *    - 'p' or 'P' followed by a `{}`-enclosed name that contains only ASCII word characters.
 *
 *  @author Mikaël Mayer
 */
private[regex] class IndicesBuilder private (pattern: String, flags: String,
    node: IndicesBuilder.Node, groupCount: Int,
    jsRegExpForFind: js.RegExp, jsRegExpForMatches: js.RegExp) {

  import IndicesBuilder._

  def apply(forMatches: Boolean, string: String, index: Int): IndicesArray = {
    val regExp =
      if (forMatches) jsRegExpForMatches
      else jsRegExpForFind

    regExp.lastIndex = index
    val allMatchResult = regExp.exec(string)
    if (allMatchResult == null || allMatchResult.index != index) {
      throw new AssertionError(
          s"[Internal error] Executed '$regExp' on " +
          s"'$string' at position $index, got an error.\n" +
          s"Original pattern '$pattern' with flags '$flags' did match however.")
    }

    val start = index // by definition
    val end = start + undefOrForceGet(allMatchResult(0)).length()

    /* Initialize the `indices` array with:
     * - `[start, end]` at index 0, which represents the whole match, and
     * - `undefined` in the other slots.
     *
     * We explicitly store `undefined` in the other slots to prevent the array
     * from containing *empty* slots. That would make it a sparse array, which
     * is less efficient.
     */
    val len = groupCount + 1
    val indices = new IndicesArray(len)
    indices(0) = js.Array(start, end).asInstanceOf[js.Tuple2[Int, Int]]
    var i = 1
    while (i != len) {
      indices(i) = undefined
      i += 1
    }

    node.propagate(allMatchResult, indices, start, end)

    indices
  }
}

private[regex] object IndicesBuilder {
  def apply(pattern: String, flags: String): IndicesBuilder = {
    val parser = new Parser(pattern)
    val node = parser.parseTopLevel()
    node.setNewGroup(1)
    val allMatchingPattern = node.buildRegex(parser.groupNodeMap)
    val jsRegExpForFind = new js.RegExp(allMatchingPattern, flags + "g")
    val jsRegExpForMatches =
      new js.RegExp(Pattern.wrapJSPatternForMatches(allMatchingPattern), flags)
    new IndicesBuilder(pattern, flags, node, parser.parsedGroupCount,
        jsRegExpForFind, jsRegExpForMatches)
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

    /* The overall algorithm consists in, given known start and end positions
     * of a parent node, determine the positions of its children. This is done
     * in the main polymorphic method `propagate`, which each node implements.
     *
     * For some kinds of parent nodes, even when we know both their start and
     * end positions, we can only determine one side of their children.
     * Obvious examples are look-around nodes. Since they are zero-length,
     * their start and end are always equal, but correspond to different sides
     * of their children:
     *
     * - For look-ahead nodes (?=X) and (?!X), they correspond to the *start* of X.
     * - For look-behind nodes (?<=X) and (?<!X), they correspond to the *end* of X.
     *
     * Because of this, we have `propagateFromStart` and `propagateFromEnd`.
     * In either case, since we know the length of the child, we can compute
     * the position of the other side, which then allows to call `propagate`.
     *
     * In addition to look-around nodes, repeated nodes also have a constraint
     * on the side on which they operate. In X+ and X*, the groups inside X
     * correspond to the *last* iteration of the repetition. When we know the
     * start and end of X*, we only know the *end* of the last iteration of X.
     *
     * Sequence nodes can choose either direction. We choose to process their
     * children from start to end, and therefore thread the start of the
     * sequence through the children, using the computed end of each child as
     * the start of its next sibling.
     *
     * Other nodes such as alternatives or capturing groups have both the same
     * start and end position as their children, so they can directly call
     * `propagate`.
     */

    /** Propagates the start or end position of this node to its descendants.
     *
     *  According to the big comment above, `RepeatedNode`s propagate the
     *  `end`, while other nodes propagate the `start`.
     */
    def propagate(matchResult: js.RegExp.ExecResult,
        indices: IndicesArray, start: Int, end: Int): Unit

    /** Propagates the appropriate positions to the descendants of this node
     *  from its end position.
     */
    final def propagateFromEnd(matchResult: js.RegExp.ExecResult,
        indices: IndicesArray, end: Int): Unit = {

      val start = undefOrFold(matchResult(newGroup))(() => -1)(matched => end - matched.length)
      propagate(matchResult, indices, start, end)
    }

    /** Propagates the appropriate positions to the descendants of this node
     *  from its start position.
     *
     *  @return the end position of this node, as a convenience for `SequenceNode.propagate`
     */
    final def propagateFromStart(matchResult: js.RegExp.ExecResult,
        indices: IndicesArray, start: Int): Int = {

      val end = undefOrFold(matchResult(newGroup))(() => -1)(matched => start + matched.length)
      propagate(matchResult, indices, start, end)
      end
    }
  }

  /** A numbered group. */
  private final class GroupNode(val number: Int, val inner: Node) extends Node {
    override def setNewGroup(newGroupIndex: Int): Int =
      inner.setNewGroup(super.setNewGroup(newGroupIndex))

    def buildRegex(groupNodeMap: js.Array[Node]): String =
      "(" + inner.buildRegex(groupNodeMap) + ")"

    def propagate(matchResult: js.RegExp.ExecResult,
        indices: IndicesArray, start: Int, end: Int): Unit = {
      /* #3901: A GroupNode within a negative look-ahead node may receive
       * `start != -1` from above, yet not match anything itself. We must
       * always keep the default `-1` if this group node does not match
       * anything.
       */
      if (undefOrIsDefined(matchResult(newGroup)))
        indices(number) = js.Array(start, end).asInstanceOf[js.Tuple2[Int, Int]]
      inner.propagate(matchResult, indices, start, end)
    }
  }

  /** A look-around group of the form `(?= )`, `(?! )`, `(?<= )` or `(?<! )`.
   *
   *  Look-aheads propagate from left to right, while look-behinds propagate
   *  from right to left.
   */
  private final class LookAroundNode(isLookBehind: Boolean, indicator: String, inner: Node)
      extends Node {

    override def setNewGroup(newGroupIndex: Int): Int =
      inner.setNewGroup(super.setNewGroup(newGroupIndex))

    def buildRegex(groupNodeMap: js.Array[Node]): String =
      "((" + indicator + inner.buildRegex(groupNodeMap) + "))"

    def propagate(matchResult: js.RegExp.ExecResult,
        indices: IndicesArray, start: Int, end: Int): Unit = {
      if (isLookBehind)
        inner.propagateFromEnd(matchResult, indices, end)
      else
        inner.propagateFromStart(matchResult, indices, start)
    }
  }

  /** A repeated node. */
  private final class RepeatedNode(val inner: Node, val repeater: String) extends Node {

    override def setNewGroup(newGroupIndex: Int): Int =
      inner.setNewGroup(super.setNewGroup(newGroupIndex))

    def buildRegex(groupNodeMap: js.Array[Node]): String =
      "(" + inner.buildRegex(groupNodeMap) + repeater + ")"

    def propagate(matchResult: js.RegExp.ExecResult,
        indices: IndicesArray, start: Int, end: Int): Unit = {
      inner.propagateFromEnd(matchResult, indices, end)
    }
  }

  /** A leaf regex, without any subgroups. */
  private final class LeafRegexNode(val regex: String) extends Node {
    def buildRegex(groupNodeMap: js.Array[Node]): String =
      "(" + regex + ")"

    def propagate(matchResult: js.RegExp.ExecResult,
        indices: IndicesArray, start: Int, end: Int): Unit = {
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

    def propagate(matchResult: js.RegExp.ExecResult,
        indices: IndicesArray, start: Int, end: Int): Unit = {
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

    def propagate(matchResult: js.RegExp.ExecResult,
        indices: IndicesArray, start: Int, end: Int): Unit = {
      val len = sequence.length
      var i = 0
      var nextStart = start
      while (i != len) {
        nextStart =
          sequence(i).propagateFromStart(matchResult, indices, nextStart)
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

    def propagate(matchResult: js.RegExp.ExecResult,
        indices: IndicesArray, start: Int, end: Int): Unit = {
      val len = alternatives.length
      var i = 0
      while (i != len) {
        alternatives(i).propagate(matchResult, indices, start, end)
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
        /* Parse the pattern by code points if RegExp supports the 'u' flag,
         * in which case PatternCompiler always uses it, or by chars if it
         * doesn't. This distinction is important for repeated surrogate pairs.
         */
        val dispatchCP =
          if (PatternCompiler.Support.supportsUnicode) pattern.codePointAt(pIndex)
          else pattern.charAt(pIndex).toInt

        val baseNode = (dispatchCP: @switch) match {
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
            val indicator = pattern.jsSubstring(pIndex + 1, pIndex + 3)
            if (indicator == "?=" || indicator == "?!") {
              // Look-ahead group
              pIndex += 3
              val inner = parseInsideParensAndClosingParen()
              new LookAroundNode(isLookBehind = false, indicator, inner)
            } else if (indicator == "?<") {
              // Look-behind group, which must be ?<= or ?<!
              val fullIndicator = pattern.jsSubstring(pIndex + 1, pIndex + 4)
              pIndex += 4
              val inner = parseInsideParensAndClosingParen()
              new LookAroundNode(isLookBehind = true, fullIndicator, inner)
            } else if (indicator == "?:") {
              // Non-capturing group
              pIndex += 3
              val inner = parseInsideParensAndClosingParen()
              // Wrap LeafRegexNode's so that they do not merge with their neighbors
              if (inner.isInstanceOf[LeafRegexNode])
                new SequenceNode(js.Array(inner))
              else
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

            val startIndex = pIndex
            val c = pattern.charAt(startIndex + 1)
            pIndex += 2

            if (isDigit(c)) {
              // it is a back reference; parse all following digits
              while (isDigit(pattern.charAt(pIndex)))
                pIndex += 1
              new BackReferenceNode(
                  Integer.parseInt(pattern.jsSubstring(startIndex + 1, pIndex)))
            } else {
              // it is a character escape, or one of \b, \B, \d, \D, \p{...} or \P{...}
              if (c == 'p' || c == 'P') {
                while (pattern.charAt(pIndex) != '}')
                  pIndex += 1
                pIndex += 1
              }
              new LeafRegexNode(pattern.jsSubstring(startIndex, pIndex))
            }

          case '[' =>
            // parse until the corresponding ']' (here surrogate pairs don't matter)
            @tailrec def loop(pIndex: Int): Int = {
              pattern.charAt(pIndex) match {
                case '\\' => loop(pIndex + 2) // this is also fine for \p{...} and \P{...}
                case ']'  => pIndex + 1
                case _    => loop(pIndex + 1)
              }
            }

            val startIndex = pIndex
            pIndex = loop(startIndex + 1)
            val regex = pattern.jsSubstring(startIndex, pIndex)
            new LeafRegexNode(regex)

          case _ =>
            val start = pIndex
            pIndex += Character.charCount(dispatchCP)
            new LeafRegexNode(pattern.jsSubstring(start, pIndex))
        }

        if (baseNode ne null) { // null if we just completed an alternative
          (pattern.charAt(pIndex): @switch) match {
            case '+' | '*' | '?' =>
              val startIndex = pIndex
              if (pattern.charAt(startIndex + 1) == '?') // non-greedy mark
                pIndex += 2
              else
                pIndex += 1

              val repeater = pattern.jsSubstring(startIndex, pIndex)
              sequence.push(new RepeatedNode(baseNode, repeater))

            case '{' =>
              // parse until end of occurrence
              val startIndex = pIndex
              pIndex = pattern.indexOf("}", startIndex + 1) + 1
              if (pattern.charAt(pIndex) == '?') // non-greedy mark
                pIndex += 1
              val repeater = pattern.jsSubstring(startIndex, pIndex)
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
