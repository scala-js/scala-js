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

package org.scalajs.linker.frontend.modulesplitter

import scala.collection.immutable.SortedSet

import org.scalajs.ir.Names.{ClassName, ObjectClass}
import org.scalajs.linker.standard.ModuleSet.ModuleID

/** Generators for internal module IDs.
 *
 *  In order to support case-insensitive file systems, the methods in this
 *  class all consider equality of module names as being case-insensitive.
 *  To be more precise, we use the *simple default casing* rules of Unicode
 *  for the default locale, without normalization.
 *
 *  The reference file in Unicode on case-insensitivy is about case folding:
 *  https://unicode.org/Public/UNIDATA/CaseFolding.txt
 *
 *  - The "simple" rules do not include case conversions that make a string
 *    longer. For example, we do not handle the fact that "ß" is equal to "SS"
 *    as well as "ss".
 *  - We do not use the Turkish-specific rules. Instead, we consider that all
 *    of 'i ı I İ' are equal.
 *
 *  We only have to ensure that we never generate names that may collide. We
 *  do not have to *optimally* do so. Therefore, it is fine to always consider
 *  all the 'i's to be the same, for example.
 */
private[modulesplitter] object InternalModuleIDGenerator {

  /** Generator based on `ClassName`s. */
  final class ForClassNames(avoid: Iterable[ModuleID]) {
    private val avoidSet: Set[String] =
      SortedSet(avoid.map(_.id).toSeq: _*)(CaseInsensitiveStringOrdering)

    /** Picks a representative from a list of classes.
     *
     *  Guarantees to return the same value independent of the order of [[names]].
     */
    def representativeClass(names: List[ClassName]): ClassName = {
      require(names.nonEmpty)

      /* Take the lexicographically smallest name as a stable name of the
       * module, with the exception of j.l.Object which identifies the root
       * module.
       *
       * We do this, because it is simple and stable (i.e. does not depend
       * on traversal order).
       */
      if (names.contains(ObjectClass)) ObjectClass
      else names.min
    }

    /** Builds an ID for the class with name [[name]].
     *
     *  The result is guaranteed to be:
     *  - Different from any public module ID.
     *  - Different for each ClassName.
     *  - Deterministic.
     */
    def forClassName(name: ClassName): ModuleID = {
      /* Build a module ID that doesn't collide with others.
       *
       * We observe:
       * - Class names are unique, so they never collide with each other.
       * - Appending a dot ('.') to a class name results in an illegal class name.
       *
       * So we append dots until we hit a ModuleID not used by a public module.
       *
       * Note that this is stable, because it does not depend on the order we
       * iterate over nodes.
       *
       * To deal with case-insensitive issues, basically we prefix every
       * uppercase character with a '-', and we prefix every '-' with a '-' to
       * avoid clashes. However, that is not good enough, since several
       * uppercase (and titlecase) code points can case-fold to the same
       * lowercase letter. Therefore, the complete scheme is:
       *
       * - ASCII uppercase letters are prefixed with '-'.
       * - '-' is prefixed with '-'.
       * - Non-ASCII characters are all prefixed by '-u' followed by the 6
       *   hexdigits of their codepoint.
       *
       * The last rule is far from being optimal, but it is safe. Encountering
       * non-ASCII characters in class names should be rare anyway.
       */

      val builder = new java.lang.StringBuilder

      // First, encode uppercase characters to avoid accidental case-insensitive clashes
      val originalNameString = name.nameString
      val originalNameStringLen = originalNameString.length()
      var i = 0
      while (i != originalNameStringLen) {
        val cp = originalNameString.codePointAt(i)
        if (cp < 0x80) {
          // ASCII
          if (cp == '-' || (cp >= 'A' && cp <= 'Z'))
            builder.append('-')
          builder.append(cp.toChar)
          i += 1
        } else {
          // Non-ASCII
          new java.util.Formatter(builder).format("-u%06x", Integer.valueOf(cp))
          builder.appendCodePoint(cp)
          i += Character.charCount(cp)
        }
      }

      // Second, avoid colliding with the public module IDs in `avoidSet`
      var candidateID = builder.toString()
      while (avoidSet.contains(candidateID)) {
        builder.append('.')
        candidateID = builder.toString()
      }
      ModuleID(candidateID)
    }
  }

  /** Generator based on digests. */
  final class ForDigests private (internalModuleIDPrefix: String) {
    def this(avoid: Iterable[ModuleID]) =
      this(freeInternalPrefix(avoid))

    def forDigest(digest: Array[Byte]): ModuleID = {
      @inline def hexDigit(digit: Int): Char =
        Character.forDigit(digit & 0x0f, 16)

      val id = new java.lang.StringBuilder(internalModuleIDPrefix)

      for (b <- digest) {
        id.append(hexDigit(b >> 4))
        id.append(hexDigit(b))
      }

      ModuleID(id.toString())
    }
  }

  /** Creates a prefix that is not a prefix of any of the IDs in [[avoid]] */
  private def freeInternalPrefix(avoid: Iterable[ModuleID]): String = {
    /* Here we can use `equalsIgnoreCase`, even though it has a poor notion of
     * case folding (which is even Char-based, not code point-based). That is
     * because we always compare against a string of the form 'internal---' for
     * an arbitrary number of '-'.
     *
     * - Only '-' is equal to '-'
     * - Only 'i ı I İ' are equal to 'i'
     * - Only ASCII letters are equal to the other letters of "internal"
     *
     * All these cases are handled by `equalsIgnoreCase`.
     */

    val BasePrefix = "internal"
    val BasePrefixLen = BasePrefix.length()

    // Does `id` start with "internal-", ignoring case
    def startsWith_internalDash(id: String): Boolean = {
      id.length() > BasePrefixLen &&
      id.charAt(BasePrefixLen) == '-' && // fast exit (avoid `substring`+`equalsIgnoreCase`)
      id.substring(0, BasePrefixLen).equalsIgnoreCase(BasePrefix)
    }

    // The first index of `id` after "internal" that is not a '-' (possibly `id.length()`).
    def findFirstNonDashIndex(id: String): Int = {
      val indexOrNegative = id.indexWhere(_ != '-', from = BasePrefixLen)
      if (indexOrNegative < 0)
        id.length()
      else
        indexOrNegative
    }

    def longestPrefixOfIDLike_internalDashes(id: ModuleID): Int = {
      if (startsWith_internalDash(id.id))
        findFirstNonDashIndex(id.id)
      else
        0
    }

    val longestPrefixLike_internalDashes =
      if (avoid.isEmpty) 0
      else avoid.iterator.map(longestPrefixOfIDLike_internalDashes(_)).max

    // Our prefix must be longer than that
    val freePrefixLen = longestPrefixLike_internalDashes + 1
    val requiredDashCount = Math.max(freePrefixLen - BasePrefixLen, 1)
    BasePrefix + ("-" * requiredDashCount)
  }

  private object CaseInsensitiveStringOrdering extends Ordering[String] {
    def compare(x: String, y: String): Int = x.compareToIgnoreCase(y)
  }
}
