/*
 * Ported from https://github.com/junit-team/junit
 */
package org.junit

object ComparisonFailure {
  private final val MAX_CONTEXT_LENGTH = 20

  private class ComparisonCompactor(private val expected: String,
      private val actual: String) {

    private val ELLIPSIS: String = "..."
    private val DIFF_END: String = "]"
    private val DIFF_START: String = "["

    def compact(message: String): String = {
      if (expected == null || actual == null || expected.equals(actual)) {
        Assert.format(message, expected, actual)
      } else {
        val extractor = new DiffExtractor()
        val compactedPrefix = extractor.compactPrefix()
        val compactedSuffix = extractor.compactSuffix()
        Assert.format(message,
          compactedPrefix + extractor.expectedDiff() + compactedSuffix,
          compactedPrefix + extractor.actualDiff() + compactedSuffix)
      }
    }

    private[junit] def sharedPrefix(): String = {
      val end: Int = Math.min(expected.length, actual.length)
      (0 until end).find(i => expected.charAt(i) != actual.charAt(i))
        .fold(expected.substring(0, end))(expected.substring(0, _))
    }

    private def sharedSuffix(prefix: String): String = {
      def charAtFromEnd(s: String, i: Int): Char =
        s.charAt(s.length() - 1 - i)

      var suffixLength = 0
      var maxSuffixLength = Math.min(expected.length() - prefix.length(),
        actual.length() - prefix.length()) - 1
      while (suffixLength <= maxSuffixLength &&
          charAtFromEnd(expected, suffixLength) == charAtFromEnd(actual, suffixLength)) {
        suffixLength += 1
      }
      expected.substring(expected.length() - suffixLength)
    }

    private class DiffExtractor {

      private val _sharedPrefix: String = sharedPrefix()
      private val _sharedSuffix: String = sharedSuffix(_sharedPrefix)

      def expectedDiff(): String = extractDiff(expected)

      def actualDiff(): String = extractDiff(actual)

      def compactPrefix(): String = {
        if (_sharedPrefix.length() <= MAX_CONTEXT_LENGTH)
          _sharedPrefix
        else
          ELLIPSIS + _sharedPrefix.substring(_sharedPrefix.length() - MAX_CONTEXT_LENGTH)
      }

      def compactSuffix(): String = {
        if (_sharedSuffix.length() <= MAX_CONTEXT_LENGTH)
          _sharedSuffix
        else
          _sharedSuffix.substring(0, MAX_CONTEXT_LENGTH) + ELLIPSIS
      }

      private def extractDiff(source: String): String = {
        val sub = source.substring(_sharedPrefix.length(),
          source.length() - _sharedSuffix.length())
        DIFF_START + sub + DIFF_END
      }
    }
  }
}

class ComparisonFailure(message: String, fExpected: String, fActual: String)
    extends AssertionError(message) {

  import ComparisonFailure._

  override def getMessage(): String = {
    val cc = new ComparisonCompactor(fExpected, fActual)
    cc.compact(super.getMessage)
  }

  def getActual(): String = fActual

  def getExpected(): String = fExpected
}
