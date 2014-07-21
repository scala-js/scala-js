package java.util.regex

trait MatchResult {
  def groupCount(): Int

  def start(): Int
  def end(): Int
  def group(): String

  def start(group: Int): Int
  def end(group: Int): Int
  def group(group: Int): String
}
