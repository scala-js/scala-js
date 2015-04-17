package java.net

import java.io.IOException

class URISyntaxException(
  private val input: String,
  private val reason: String,
  private val index: Int) extends Exception(
  s"$reason in $input at $index") {

  def this(input: String, reason: String) = this(input, reason, -1)

  def getIndex(): Int = index
  def getInput(): String = input
  def getReason(): String = reason

}

class MalformedURLException(message: String) extends IOException(message) {
  def this() = this(null)
}
