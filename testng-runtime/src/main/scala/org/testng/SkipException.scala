package org.testng

class SkipException(skipMessage: String) extends RuntimeException(skipMessage) {
  def isSkip: Boolean = true
}
