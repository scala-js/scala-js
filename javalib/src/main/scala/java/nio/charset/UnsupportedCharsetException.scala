package java.nio.charset

class UnsupportedCharsetException(
    charsetName: String) extends IllegalArgumentException(charsetName) {
  def getCharsetName(): String = charsetName
}
