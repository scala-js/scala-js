package java.nio.charset

class UnmappableCharacterException(
    inputLength: Int) extends CharacterCodingException {
  def getInputLength(): Int = inputLength

  override def getMessage(): String =
    "Input length = " + inputLength
}
