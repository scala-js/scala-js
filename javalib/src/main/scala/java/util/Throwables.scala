package java.util

class ServiceConfigurationError(s: String, e: Throwable) extends Error(s, e) {
  def this(s: String) = this(s, null)
}

class ConcurrentModificationException(s: String) extends RuntimeException(s) {
  def this() = this(null)
}

class DuplicateFormatFlagsException(f: String) extends IllegalFormatException {
  if (f == null)
    throw new NullPointerException()

  def getFlags(): String = f
  override def getMessage(): String = "Flags = '" + f + "'"
}

class EmptyStackException extends RuntimeException

class FormatFlagsConversionMismatchException(f: String, c: Char)
    extends IllegalFormatException {

  if (f == null)
    throw new NullPointerException()

  def getFlags(): String = f
  def getConversion(): Char = c
  override def getMessage(): String = "Conversion = " + c + ", Flags = " + f
}

class FormatterClosedException extends IllegalStateException

class IllegalFormatCodePointException(c: Int) extends IllegalFormatException {
  def getCodePoint(): Int = c
  override def getMessage(): String = "Code point = 0x" + Integer.toHexString(c)
}

class IllegalFormatConversionException(c: Char, arg: Class[_])
    extends IllegalFormatException {

  if (arg == null)
    throw new NullPointerException()

  def getConversion(): Char = c
  def getArgumentClass(): Class[_] = arg

  override def getMessage(): String = c.toString() + " != " + arg.getName()
}

class IllegalFormatException private[util] () extends IllegalArgumentException

class IllegalFormatFlagsException(f: String) extends IllegalFormatException {
  if (f == null)
    throw new NullPointerException()

  def getFlags(): String = f
  override def getMessage(): String = "Flags = '" + f + "'"
}

class IllegalFormatPrecisionException(p: Int) extends IllegalFormatException {
  def getPrecision(): Int = p
  override def getMessage(): String = Integer.toString(p)
}

class IllegalFormatWidthException(w: Int) extends IllegalFormatException {
  def getWidth(): Int = w
  override def getMessage(): String = Integer.toString(w)
}

class IllformedLocaleException(s: String, errorIndex: Int)
  extends RuntimeException(s + (if (errorIndex < 0) "" else " [at index " + errorIndex + "]")) {
  def this() = this(null, -1)
  def this(s: String) = this(s, -1)
  def getErrorIndex(): Int = errorIndex
}

class InputMismatchException(s: String) extends NoSuchElementException(s) {
  def this() = this(null)
}

class InvalidPropertiesFormatException(s: String) extends java.io.IOException(s) {
  def this(e: Throwable) = {
    this(if (e == null) null.asInstanceOf[String] else e.toString())
    this.initCause(e)
  }
  // private def writeObject(out: java.io.ObjectOutputStream) =
  //   throw new java.io.NotSerializableException("Not serializable.")
  // private def readObject(in: java.io.ObjectInputStream) =
  //   throw new java.io.NotSerializableException("Not serializable.")
}

class MissingFormatArgumentException(s: String) extends IllegalFormatException {
  if (s == null)
    throw new NullPointerException()

  def getFormatSpecifier(): String = s
  override def getMessage(): String = "Format specifier '" + s + "'"
}

class MissingFormatWidthException(s: String) extends IllegalFormatException {
  if (s == null)
    throw new NullPointerException()

  def getFormatSpecifier(): String = s
  override def getMessage(): String = s
}

class MissingResourceException private[util](
    s: String, private var className: String, private var key: String, e: Throwable)
    extends RuntimeException(s, e) {
  def this(s: String, className: String, key: String) = this(s, className, key, null)
  def getClassName(): String = className
  def getKey(): String = key
}

class NoSuchElementException(s: String) extends RuntimeException(s) {
  def this() = this(null)
}

class TooManyListenersException(s: String) extends Exception(s) {
  def this() = this(null)
}

class UnknownFormatConversionException(s: String)
    extends IllegalFormatException {

  if (s == null)
    throw new NullPointerException()

  def getConversion(): String = s
  override def getMessage(): String = "Conversion = '" + s + "'"
}

class UnknownFormatFlagsException(f: String) extends IllegalFormatException {
  if (f == null)
    throw new NullPointerException()

  def getFlags(): String = f
  override def getMessage(): String = "Flags = " + f
}
