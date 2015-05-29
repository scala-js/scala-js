package java.util

class ServiceConfigurationError(s: String, e: Throwable) extends Error(s, e) {
  def this(s: String) = this(s, null)
}

class ConcurrentModificationException(s: String) extends RuntimeException(s) {
  def this() = this(null)
}

class DuplicateFormatFlagsException private() extends IllegalFormatException {
  private var flags: String = null
  def this(f: String) = {
    this()
    if (f == null)
      throw new NullPointerException()
    flags = f
  }
  def getFlags(): String = flags
  override def getMessage(): String = s"Flags = '$flags'"
}

class EmptyStackException extends RuntimeException

class FormatFlagsConversionMismatchException private(private val c: Char) extends IllegalFormatException {
  private var f: String = null
  def this(f: String, c: Char) = {
    this(c)
    if (f == null)
      throw new NullPointerException()
    this.f = f
  }
  def getFlags(): String = f
  def getConversion(): Char = c
  override def getMessage(): String = "Conversion = " + c + ", Flags = " + f
}

class FormatterClosedException extends IllegalStateException

class IllegalFormatCodePointException(private val c: Int) extends IllegalFormatException {
  def getCodePoint(): Int = c
  override def getMessage(): String = s"Code point = $c"
}

class IllegalFormatConversionException private(private val c: Char) extends IllegalFormatException {
  private var arg: Class[_] = null
  def this(c: Char, arg: Class[_]) = {
    this(c)
    if (arg == null)
      throw new NullPointerException()
    this.arg = arg
  }
  def getConversion(): Char = c
  def getArgumentClass(): Class[_] = arg
  override def getMessage(): String = s"$c != ${arg.getName()}"
}

class IllegalFormatException private[util] () extends IllegalArgumentException

class IllegalFormatFlagsException private() extends IllegalFormatException {
  private var flags: String = null
  def this(f: String) = {
    this()
    if (f == null)
      throw new NullPointerException()
    this.flags = f
  }
  def getFlags(): String = flags
  override def getMessage(): String = "Flags = '" + flags + "'"
}

class IllegalFormatPrecisionException(private val p: Int) extends IllegalFormatException {
  def getPrecision(): Int = p
  override def getMessage(): String = Integer.toString(p)
}

class IllegalFormatWidthException(private val w: Int) extends IllegalFormatException {
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

class MissingFormatArgumentException private() extends IllegalFormatException {
  private var s: String = null
  def this(s: String) = {
    this()
    if (s == null)
      throw new NullPointerException()
    this.s = s
  }
  def getFormatSpecifier(): String = s
  override def getMessage(): String = "Format specifier '" + s + "'"
}

class MissingFormatWidthException private() extends IllegalFormatException {
  private var s: String = null
  def this(s: String) = {
    this()
    if (s == null)
      throw new NullPointerException()
    this.s = s
  }
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

class UnknownFormatConversionException private () extends IllegalFormatException {
  private var s: String = null
  def this(s: String) = {
    this()
    if (s == null)
      throw new NullPointerException()
    this.s = s
  }
  def getConversion(): String = s
  override def getMessage(): String = s"Conversion = '$s'"
}

class UnknownFormatFlagsException private() extends IllegalFormatException {
  private var flags: String = null
  def this(f: String) = {
    this()
    if (f == null)
      throw new NullPointerException()
    this.flags = f
  }
  def getFlags(): String = flags
  override def getMessage(): String = "Flags = " + flags
}
