package java.lang

class StringBuffer(private var content: String) extends CharSequence
                                                   with Appendable
                                                   with java.io.Serializable {
  def this() = this("")
  def this(initialCapacity: Int) = this("")
  def this(csq: CharSequence) = this(csq.toString)

  def append(s: String): StringBuffer = {
    content += { if (s == null) "null" else s }
    this
  }

  def append(b: scala.Boolean): StringBuffer = append(b.toString())
  def append(c: scala.Char): StringBuffer = append(c.toString())

  def append(str: Array[scala.Char]): StringBuffer =
    append(str, 0, str.length)

  def append(str: Array[scala.Char], offset: Int, len: Int): StringBuffer = {
    var i = 0
    while (i < len) {
      content += str(i + offset)
      i += 1
    }
    this
  }

  def append(b: scala.Byte): StringBuffer = append(b.toString())
  def append(s: scala.Short): StringBuffer = append(s.toString())
  def append(i: scala.Int): StringBuffer = append(i.toString())
  def append(lng: scala.Long): StringBuffer = append(lng.toString())
  def append(f: scala.Float): StringBuffer = append(f.toString())
  def append(d: scala.Double): StringBuffer = append(d.toString())

  def append(obj: AnyRef): StringBuffer = {
    if (obj == null) append(null: String)
    else             append(obj.toString())
  }

  def append(csq: CharSequence): StringBuffer = append(csq: AnyRef)
  def append(csq: CharSequence, start: Int, end: Int): StringBuffer = {
    if (csq == null) append("null", start, end)
    else append(csq.subSequence(start, end).toString())
  }

  def appendCodePoint(codePoint: Int): StringBuffer =
    append(Character.toChars(codePoint))

  override def toString(): String = content

  def length(): Int = content.length()

  def charAt(index: Int): Char = content.charAt(index)
  def codePointAt(index: Int): Int = content.codePointAt(index)

  def indexOf(str: String): Int = content.indexOf(str)

  def indexOf(str: String, fromIndex: Int): Int =
    content.indexOf(str, fromIndex)

  def lastIndexOf(str: String): Int = content.lastIndexOf(str)

  def lastIndexOf(str: String, fromIndex: Int): Int =
    content.lastIndexOf(str, fromIndex)

  def subSequence(start: Int, end: Int): CharSequence = substring(start, end)
  def substring(start: Int): String = content.substring(start)
  def substring(start: Int, end: Int): String = content.substring(start, end)

  def reverse(): StringBuffer = {
    content = new StringBuilder(content).reverse().toString()
    this
  }

  def deleteCharAt(index: Int): StringBuffer = {
    if (index < 0 || index >= content.length)
      throw new StringIndexOutOfBoundsException("String index out of range: " + index)
    content = content.substring(0, index) + content.substring(index+1)
    this
  }

  def ensureCapacity(minimumCapacity: Int): Unit = {
    // Do nothing
  }

  /**
   * @param start The beginning index, inclusive.
   * @param end The ending index, exclusive.
   * @param str String that will replace previous contents.
   * @return This StringBuilder.
   */
  def replace(start: Int, end: Int, str: String): StringBuffer = {
    val length = content.length
    if (start < 0 || start > end || start > length) {
      throw new StringIndexOutOfBoundsException(
          s"Illegal to replace substring at [$start - $end] in string of length $length")
    }

    val realEnd = if (end > length) length else end // java api convention
    content = content.substring(0, start) + str + content.substring(realEnd)
    this
  }

  def setCharAt(index: Int, ch: scala.Char): Unit = {
    if (index < 0 || index >= content.length)
      throw new IndexOutOfBoundsException("String index out of range: " + index)
    content = content.substring(0, index) + ch + content.substring(index + 1)
  }

  def setLength(newLength: Int): Unit = {
    if (newLength < 0)
      throw new IndexOutOfBoundsException("String index out of range: " + newLength)

    val len = length()
    if (len == newLength) {
    } else if (len < newLength) {
      var index = len
      while (index < newLength) {
        append("\u0000")
        index += 1
      }
    } else {
      content = substring(0, newLength)
    }
  }

  def insert(index: Int, b: scala.Boolean): StringBuffer        = insert(index, b.toString)
  def insert(index: Int, b: scala.Byte): StringBuffer           = insert(index, b.toString)
  def insert(index: Int, s: scala.Short): StringBuffer          = insert(index, s.toString)
  def insert(index: Int, i: scala.Int): StringBuffer            = insert(index, i.toString)
  def insert(index: Int, l: scala.Long): StringBuffer           = insert(index, l.toString)
  def insert(index: Int, f: scala.Float): StringBuffer          = insert(index, f.toString)
  def insert(index: Int, d: scala.Double): StringBuffer         = insert(index, d.toString)
  def insert(index: Int, c: scala.Char): StringBuffer           = insert(index, c.toString)
  def insert(index: Int, csq: CharSequence): StringBuffer       = insert(index: Int, csq: AnyRef)
  def insert(index: Int, arr: Array[scala.Char]): StringBuffer  = insert(index, arr, 0, arr.length)

  def insert(index: Int, ref: AnyRef): StringBuffer =
    if (ref == null)
      insert(index, null: String)
    else
      insert(index, ref.toString)

  def insert(index: Int, csq: CharSequence, start: Int, end: Int): StringBuffer =
    if (csq == null)
      insert(index, "null", start, end)
    else
      insert(index, csq.subSequence(start, end).toString)


  def insert(index: Int, arr: Array[scala.Char], offset: Int, len: Int): StringBuffer = {
    var str = ""
    var i = 0
    while (i < len) {
      str += arr(i + offset)
      i += 1
    }
    insert(index, str)
  }

  def insert(index: Int, str: String): StringBuffer = {
    val thisLength = length()
    if (index < 0 || index > thisLength)
      throw new StringIndexOutOfBoundsException(index)
    else if (index == thisLength)
      append(str)
    else
      content = content.substring(0, index) + Option(str).getOrElse("null") + content.substring(index)
    this
  }
}
