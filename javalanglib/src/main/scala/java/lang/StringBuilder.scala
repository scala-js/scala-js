package java.lang

class StringBuilder(private var content: String) extends CharSequence
                                                    with Appendable
                                                    with java.io.Serializable {
  def this() = this("")
  def this(initialCapacity: Int) = this("")
  def this(csq: CharSequence) = this(csq.toString)

  def append(s: String): StringBuilder = {
    content += { if (s == null) "null" else s }
    this
  }

  def append(b: scala.Boolean): StringBuilder = append(b.toString())
  def append(c: scala.Char): StringBuilder = append(c.toString())
  def append(b: scala.Byte): StringBuilder = append(b.toString())
  def append(s: scala.Short): StringBuilder = append(s.toString())
  def append(i: scala.Int): StringBuilder = append(i.toString())
  def append(lng: scala.Long): StringBuilder = append(lng.toString())
  def append(f: scala.Float): StringBuilder = append(f.toString())
  def append(d: scala.Double): StringBuilder = append(d.toString())

  def append(obj: AnyRef): StringBuilder = {
    if (obj == null) append(null: String)
    else             append(obj.toString())
  }

  def append(csq: CharSequence): StringBuilder = append(csq: AnyRef)
  def append(csq: CharSequence, start: Int, end: Int): StringBuilder = {
    if (csq == null) append("null", start, end)
    else append(csq.subSequence(start, end).toString())
  }

  override def toString() = content

  def length() = content.length()

  def charAt(index: Int) = content.charAt(index)
  def codePointAt(index: Int) = content.codePointAt(index)

  def indexOf(str: String) = content.indexOf(str)
  def indexOf(str: String, fromIndex: Int) = content.indexOf(str, fromIndex)

  def lastIndexOf(str: String) = content.lastIndexOf(str)
  def lastIndexOf(str: String, fromIndex: Int) = content.lastIndexOf(str, fromIndex)

  def subSequence(start: Int, end: Int): CharSequence = substring(start, end)
  def substring(start: Int): String = content.substring(start)
  def substring(start: Int, end: Int): String = content.substring(start, end)

  def reverse(): StringBuilder = {
    val original = content
    var result = ""
    var i = 0
    while (i < original.length) {
      val c = original.charAt(i)
      if (Character.isHighSurrogate(c) && (i+1 < original.length)) {
        val c2 = original.charAt(i+1)
        if (Character.isLowSurrogate(c2)) {
          result = c.toString + c2.toString + result
          i += 2
        } else {
          result = c.toString + result
          i += 1
        }
      } else {
        result = c.toString + result
        i += 1
      }
    }
    content = result
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

}
