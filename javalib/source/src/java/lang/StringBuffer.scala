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

  def reverse(): StringBuffer = {
    content = new StringBuilder(content).reverse().toString()
    this
  }

  def setCharAt(index: Int, ch: scala.Char): Unit = {
    if (index < 0 || index >= content.length)
      throw new IndexOutOfBoundsException("String index out of range: " + index)
    content = content.substring(0, index) + ch + content.substring(index + 1)
  }

}
