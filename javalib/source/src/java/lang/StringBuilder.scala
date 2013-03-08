package java.lang

class StringBuilder(var content: String) {
  def this() = this("")
  def this(initialCapacity: Int) = this("")

  def append(s: String): StringBuilder = {
    content += s
    this
  }

  def append(b: Boolean): StringBuilder = append(b.toString())
  def append(c: Char): StringBuilder = append(c.toString())
  def append(b: Byte): StringBuilder = append(b.toString())
  def append(s: Short): StringBuilder = append(s.toString())
  def append(i: Int): StringBuilder = append(i.toString())
  def append(lng: Long): StringBuilder = append(lng.toString())
  def append(f: Float): StringBuilder = append(f.toString())
  def append(d: Double): StringBuilder = append(d.toString())

  def append(obj: AnyRef): StringBuilder = append(obj.toString())

  override def toString() = content

  def length() = content.length()

  def charAt(index: Int) = content.charAt(index)
  def codePointAt(index: Int) = content.codePointAt(index)
}
