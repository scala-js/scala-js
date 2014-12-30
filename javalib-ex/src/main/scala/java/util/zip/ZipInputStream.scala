package java.util.zip

import java.io._

import scala.scalajs.js
import scala.scalajs.js.typedarray._

/** <a href="https://stuk.github.io/jszip/" target="_blank"
 *     class="badge badge-external" style="float: right;">
 *  External: JSZip</a>
 *  ZipInputStream implementation using
 *  <a href="https://stuk.github.io/jszip/" target="_blank">JSZip</a>
 */
class ZipInputStream(in: InputStream) extends InflaterInputStream(in) {

  // Not implemented
  // - All static constant fields (zip internals)
  // - protected def createZipEntry(name: String): ZipEntry

  private[this] val entryIter = {
    import js.Dynamic.{global => g}

    val data = in match {
      case in: ArrayBufferInputStream =>
        // Simulate reading all the data
        while (in.skip(in.available()) > 0) {}
        new Uint8Array(in.buffer, in.offset, in.length)
      case _ =>
        val arr = new js.Array[Int]
        var x = in.read()
        while (x != -1) {
          arr.push(x)
          x = in.read()
        }
        new Uint8Array(arr)
    }

    val zip = js.Dynamic.newInstance(g.JSZip)(data)
    val entries = zip.files.asInstanceOf[js.Dictionary[js.Dynamic]]

    entries.iterator
  }

  private[this] var inner: ArrayBufferInputStream = null

  override def close(): Unit = {
    closeEntry()
    super.close()
  }

  override def available(): Int = {
    if (inner == null || inner.available() <= 0) 0
    else 1
  }

  def closeEntry(): Unit = {
    if (inner != null)
      inner.close()
    inner = null
  }

  def getNextEntry(): ZipEntry = {
    closeEntry()
    if (entryIter.hasNext) {
      val (name, jsEntry) = entryIter.next()
      val res = new ZipEntry(name)
      res.setTime(jsEntry.date.asInstanceOf[js.Date].getTime().toLong)
      res.setComment(jsEntry.comment.asInstanceOf[String])

      inner = new ArrayBufferInputStream(
        jsEntry.asArrayBuffer().asInstanceOf[ArrayBuffer])

      res
    } else null
  }

  override def read(): Int = {
    if (inner == null) -1
    else inner.read()
  }

  override def read(buf: Array[Byte], off: Int, len: Int): Int = {
    if (len == 0) 0
    else if (inner == null) -1
    else inner.read(buf, off, len)
  }

  override def skip(n: Long): Long = {
    if (inner == null) 0
    else inner.skip(n)
  }

}
