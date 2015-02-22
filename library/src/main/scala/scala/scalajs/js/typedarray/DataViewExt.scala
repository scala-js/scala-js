/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-lang.org/     **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.js.typedarray

/** Extensions for [[DataView]]. */
object DataViewExt {
  implicit class DataViewExtOps(val dataView: DataView) extends AnyVal {
    /** Reads a 2's complement signed 64-bit integers from the data view.
     *  @param index        Starting index
     *  @param littleEndian Whether the number is stored in little endian
     */
    @inline
    def getInt64(index: Int, littleEndian: Boolean = false): Long = {
      val high = dataView.getInt32(index + (if (littleEndian) 4 else 0), littleEndian)
      val low  = dataView.getInt32(index + (if (littleEndian) 0 else 4), littleEndian)
      (high.toLong << 32) | (low.toLong & 0xffffffffL)
    }

    /** Writes a 2's complement signed 64-bit integers to the data view.
     *  @param index        Starting index
     *  @param value        Value to be written
     *  @param littleEndian Whether to store the number in little endian
     */
    @inline
    def setInt64(index: Int, value: Long, littleEndian: Boolean = false): Unit = {
      val high = (value >>> 32).toInt
      val low = value.toInt
      dataView.setInt32(index + (if (littleEndian) 4 else 0), high, littleEndian)
      dataView.setInt32(index + (if (littleEndian) 0 else 4), low,  littleEndian)
    }
  }
}
