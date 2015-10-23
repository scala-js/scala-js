package sbt.testing

/** An optional <code>Throwable</code>. */
final class OptionalThrowable(
    private val exception: Throwable) extends Serializable {

  def this() = this(null)

  /** Indicates whether this <code>OptionalThrowable</code> is "defined,"
   *  <em>i.e.</em>, contains a <code>Throwable</code>.
   *
   * @return true if this <code>OptionalThrowable</code> contains a
   *         <code>Throwable</code>
   */
  def isDefined(): Boolean = exception != null

  /** Indicates whether this <code>OptionalThrowable</code> is "empty,"
   *  <em>i.e.</em>, contains no <code>Throwable</code>.
   *
   *  @return true if this <code>OptionalThrowable</code> contains no
   *          <code>Throwable</code>
   */
  def isEmpty(): Boolean = exception == null

  /** Returns the <code>Throwable</code> contained in this
   *  <code>OptionalThrowable</code> if defined, else throws
   *  <code>IllegalStateException</code>.
   *
   *  To avoid the <code>IllegalStateException</code>, ensure
   *  <code>isDefined</code> returns <code>true</code> before calling this
   *  method.
   *
   * @return the contained <code>Throwable</code>, if this
   *         <code>OptionalThrowable</code> is defined
   * @throws java.lang.IllegalStateException if this
   *     <code>OptionalThrowable</code> is not defined.
   */
  def get(): Throwable = {
    if (exception == null)
      throw new IllegalStateException("This OptionalThrowable is not defined")
    else
      exception
  }

  override def equals(that: Any): Boolean = that match {
    case that: OptionalThrowable =>
      this.exception eq that.exception
    case _ => false
  }

  override def hashCode(): Int =
    if (exception == null) 0 else exception.hashCode()

  override def toString(): String = {
    if (exception == null)
      "OptionalThrowable()"
    else
      s"OptionalThrowable($exception)"
  }
}
