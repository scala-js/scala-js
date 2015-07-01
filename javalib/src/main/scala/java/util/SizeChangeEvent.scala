package java.util

private[util] trait SizeChangeEvent {
  protected var end: Int

  @inline
  protected final def changeSize(delta: Int): Unit = {
    end += delta
    onSizeChanged(delta)
  }

  protected def onSizeChanged(delta: Int): Unit = () // override if needed
}
