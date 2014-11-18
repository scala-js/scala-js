package java.nio

abstract class Buffer private[nio] (val _capacity: Int) {
  private var _limit: Int = capacity
  private var _position: Int = 0
  private[nio] var _mark: Int = -1

  final def capacity(): Int = _capacity

  final def position(): Int = _position

  final def position(newPosition: Int): Buffer = {
    if (newPosition < 0 || newPosition > limit())
      throw new IllegalArgumentException
    _position = newPosition
    if (_mark > newPosition)
      _mark = -1
    this
  }

  final def limit(): Int = _limit

  final def limit(newLimit: Int): Buffer = {
    if (newLimit < 0 || newLimit > capacity())
      throw new IllegalArgumentException
    _limit = newLimit
    if (_position > newLimit) {
      _position = newLimit
      if (_mark > newLimit)
        _mark = -1
    }
    this
  }

  final def mark(): Buffer = {
    _mark = _position
    this
  }

  final def reset(): Buffer = {
    if (_mark == -1)
      throw new InvalidMarkException
    _position = _mark
    this
  }

  final def clear(): Buffer = {
    _mark = -1
    _position = 0
    _limit = capacity
    this
  }

  final def flip(): Buffer = {
    _mark = -1
    _limit = _position
    _position = 0
    this
  }

  final def rewind(): Buffer = {
    _mark = -1
    _position = 0
    this
  }

  @inline final def remaining(): Int = limit - position

  @inline final def hasRemaining(): Boolean = position != limit

  def isReadOnly(): Boolean

  def hasArray(): Boolean

  def array(): Object

  def arrayOffset(): Int

  def isDirect(): Boolean

  override def toString(): String =
    s"${getClass.getName}[pos=$position lim=$limit cap=$capacity]"
}
