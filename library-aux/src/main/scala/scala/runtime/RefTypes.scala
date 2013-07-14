package scala.runtime

import java.io.Serializable

class BooleanRef(var elem: Boolean) extends Serializable {
  override def toString() = String.valueOf(elem)
}
class VolatileBooleanRef(var elem: Boolean) extends Serializable {
  override def toString() = String.valueOf(elem)
}

class CharRef(var elem: Char) extends Serializable {
  override def toString() = String.valueOf(elem)
}
class VolatileCharRef(var elem: Char) extends Serializable {
  override def toString() = String.valueOf(elem)
}

class ByteRef(var elem: Byte) extends Serializable {
  override def toString() = String.valueOf(elem)
}
class VolatileByteRef(var elem: Byte) extends Serializable {
  override def toString() = String.valueOf(elem)
}

class ShortRef(var elem: Short) extends Serializable {
  override def toString() = String.valueOf(elem)
}
class VolatileShortRef(var elem: Short) extends Serializable {
  override def toString() = String.valueOf(elem)
}

class IntRef(var elem: Int) extends Serializable {
  override def toString() = String.valueOf(elem)
}
class VolatileIntRef(var elem: Int) extends Serializable {
  override def toString() = String.valueOf(elem)
}

class LongRef(var elem: Long) extends Serializable {
  override def toString() = String.valueOf(elem)
}
class VolatileLongRef(var elem: Long) extends Serializable {
  override def toString() = String.valueOf(elem)
}

class FloatRef(var elem: Float) extends Serializable {
  override def toString() = String.valueOf(elem)
}
class VolatileFloatRef(var elem: Float) extends Serializable {
  override def toString() = String.valueOf(elem)
}

class DoubleRef(var elem: Double) extends Serializable {
  override def toString() = String.valueOf(elem)
}
class VolatileDoubleRef(var elem: Double) extends Serializable {
  override def toString() = String.valueOf(elem)
}

class ObjectRef[A](var elem: A) extends Serializable {
  override def toString() = String.valueOf(elem)
}
class VolatileObjectRef[A](var elem: A) extends Serializable {
  override def toString() = String.valueOf(elem)
}
