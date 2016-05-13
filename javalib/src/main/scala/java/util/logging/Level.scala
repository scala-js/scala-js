package java.util.logging

object Level {
  val OFF: Level     = new Level("OFF", Int.MaxValue)
  val SEVERE: Level  = new Level("SEVERE", 1000)
  val WARNING: Level = new Level("WARNING", 900)
  val INFO: Level    = new Level("INFO", 800)
  val CONFIG: Level  = new Level("CONFIG", 700)
  val FINE: Level    = new Level("FINE", 500)
  val FINER: Level   = new Level("FINER", 400)
  val FINEST: Level  = new Level("FINEST", 300)
  val ALL: Level     = new Level("ALL", Int.MinValue)

  private val knownLevels = List(OFF, SEVERE, WARNING, INFO, CONFIG, FINE,
    FINER, FINEST, ALL)

  def parse(name: String):Level = {
    if (name == null) throw new NullPointerException("Name cannot be null")
    knownLevels.find(l => l.getName == name || l.intValue().toString == name)
      .fold(throw new IllegalArgumentException(""))(l => l)
  }
}

class Level protected (private[this] val name: String,
    private[this] val value: Int,
    private[this] val resourceBundleName: String) {
  if (name == null) throw new NullPointerException("Name cannot be null")

  protected def this(name: String, value: Int) = this(name, value, null)

  def getResourceBundleName():String = resourceBundleName

  def getName():String = name

  // Not implemented, no locale in Scala.js
  //def getLocalizedName():String

  override def toString():String = name

  def intValue():Int = value

  override def equals(ox: Any):Boolean = ox match {
    case l: Level => intValue() == l.intValue()
    case _        => false
  }

  override def hashCode():Int = value.hashCode()
}
