package java.util.logging

object Level {
  val OFF: Level = new Level("OFF", Int.MaxValue)
  val SEVERE: Level = new Level("SEVERE", 1000)
  val WARNING: Level = new Level("WARNING", 900)
  val INFO: Level = new Level("INFO", 800)
  val CONFIG: Level = new Level("CONFIG", 700)
  val FINE: Level = new Level("FINE", 500)
  val FINER: Level = new Level("FINER", 400)
  val FINEST: Level = new Level("FINEST", 300)
  val ALL: Level = new Level("ALL", Int.MinValue)

  private lazy val knownLevels = Map[String, Level](OFF.getName -> OFF,
      SEVERE.getName -> SEVERE, WARNING.getName -> WARNING,
      INFO.getName -> INFO, CONFIG.getName -> CONFIG, FINE.getName -> FINE,
      FINER.getName -> FINER, FINEST.getName -> FINEST, ALL.getName -> ALL)

  def parse(name: String): Level =
    if (name == null) throw new NullPointerException("Name cannot be null")
    else knownLevels.getOrElse(name, throw new IllegalArgumentException(""))
}

class Level protected (private[this] val name: String,
    private[this] val value: Int,
    private[this] val resourceBundleName: String) {

  if (name == null)
    throw new NullPointerException("Name cannot be null")

  protected def this(name: String, value: Int) = this(name, value, null)

  def getResourceBundleName(): String = resourceBundleName

  def getName(): String = name

  // Not implemented, no locale in Scala.js
  //def getLocalizedName():String

  override def toString(): String = name

  def intValue(): Int = value

  override def equals(obj: Any): Boolean = obj match {
    case l: Level => intValue() == l.intValue()
    case _        => false
  }

  override def hashCode(): Int = value
}
