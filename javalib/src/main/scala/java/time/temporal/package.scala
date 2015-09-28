package java.time

package object temporal {
  private[time] def toScreamingSnakeCase(s: String): String = {
    val s1 = s.replaceAll("[A-Z]", "_$0").toUpperCase
    val s2 = s1.replace("AM_PM", "AMPM")
    if (s2.startsWith("_")) s2.tail else s2
  }
}
