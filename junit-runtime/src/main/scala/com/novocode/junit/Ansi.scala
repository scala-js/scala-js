package com.novocode.junit

object Ansi {

  private[this] final val NORMAL = "\u001B[0m"

  def c(s: String, colorSequence: String): String =
    if (colorSequence == null) s
    else colorSequence + s + NORMAL

  def filterAnsi(s: String): String = {
    if (s == null) {
      null
    } else {
      var r: String = ""
      val len = s.length
      var i = 0
      while (i < len) {
        val c = s.charAt(i)
        if (c == '\u001B') {
          i += 1
          while (i < len && s.charAt(i) != 'm')
            i += 1
        } else {
          r += c
        }
        i += 1
      }
      r
    }
  }

  final val INFO = "\u001B[34m" // BLUE
  final val ERRCOUNT = "\u001B[31m" // RED
  final val IGNCOUNT = "\u001B[33m" // YELLOW
  final val ERRMSG = "\u001B[31m" // RED
  final val NNAME1 = "\u001B[33m" // YELLOW
  final val NNAME2 = "\u001B[36m" // CYAN
  final val NNAME3 = "\u001B[33m" // YELLOW
  final val ENAME1 = "\u001B[33m" // YELLOW
  final val ENAME2 = "\u001B[31m" // RED
  final val ENAME3 = "\u001B[33m" // YELLOW
  final val TESTFILE1 = "\u001B[35m" // MAGENTA
  final val TESTFILE2 = "\u001B[33m" // YELLOW
}
