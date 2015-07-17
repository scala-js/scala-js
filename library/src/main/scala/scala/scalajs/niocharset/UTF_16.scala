/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-lang.org/     **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.niocharset

import java.nio.charset._

private[niocharset] object UTF_16 extends UTF_16_Common( // scalastyle:ignore
    "UTF-16", Array(
    "utf16", "UTF_16", "UnicodeBig", "unicode"),
    endianness = UTF_16_Common.AutoEndian)
