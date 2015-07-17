/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-lang.org/     **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.niocharset

import java.nio.charset._

private[niocharset] object UTF_16BE extends UTF_16_Common( // scalastyle:ignore
    "UTF-16BE", Array(
    "X-UTF-16BE", "UTF_16BE", "ISO-10646-UCS-2", "UnicodeBigUnmarked"),
    endianness = UTF_16_Common.BigEndian)
