/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-lang.org/     **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */

package scala.scalajs.niocharset

import java.nio.charset._

private[niocharset] object UTF_16LE // scalastyle:ignore
    extends UTF_16_Common("UTF-16LE",
        Array("UnicodeLittleUnmarked", "UTF_16LE", "X-UTF-16LE"),
        endianness = UTF_16_Common.LittleEndian)
