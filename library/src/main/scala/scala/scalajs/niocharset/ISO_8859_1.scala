/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-lang.org/     **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.niocharset

import java.nio.charset._

private[niocharset] object ISO_8859_1 extends ISO_8859_1_And_US_ASCII_Common( // scalastyle:ignore
    "ISO-8859-1", Array(
    "csISOLatin1", "IBM-819", "iso-ir-100", "8859_1", "ISO_8859-1", "l1",
    "ISO8859-1", "ISO_8859_1", "cp819", "ISO8859_1", "latin1",
    "ISO_8859-1:1987", "819", "IBM819"),
    maxValue = 0xff)
