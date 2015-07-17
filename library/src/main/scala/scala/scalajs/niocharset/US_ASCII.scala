/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-lang.org/     **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.niocharset

import java.nio.charset._

private[niocharset] object US_ASCII extends ISO_8859_1_And_US_ASCII_Common( // scalastyle:ignore
    "US-ASCII", Array(
    "cp367", "ascii7", "ISO646-US", "646", "csASCII", "us", "iso_646.irv:1983",
    "ISO_646.irv:1991", "IBM367", "ASCII", "default", "ANSI_X3.4-1986",
    "ANSI_X3.4-1968", "iso-ir-6"),
    maxValue = 0x7f)
