/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.io

import java.io._

import scala.language.implicitConversions

class ByteArrayInputStreamTest extends CommonStreamsTests {
  def mkStream(seq: Seq[Int]): InputStream = {
    new ByteArrayInputStream(seq.map(_.toByte).toArray)
  }
}
