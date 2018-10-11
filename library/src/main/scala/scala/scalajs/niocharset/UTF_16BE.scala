/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.scalajs.niocharset

import java.nio.charset._

private[niocharset] object UTF_16BE extends UTF_16_Common(
    "UTF-16BE", Array(
    "X-UTF-16BE", "UTF_16BE", "ISO-10646-UCS-2", "UnicodeBigUnmarked"),
    endianness = UTF_16_Common.BigEndian)
