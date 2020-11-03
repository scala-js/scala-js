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

package java.nio.charset

private[charset] object UTF_16LE
    extends UTF_16_Common("UTF-16LE", Array("UnicodeLittleUnmarked", "UTF_16LE", "X-UTF-16LE"),
        endianness = UTF_16_Common.LittleEndian)
