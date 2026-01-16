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

private[charset] object UTF_16 extends UTF_16_Common(
        "UTF-16",
        Array(
            "utf16", "UTF_16", "UnicodeBig", "unicode"),
        endianness = UTF_16_Common.AutoEndian)
