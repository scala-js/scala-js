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

private[charset] object ISO_8859_1 extends ISO_8859_1_And_US_ASCII_Common(
    "ISO-8859-1", Array(
    "csISOLatin1", "IBM-819", "iso-ir-100", "8859_1", "ISO_8859-1", "l1",
    "ISO8859-1", "ISO_8859_1", "cp819", "ISO8859_1", "latin1",
    "ISO_8859-1:1987", "819", "IBM819"),
    maxValue = 0xff)
