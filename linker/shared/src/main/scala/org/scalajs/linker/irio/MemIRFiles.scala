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

package org.scalajs.linker.irio

import org.scalajs.io._

/** A simple in-memory mutable virtual serialized Scala.js IR file. */
class MemVirtualSerializedScalaJSIRFile(path: String, val relativePath: String,
    content: Array[Byte], version: Option[String])
    extends MemVirtualBinaryFile(path, content, version)
    with VirtualSerializedScalaJSIRFile
