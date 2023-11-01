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

package java.io

abstract class FilterReader protected (protected val in: Reader) extends Reader {

  in.getClass() // null check

  override def close(): Unit = in.close()

  override def mark(readLimit: Int): Unit = in.mark(readLimit)

  override def markSupported(): Boolean = in.markSupported()

  override def read(): Int = in.read()

  override def read(buffer: Array[Char], offset: Int, count: Int): Int =
    in.read(buffer, offset, count)

  override def ready(): Boolean = in.ready()

  override def reset(): Unit = in.reset()

  override def skip(count: Long): Long = in.skip(count)
}
