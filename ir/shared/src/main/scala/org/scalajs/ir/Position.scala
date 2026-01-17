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

package org.scalajs.ir

final case class Position(
    /** Source file. */
    source: Position.SourceFile,
    /** Zero-based line number. */
    line: Int,
    /** Zero-based column number. */
    column: Int
) {
  private val _isEmpty: Boolean = {
    def isEmptySlowPath(): Boolean = {
      source.getScheme == null && source.getRawAuthority == null &&
      source.getRawQuery == null && source.getRawFragment == null
    }
    source.getRawPath == "" && isEmptySlowPath()
  }

  def show: String = s"$line:$column"

  def isEmpty: Boolean = _isEmpty

  def isDefined: Boolean = !isEmpty

  def orElse(that: => Position): Position = if (isDefined) this else that
}

object Position {
  type SourceFile = java.net.URI

  object SourceFile {
    def apply(f: java.io.File): SourceFile = f.toURI
    def apply(f: String): SourceFile = new java.net.URI(f)
  }

  val NoPosition = Position(SourceFile(""), 0, 0)
}
