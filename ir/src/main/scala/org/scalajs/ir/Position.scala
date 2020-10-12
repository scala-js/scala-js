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

/** Represents a character position in a source file
 *
 *  @param source Source file
 *  @param line Zero-based line number
 *  @param column Zero-based column number
 */
final case class Position(
    source: Position.SourceFile,
    line: Int,
    column: Int
) {
  def show: String = s"$line:$column"

  def isEmpty: Boolean = {
    def isEmptySlowPath(): Boolean = {
      source.getScheme == null && source.getRawAuthority == null &&
        source.getRawQuery == null && source.getRawFragment == null
    }
    source.getRawPath == "" && isEmptySlowPath()
  }

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
