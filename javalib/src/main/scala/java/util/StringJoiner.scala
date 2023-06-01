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

package java.util

@inline
final class StringJoiner private (delimiter: String, prefix: String, suffix: String) extends AnyRef {
  /** The custom value to return if empty, set by `setEmptyValue` (nullable).
   *
   *  If `null`, defaults to `prefix + suffix`.
   */
  private var emptyValue: String = null

  /** The current value, excluding prefix and suffix. */
  private var value: String = ""

  /** Whether the string joiner is currently empty. */
  private var isEmpty: Boolean = true

  def this(delimiter: CharSequence) =
    this(delimiter.toString(), "", "")

  def this(delimiter: CharSequence, prefix: CharSequence, suffix: CharSequence) =
    this(delimiter.toString(), prefix.toString(), suffix.toString())

  def setEmptyValue(emptyValue: CharSequence): StringJoiner = {
    this.emptyValue = emptyValue.toString()
    this
  }

  override def toString(): String =
    if (isEmpty && emptyValue != null) emptyValue
    else prefix + value + suffix

  def add(newElement: CharSequence): StringJoiner = {
    if (isEmpty)
      isEmpty = false
    else
      value += delimiter
    value += newElement // if newElement is null, adds "null"
    this
  }

  def merge(other: StringJoiner): StringJoiner = {
    if (!other.isEmpty) // if `other` is empty, `merge` has no effect
      add(other.value) // without prefix nor suffix, but with delimiters
    this
  }

  def length(): Int =
    if (isEmpty && emptyValue != null) emptyValue.length()
    else prefix.length() + value.length() + suffix.length()
}
