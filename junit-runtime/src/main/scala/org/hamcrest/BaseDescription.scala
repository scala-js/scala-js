/*
 * Ported from https://github.com/hamcrest/JavaHamcrest/
 */
package org.hamcrest

import java.util.Arrays
import scala.annotation.tailrec
import org.hamcrest.internal.ArrayIterator
import org.hamcrest.internal.SelfDescribingValueIterator

abstract class BaseDescription extends Description {
  override def appendText(text: String): Description = {
    append(text)
    this
  }

  override def appendDescriptionOf(value: SelfDescribing): Description = {
    value.describeTo(this)
    this
  }

  override def appendValue(value: AnyRef): Description = {
    value match {
      case null =>
        append("null")

      case value: String =>
        append(toJavaSyntax(value))

      case value: java.lang.Character =>
        append('"')
        append(toJavaSyntax(value))
        append('"')

      case value: Array[AnyRef] =>
        appendValueList("[", ", ", "]", new ArrayIterator(value))

      case _ =>
        append('<')
        append(descriptionOf(value))
        append('>')
    }
    this
  }

  private def descriptionOf(value: AnyRef): String = {
    try {
      String.valueOf(value)
    } catch {
      case _: Exception =>
        s"${value.getClass.getName}@${Integer.toHexString(value.hashCode)}"
    }
  }

  override def appendValueList[T](start: String, separator: String, end: String,
      values: T*): Description = {
    appendValueList(start, separator, end, Arrays.asList(values))
  }

  override def appendValueList[T](start: String, separator: String, end: String,
      values:  java.lang.Iterable[T]): Description = {
    appendValueList(start, separator, end, values.iterator())
  }

  private def appendValueList[T](start: String, separator: String, end: String,
      values: java.util.Iterator[T]): Description = {
    appendList(start, separator, end, new SelfDescribingValueIterator[T](values))
  }

  override def appendList(start: String, separator: String, end: String,
      values: java.lang.Iterable[SelfDescribing]): Description = {
    appendList(start, separator, end, values.iterator())
  }

  private def appendList(start: String, separator: String, end: String,
      i: java.util.Iterator[SelfDescribing]): Description = {
    @tailrec
    def appendElems(separate: Boolean): Unit = {
      if (i.hasNext) {
        if (separate) append(separator)
        appendDescriptionOf(i.next)
        appendElems(true)
      }
    }
    append(start)
    appendElems(false)
    append(end)
    this
  }

  protected def append(str: String): Unit = {
    str.foreach(append)
  }

  protected def append(c: Char): Unit

  private def toJavaSyntax(unformatted: String): String =
    s""""${unformatted.map((ch: Char) => toJavaSyntax(ch))}"""" // Note the four "

  private def toJavaSyntax(ch: Char): String = {
    ch match {
      case '"'  => "\\\""
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case _    => ch.toString
    }
  }
}
