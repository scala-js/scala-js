/*
 * Ported from https://github.com/hamcrest/JavaHamcrest/
 */
package org.hamcrest

object Description {
  val NONE: Description = new NullDescription

  final class NullDescription extends Description {
    override def appendDescriptionOf(value: SelfDescribing): Description = this

    override def appendList(start: String, separator: String, end: String,
        values: java.lang.Iterable[SelfDescribing]): Description = {
      this
    }

    override def appendText(text: String): Description = this

    override def appendValue(value: AnyRef): Description = this

    override def appendValueList[T](start: String, separator: String,
        end: String, values: T*): Description = {
      this
    }

    override def appendValueList[T](start: String, separator: String,
        end: String, values: java.lang.Iterable[T]): Description = {
      this
    }

    override def toString(): String = ""
  }
}

trait Description {
  def appendText(text: String): Description

  def appendDescriptionOf(value: SelfDescribing): Description

  def appendValue(value: AnyRef): Description

  def appendValueList[T](start: String, separator: String, end: String,
      values: T*): Description

  def appendValueList[T](start: String, separator: String, end: String,
      values: java.lang.Iterable[T]): Description

  def appendList(start: String, separator: String, end: String,
      values: java.lang.Iterable[SelfDescribing]): Description
}
