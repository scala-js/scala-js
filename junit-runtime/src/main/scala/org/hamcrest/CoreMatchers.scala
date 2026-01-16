/*
 * Ported from https://github.com/hamcrest/JavaHamcrest/
 */
package org.hamcrest

import org.hamcrest.core._

object CoreMatchers {
  // Commented matchers where implemented using reflexion. It is possible that
  // some of them could be reimplemented from scratch without using reflexion.

  //  def allOf[T](matchers: java.lang.Iterable[Matcher[T]]): Matcher[T] =
  //    AllOf.allOf(matchers)

  //  def allOf[T](matchers: Matcher[T]*): Matcher[T] =
  //    AllOf.allOf(matchers)

  //  def anyOf[T](matchers: java.lang.Iterable[Matcher[T]]): AnyOf[T] =
  //    AnyOf.anyOf(matchers)

  //  def anyOf[T](matchers: Matcher[T]*) : AnyOf = AnyOf.anyOf(matchers)

  //  def both[LHS](matcher: Matcher[LHS]):
  //      CombinableMatcher.CombinableBothMatcher[LHS] = {
  //    CombinableMatcher.both(matcher)
  //  }

  //  def either[LHS](matcher: Matcher[LHS]):
  //      CombinableMatcher.CombinableEitherMatcher[LHS] = {
  //    CombinableMatcher.either(matcher)
  //  }

  //  def describedAs[T](description: String, matcher: Matcher[T],
  //      values: AnyRef*): Matcher[T] = {
  //    DescribedAs.describedAs(description, matcher, values)
  //  }

  //  def everyItem[U](itemMatcher: Matcher[U]): Matcher[java.lang.Iterable[U]] =
  //    Every.everyItem(itemMatcher)

  def is[T](matcher: Matcher[T]): Matcher[T] = Is.is(matcher)

  def is[T](value: T): Matcher[T] = Is.is(value)

  def isA[T](typ: java.lang.Class[T]): Matcher[T] = Is.isA(typ)

  //  def anything(): Matcher[AnyRef] = IsAnything.anything()

  //  def anything(description: String): Matcher[AnyRef] =
  //    IsAnything.anything(description)

  //  def hasItem[T](itemMatcher: Matcher[T]): Match[Iterable[T]] =
  //    IsCollectionContaining.hasItem(itemMatcher)

  //  def hasItem[T](item: T): Matcher[Iterable[T]] =
  //    IsCollectionContaining.hasItem(item)

  //  def hasItems[T](itemMatchers:Matcher[T]*): Matcher[T] =
  //    IsCollectionContaining.hasItems(itemMatchers)

  //  def hasItems[T](items: T*): Matcher[java.lang.Iterable[T]] =
  //    IsCollectionContaining.hasItems(items)

  //  def equalTo[T](operand: T): Matcher[T] =
  //    IsEqual.equalTo(operand)

  //  def equalToObject[T](operand: AnyRef): Matcher[AnyRef] =
  //    IsEqual.equalToObject(operand)

  def any[T](typ: Class[T]): Matcher[T] =
    core.IsInstanceOf.any(typ)

  def instanceOf[T](typ: Class[_]): Matcher[T] =
    core.IsInstanceOf.instanceOf(typ)

  def not[T](matcher: Matcher[T]): Matcher[T] =
    core.IsNot.not(matcher)

  def not[T](value: T): Matcher[T] =
    core.IsNot.not(value)

  def notNullValue(): Matcher[AnyRef] =
    core.IsNull.notNullValue()

  def notNullValue[T](typ: java.lang.Class[T]): Matcher[T] =
    core.IsNull.notNullValue(typ)

  def nullValue(): Matcher[AnyRef] =
    core.IsNull.nullValue()

  def nullValue[T](typ: java.lang.Class[T]): Matcher[T] =
    core.IsNull.nullValue(typ)

  //  def sameInstance[T](target: T): Matcher[T] =
  //    IsSame.sameInstance(target)

  //  def theInstance[T](target: T): Matcher[T] =
  //    IsSame.theInstance(target)

  //  def containsString(substring: String): Matcher[String] =
  //    StringContains.containsString(substring)

  //  def containsStringIgnoringCase(substring: String): Matcher[String] =
  //    StringContains.containsStringIgnoringCase(substring)

  //  def startsWith(prefix: String): Matcher[String] =
  //    core.StringStartsWith.startsWith(prefix)

  //  def startsWithIgnoringCase(prefix: String): Matcher[String] =
  //    core.StringStartsWith.startsWithIgnoringCase(prefix)

  //  def endsWith(suffix: String): Matcher[String] =
  //    core.StringEndsWith.endsWith(suffix)

  //  def endsWithIgnoringCase(suffix: String): Matcher[String] =
  //    core.StringEndsWith.endsWithIgnoringCase(suffix)
}
