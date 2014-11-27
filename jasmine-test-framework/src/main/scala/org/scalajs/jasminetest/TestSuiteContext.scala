/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Framework    **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.jasminetest

import scala.scalajs.js
import js.annotation.JSExport

trait TestSuiteContext {
  def describe(title: String)(test: => Unit): Unit
  def it(title: String)(test: => Unit): Unit
  def xdescribe(title: String)(test: => Unit): Unit
  def xit(title: String)(test: => Unit): Unit

  def when(tag: String): TestSuiteContext =
    if (TestSuiteContext.hasTag(tag)) this
    else new TestSuiteContext.IgnoredContext(this)

  def whenAll(tags: String*): TestSuiteContext =
    if (tags.forall(TestSuiteContext.hasTag)) this
    else new TestSuiteContext.IgnoredContext(this)

  def whenAny(tags: String*): TestSuiteContext =
    if (tags.exists(TestSuiteContext.hasTag)) this
    else new TestSuiteContext.IgnoredContext(this)

  def unless(tag: String): TestSuiteContext =
    if (!TestSuiteContext.hasTag(tag)) this
    else new TestSuiteContext.IgnoredContext(this)

  def unlessAll(tags: String*): TestSuiteContext =
    if (!tags.forall(TestSuiteContext.hasTag)) this
    else new TestSuiteContext.IgnoredContext(this)

  def unlessAny(tags: String*): TestSuiteContext =
    if (!tags.exists(TestSuiteContext.hasTag)) this
    else new TestSuiteContext.IgnoredContext(this)
}

@JSExport
object TestSuiteContext {
  // Stores tags in this Jasmine node. Is only modified by runner, which is
  // guaranteed to be unique by contract of the test framework.
  private var tags = Set.empty[String]

  /** Used to set tags from HTML/Ad-hoc test invocation */
  @JSExport
  protected def setTags(newTags: String*): Unit = setTags(newTags.toSet)

  private[jasminetest] def setTags(newTags: Set[String]): Unit =
    tags = newTags

  def hasTag(tag: String): Boolean = tags.contains(tag)

  private class IgnoredContext(
      baseContext: TestSuiteContext) extends TestSuiteContext {
    def describe(title: String)(test: => Unit): Unit =
      baseContext.xdescribe(title)(test)
    def it(title: String)(test: => Unit): Unit =
      baseContext.xit(title)(test)
    def xdescribe(title: String)(test: => Unit): Unit =
      baseContext.xdescribe(title)(test)
    def xit(title: String)(test: => Unit): Unit =
      baseContext.xit(title)(test)
  }
}
