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

package org.scalajs.linker.interface

import org.junit.Test
import org.junit.Assert._

class ModuleSplitStyleSmallModulesForTest {
  import ModuleSplitStyle.SmallModulesFor

  @Test
  def acceptPackage(): Unit =
    SmallModulesFor(List("a.b.c"))

  @Test(expected = classOf[IllegalArgumentException])
  def rejectNoPackage(): Unit =
    SmallModulesFor(List())

  @Test(expected = classOf[IllegalArgumentException])
  def rejectEmptyPackage(): Unit =
    SmallModulesFor(List(""))

  @Test(expected = classOf[IllegalArgumentException])
  def rejectTrailingDot(): Unit =
    SmallModulesFor(List("a.b.c."))

  @Test(expected = classOf[IllegalArgumentException])
  def rejectLeadingDot(): Unit =
    SmallModulesFor(List(".a.b.c"))

  @Test(expected = classOf[IllegalArgumentException])
  def rejectDoubleDot(): Unit =
    SmallModulesFor(List("a.b..c"))

  @Test(expected = classOf[IllegalArgumentException])
  def rejectSlash(): Unit =
    SmallModulesFor(List("a.b/.c"))

  @Test(expected = classOf[IllegalArgumentException])
  def rejectOpenBracket(): Unit =
    SmallModulesFor(List("a.b[.c"))

  @Test(expected = classOf[IllegalArgumentException])
  def rejectColon(): Unit =
    SmallModulesFor(List("a.b:.c"))

  @Test(expected = classOf[IllegalArgumentException])
  def rejectUnpairedSurrogate(): Unit =
    SmallModulesFor(List("a.\ud800.c"))
}
