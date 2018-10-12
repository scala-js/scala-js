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

package org.scalajs.testsuite.javalib.util

import org.junit.Test
import org.scalajs.testsuite.javalib.util.concurrent.CopyOnWriteArrayListFactory
import org.scalajs.testsuite.utils.CollectionsTestBase

class CollectionsOnCopyOnWriteArrayListTestOnJDK8 extends CollectionsTestBase {

  val factory = new CopyOnWriteArrayListFactory

  @Test def sort_on_comparables(): Unit =
    CollectionsOnListTest.sort_on_comparables(factory)

  @Test def sort_with_comparator(): Unit =
    CollectionsOnListTest.sort_with_comparator(factory)
}
