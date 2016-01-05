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
