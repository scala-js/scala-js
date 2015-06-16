package org.scalajs.testsuite.javalib

import java.{util => ju}

trait SortedSetTest extends SetTest {

  def testSortedSetApi(setFactory: SortedSetFactory): Unit = {
    testSetApi(setFactory)

    // TODO: implement tests (when we port the first SortedSet)
    it("should always be sorted") {

    }

    it("should return the first") {

    }

    it("should return the last") {

    }

    it("should return a proper headSet") {

    }

    it("should return a proper tailSet") {

    }

    it("should return a proper subSet") {

    }
  }
}

object SortedSetFactory {
  def allFactories: Iterator[SortedSetFactory] = Iterator.empty
}

trait SortedSetFactory extends SetFactory {
  def empty[E]: ju.SortedSet[E]
}
