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

package org.scalajs.io

import java.net.URI

import org.junit.Test
import org.junit.Assert._

class URIUtilsTest {

  @Test def relativizeURI(): Unit = {
    def test(base: String, trgt: String, rel: String) = {
      val baseURI = new URI(base)
      val trgtURI = new URI(trgt)
      val relURI = URIUtils.relativize(baseURI, trgtURI)
      assertEquals(rel, relURI.toString)
      assertEquals(trgtURI, baseURI.resolve(relURI))
    }

    test("file:///foo/bar/", "file:///foo/bar/baz", "baz")
    test("file:///foo/bar/boo", "file:///foo/bar/baz", "baz")

    test("file:///foo/bar/", "file:///foo/bar/baz/", "baz/")
    test("file:///foo/bar/boo", "file:///foo/bar/baz/", "baz/")

    test("file:///foo/bar/", "file:///foo/baz", "../baz")
    test("file:///foo/bar/boo", "file:///foo/baz", "../baz")

    test("file:///foo/bar/", "file:///foo/baz/", "../baz/")
    test("file:///foo/bar/boo", "file:///foo/baz/", "../baz/")

    test("file:///bar/foo/", "file:///foo/bar/baz", "../../foo/bar/baz")

    test("file:///foo", "http://bar.com/foo", "http://bar.com/foo")
    test("http://bob.com/foo", "http://bar.com/foo", "http://bar.com/foo")
  }

}
