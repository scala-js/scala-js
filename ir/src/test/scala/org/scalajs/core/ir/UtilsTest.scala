package org.scalajs.core.ir

import java.net.URI

import org.junit.Test
import org.junit.Assert._

class UtilsTest {

  @Test def relativizeURI(): Unit = {
    def test(base: String, trgt: String, rel: String) = {
      val baseURI = new URI(base)
      val trgtURI = new URI(trgt)
      val relURI = Utils.relativize(baseURI, trgtURI)
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
