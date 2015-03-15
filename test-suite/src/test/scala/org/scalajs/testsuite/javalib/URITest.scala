/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib

import scala.scalajs.js
import org.scalajs.jasminetest.JasmineTest

import java.net.URI

object URITest extends JasmineTest {

  def expectURI(uri: URI, isAbsolute: Boolean, isOpaque: Boolean)(
    authority: String = null, fragment: String = null,
    host: String = null, path: String = null, port: Int = -1,
    query: String = null, scheme: String = null, userInfo: String = null,
    schemeSpecificPart: String = null)(rawAuthority: String = authority,
    rawFragment: String = fragment, rawPath: String = path,
    rawQuery: String = query, rawUserInfo: String = userInfo,
    rawSchemeSpecificPart: String = schemeSpecificPart): Unit = {

    expect(uri.getAuthority()).toBe(authority)
    expect(uri.getFragment()).toBe(fragment)
    expect(uri.getHost()).toBe(host)
    expect(uri.getPath()).toBe(path)
    expect(uri.getPort()).toBe(port)
    expect(uri.getQuery()).toBe(query)
    expect(uri.getRawAuthority()).toBe(rawAuthority)
    expect(uri.getRawFragment()).toBe(rawFragment)
    expect(uri.getRawPath()).toBe(rawPath)
    expect(uri.getRawQuery()).toBe(rawQuery)
    expect(uri.getRawSchemeSpecificPart()).toBe(rawSchemeSpecificPart)
    expect(uri.getRawUserInfo()).toBe(rawUserInfo)
    expect(uri.getScheme()).toBe(scheme)
    expect(uri.getSchemeSpecificPart()).toBe(schemeSpecificPart)
    expect(uri.getUserInfo()).toBe(userInfo)
    expect(uri.isAbsolute()).toBe(isAbsolute)
    expect(uri.isOpaque()).toBe(isOpaque)

  }

  describe("java.net.URI") {

    it("should parse vanilla absolute URIs") {
      expectURI(new URI("http://java.sun.com/j2se/1.3/"), true, false)(
          scheme = "http",
          host = "java.sun.com",
          path = "/j2se/1.3/",
          authority = "java.sun.com",
          schemeSpecificPart = "//java.sun.com/j2se/1.3/")()
    }

    it("should parse absolute URIs with IPv6") {
      val uri = new URI("http://hans@[ffff::0:128.4.5.3]:345/~hans/")
      expectURI(uri, true, false)(
          scheme = "http",
          host = "[ffff::0:128.4.5.3]",
          userInfo = "hans",
          port = 345,
          path = "/~hans/",
          authority = "hans@[ffff::0:128.4.5.3]:345",
          schemeSpecificPart = "//hans@[ffff::0:128.4.5.3]:345/~hans/"
      )()
    }

    it("should parse absolute URIs without authority") {
      expectURI(new URI("file:/~/calendar"), true, false)(
          scheme = "file",
          path = "/~/calendar",
          schemeSpecificPart = "/~/calendar")()
    }

    it("should parse absolute URIs with empty authority") {
      expectURI(new URI("file:///~/calendar"), true, false)(
          authority = "",
          scheme = "file",
          path = "/~/calendar",
          schemeSpecificPart = "///~/calendar")()
    }

    it("should parse opaque URIs") {
      expectURI(new URI("mailto:java-net@java.sun.com"), true, true)(
          scheme = "mailto",
          schemeSpecificPart = "java-net@java.sun.com")()

      expectURI(new URI("news:comp.lang.java"), true, true)(
          scheme = "news",
          schemeSpecificPart = "comp.lang.java")()

      expectURI(new URI("urn:isbn:096139210x"), true, true)(
          scheme = "urn",
          schemeSpecificPart = "isbn:096139210x")()
    }

    it("should parse relative URIs") {
      expectURI(new URI("docs/guide/collections/designfaq.html#28"), false, false)(
          path = "docs/guide/collections/designfaq.html",
          fragment = "28",
          schemeSpecificPart = "docs/guide/collections/designfaq.html"
      )()
      expectURI(new URI("../../../demo/jfc/SwingSet2/src/SwingSet2.java"), false, false)(
          path = "../../../demo/jfc/SwingSet2/src/SwingSet2.java",
          schemeSpecificPart = "../../../demo/jfc/SwingSet2/src/SwingSet2.java"
      )()
    }

    it("should parse relative URIs with IPv4") {
      expectURI(new URI("//123.5.6.3:45/bar"), false, false)(
          authority = "123.5.6.3:45",
          host = "123.5.6.3",
          port = 45,
          path = "/bar",
          schemeSpecificPart = "//123.5.6.3:45/bar"
      )()
    }

    it("should parse relative URIs with registry-based authority") {
      expectURI(new URI("//foo:bar"), false, false)(
          authority = "foo:bar",
          schemeSpecificPart = "//foo:bar"
      )()
    }

    it("should parse relative URIs with escapes") {
      expectURI(new URI("//ma%5dx:secret@example.com:8000/foo"), false, false)(
          authority = "ma]x:secret@example.com:8000",
          userInfo = "ma]x:secret",
          host = "example.com",
          port = 8000,
          path = "/foo",
          schemeSpecificPart = "//ma]x:secret@example.com:8000/foo")(
          rawUserInfo = "ma%5dx:secret",
          rawAuthority = "ma%5dx:secret@example.com:8000",
          rawSchemeSpecificPart = "//ma%5dx:secret@example.com:8000/foo")
    }

    it("should parse relative URIs with fragment only") {
      expectURI(new URI("#foo"), false, false)(
          fragment = "foo",
          path = "",
          schemeSpecificPart = ""
          )()
    }

    it("should parse relative URIs with query and fragment") {
      expectURI(new URI("?query=1#foo"), false, false)(
          query = "query=1",
          fragment = "foo",
          path = "",
          schemeSpecificPart = "?query=1"
          )()
    }

    it("should provide compareTo") {
      val x = new URI("http://example.com/asdf%6a")
      val y = new URI("http://example.com/asdf%6A")
      val z = new URI("http://example.com/asdfj")
      val rel = new URI("/foo/bar")

      expect(x.compareTo(y)).toBeGreaterThan(0)
      expect(x.compareTo(z)).toBeLessThan(0)
      expect(y.compareTo(z)).toBeLessThan(0)
      expect(x.compareTo(x)).toBe(0)
      expect(y.compareTo(y)).toBe(0)
      expect(z.compareTo(z)).toBe(0)
      expect(x.compareTo(rel)).toBeGreaterThan(0)
      expect(y.compareTo(rel)).toBeGreaterThan(0)
      expect(z.compareTo(rel)).toBeGreaterThan(0)
      expect(rel.compareTo(rel)).toBe(0)
    }

    it("should provide equals") {
      val x = new URI("http://example.com/asdf%6a")
      val y = new URI("http://example.com/asdf%6A")
      val z = new URI("http://example.com/asdfj")

      expect(x == y).toBeTruthy
      expect(x == z).toBeFalsy
      expect(y == z).toBeFalsy
      expect(x == x).toBeTruthy
      expect(y == y).toBeTruthy
      expect(z == z).toBeTruthy

      expect(new URI("foo:helloWorld%6b%6C") == new URI("foo:helloWorld%6C%6b"))
    }

    it("should provide normalize") {
      expectURI(new URI("http://example.com/../asef/../../").normalize, true, false)(
          scheme = "http",
          host = "example.com",
          authority = "example.com",
          path = "/../../",
          schemeSpecificPart = "//example.com/../../")()
      expectURI(new URI("http://example.com/../as/./ef/foo/../../").normalize, true, false)(
          scheme = "http",
          host = "example.com",
          authority = "example.com",
          path = "/../as/",
          schemeSpecificPart = "//example.com/../as/")()
      expectURI(new URI("bar/../fo:o/./bar").normalize, false, false)(
          path = "./fo:o/bar",
          schemeSpecificPart = "./fo:o/bar")()
      expectURI(new URI("bar/..//fo:o//./bar").normalize, false, false)(
          path = "./fo:o/bar",
          schemeSpecificPart = "./fo:o/bar")()

      val x = new URI("http://www.example.com/foo/bar")
      expect(x.normalize eq x).toBeTruthy
    }

    it("should provide resolve - JavaDoc examples") {
      val base = "http://java.sun.com/j2se/1.3/"
      val relative1 = "docs/guide/collections/designfaq.html#28"
      val resolved1 =
        "http://java.sun.com/j2se/1.3/docs/guide/collections/designfaq.html#28"
      val relative2 = "../../../demo/jfc/SwingSet2/src/SwingSet2.java"
      val resolved2 =
        "http://java.sun.com/j2se/1.3/demo/jfc/SwingSet2/src/SwingSet2.java"

      expect(new URI(base).resolve(relative1).toString).toEqual(resolved1)
      expect(new URI(resolved1).resolve(relative2).toString).toEqual(resolved2)
    }

    it("should provide resolve - RFC2396 examples") {
      val base = new URI("http://a/b/c/d;p?q")
      def resTest(ref: String, trg: String) =
        expect(base.resolve(ref).toString).toEqual(trg)

      // Normal examples
      resTest("g:h"    , "g:h")
      resTest("g"      , "http://a/b/c/g")
      resTest("./g"    , "http://a/b/c/g")
      resTest("g/"     , "http://a/b/c/g/")
      resTest("/g"     , "http://a/g")
      resTest("//g"    , "http://g")
      resTest("?y"     , "http://a/b/c/?y")
      resTest("g?y"    , "http://a/b/c/g?y")
      resTest("#s"     , "http://a/b/c/d;p?q#s")
      resTest("g#s"    , "http://a/b/c/g#s")
      resTest("g?y#s"  , "http://a/b/c/g?y#s")
      resTest(";x"     , "http://a/b/c/;x")
      resTest("g;x"    , "http://a/b/c/g;x")
      resTest("g;x?y#s", "http://a/b/c/g;x?y#s")
      resTest("."      , "http://a/b/c/")
      resTest("./"     , "http://a/b/c/")
      resTest(".."     , "http://a/b/")
      resTest("../"    , "http://a/b/")
      resTest("../g"   , "http://a/b/g")
      resTest("../.."  , "http://a/")
      resTest("../../" , "http://a/")
      resTest("../../g", "http://a/g")

      // Abnormal examples
      resTest("../../../g"   , "http://a/../g")
      resTest("../../../../g", "http://a/../../g")
      resTest("/./g"         , "http://a/./g")
      resTest("/../g"        , "http://a/../g")
      resTest("g."           , "http://a/b/c/g.")
      resTest(".g"           , "http://a/b/c/.g")
      resTest("g.."          , "http://a/b/c/g..")
      resTest("..g"          , "http://a/b/c/..g")
      resTest("./../g"       , "http://a/b/g")
      resTest("./g/."        , "http://a/b/c/g/")
      resTest("g/./h"        , "http://a/b/c/g/h")
      resTest("g/../h"       , "http://a/b/c/h")
      resTest("g;x=1/./y"    , "http://a/b/c/g;x=1/y")
      resTest("g;x=1/../y"   , "http://a/b/c/y")
      resTest("g?y/./x"      , "http://a/b/c/g?y/./x")
      resTest("g?y/../x"     , "http://a/b/c/g?y/../x")
      resTest("g#s/./x"      , "http://a/b/c/g#s/./x")
      resTest("g#s/../x"     , "http://a/b/c/g#s/../x")
      resTest("http:g"       , "http:g")
    }

    it("should provide normalize - examples derived from RFC relativize") {
      expectURI(new URI("http://a/b/c/..").normalize, true, false)(
          scheme = "http",
          host = "a",
          authority = "a",
          path = "/b/",
          schemeSpecificPart = "//a/b/")()

      expectURI(new URI("http://a/b/c/.").normalize, true, false)(
          scheme = "http",
          host = "a",
          authority = "a",
          path = "/b/c/",
          schemeSpecificPart = "//a/b/c/")()
    }

    it("should provide relativize") {
      val x = new URI("http://f%4Aoo@asdf/a")
      val y = new URI("http://fJoo@asdf/a/b/")
      val z = new URI("http://f%4aoo@asdf/a/b/")
      expect(x.relativize(y) eq y).toBeTruthy
      expect(x.relativize(z).toString()).toEqual("b/")

      def relTest(base: String, trg: String, exp: String) =
        expect(new URI(base).relativize(new URI(trg)).toString()).toEqual(exp)

      relTest("http://a.ch/a", "http://a.ch/a/b", "b")
      relTest("http://a.ch/a/", "http://a.ch/a/b", "b")
      relTest("https://a.ch/a", "http://a.ch/a/b", "http://a.ch/a/b")
      relTest("/a/b/c", "/a/b/c/d/e", "d/e")
      relTest("/a/b/c/", "/a/b/c/d/e", "d/e")
      relTest("/a/b/c/", "/a/b/c/foo:e/d", "foo:e/d") // see bug JDK-7037120
      relTest("../a/b", "../a/b/c", "c")
    }

    it("should provide hashCode") {
      expect(new URI("http://example.com/asdf%6a").hashCode).toEqual(
        new URI("http://example.com/asdf%6A").hashCode)
    }

    it("should allow non-ASCII characters") {
      expectURI(new URI("http://cs.dbpedia.org/resource/Víno"), true, false)(
          scheme = "http",
          host = "cs.dbpedia.org",
          path = "/resource/Víno",
          authority = "cs.dbpedia.org",
          schemeSpecificPart = "//cs.dbpedia.org/resource/Víno")()
    }

    it("should decode UTF-8") {
      expectURI(new URI("http://cs.dbpedia.org/resource/V%C3%ADno"), true, false)(
          scheme = "http",
          host = "cs.dbpedia.org",
          path = "/resource/Víno",
          authority = "cs.dbpedia.org",
          schemeSpecificPart = "//cs.dbpedia.org/resource/Víno")(
          rawPath = "/resource/V%C3%ADno",
          rawSchemeSpecificPart = "//cs.dbpedia.org/resource/V%C3%ADno")

      expectURI(new URI("%e3%81%93a%e3%82%93%e3%81%AB%e3%81%a1%e3%81%af"), false, false)(
          path = "こaんにちは",
          schemeSpecificPart = "こaんにちは")(
          rawPath = "%e3%81%93a%e3%82%93%e3%81%AB%e3%81%a1%e3%81%af",
          rawSchemeSpecificPart = "%e3%81%93a%e3%82%93%e3%81%AB%e3%81%a1%e3%81%af")
    }

    it("should support toASCIIString") {
      def cmp(base: String, encoded: String) =
        expect(new URI(base).toASCIIString()).toEqual(encoded)

      cmp("http://cs.dbpedia.org/resource/Víno",
          "http://cs.dbpedia.org/resource/V%C3%ADno")
      cmp("http://こaんにちは/",
          "http://%E3%81%93a%E3%82%93%E3%81%AB%E3%81%A1%E3%81%AF/")
      cmp("foo://bar/\uD800\uDCF5/",
          "foo://bar/%F0%90%83%B5/")
    }

    it("should replace when bad surrogates are present") {
      expectURI(new URI("http://booh/%E3a"), true, false)(
          scheme = "http",
          host = "booh",
          path = "/�a",
          authority = "booh",
          schemeSpecificPart = "//booh/�a")(
          rawPath = "/%E3a",
          rawSchemeSpecificPart = "//booh/%E3a")

      // lowercase e is kept
      expectURI(new URI("http://booh/%e3a"), true, false)(
          scheme = "http",
          host = "booh",
          path = "/�a",
          authority = "booh",
          schemeSpecificPart = "//booh/�a")(
          rawPath = "/%e3a",
          rawSchemeSpecificPart = "//booh/%e3a")

      // %E3%81 is considered as 1 malformed
      expectURI(new URI("http://booh/%E3%81a"), true, false)(
          scheme = "http",
          host = "booh",
          path = "/�a",
          authority = "booh",
          schemeSpecificPart = "//booh/�a")(
          rawPath = "/%E3%81a",
          rawSchemeSpecificPart = "//booh/%E3%81a")

      // %E3%E3 is considered as 2 malformed
      expectURI(new URI("http://booh/%E3%E3a"), true, false)(
          scheme = "http",
          host = "booh",
          path = "/��a",
          authority = "booh",
          schemeSpecificPart = "//booh/��a")(
          rawPath = "/%E3%E3a",
          rawSchemeSpecificPart = "//booh/%E3%E3a")
    }

    it("should throw on bad escape sequences") {
      expect(() => new URI("http://booh/%E")).toThrow
      expect(() => new URI("http://booh/%Ep")).toThrow
    }

  }

}
