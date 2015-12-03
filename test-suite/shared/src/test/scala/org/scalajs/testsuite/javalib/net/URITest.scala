/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.net

import java.net.{URI, URISyntaxException}

import org.junit.Assert._
import org.junit.Test

import org.scalajs.testsuite.utils.AssertThrows._
import org.scalajs.testsuite.utils.Platform.executingInJVM

class URITest {

  def expectURI(uri: URI, isAbsolute: Boolean, isOpaque: Boolean)(
      authority: String = null, fragment: String = null,
      host: String = null, path: String = null, port: Int = -1,
      query: String = null, scheme: String = null, userInfo: String = null,
      schemeSpecificPart: String = null)(rawAuthority: String = authority,
      rawFragment: String = fragment, rawPath: String = path,
      rawQuery: String = query, rawUserInfo: String = userInfo,
      rawSchemeSpecificPart: String = schemeSpecificPart): Unit = {

    assertEquals(authority, uri.getAuthority())
    assertEquals(fragment, uri.getFragment())
    assertEquals(host, uri.getHost())
    assertEquals(path, uri.getPath())
    assertEquals(port, uri.getPort())
    assertEquals(query, uri.getQuery())
    assertEquals(rawAuthority, uri.getRawAuthority())
    assertEquals(rawFragment, uri.getRawFragment())
    assertEquals(rawPath, uri.getRawPath())
    assertEquals(rawQuery, uri.getRawQuery())
    assertEquals(rawSchemeSpecificPart, uri.getRawSchemeSpecificPart())
    assertEquals(rawUserInfo, uri.getRawUserInfo())
    assertEquals(scheme, uri.getScheme())
    assertEquals(schemeSpecificPart, uri.getSchemeSpecificPart())
    assertEquals(userInfo, uri.getUserInfo())
    assertEquals(isAbsolute, uri.isAbsolute())
    assertEquals(isOpaque, uri.isOpaque())
  }

  @Test def should_parse_vanilla_absolute_URIs(): Unit = {
    expectURI(new URI("http://java.sun.com/j2se/1.3/"), true, false)(
        scheme = "http",
        host = "java.sun.com",
        path = "/j2se/1.3/",
        authority = "java.sun.com",
        schemeSpecificPart = "//java.sun.com/j2se/1.3/")()
  }

  @Test def should_parse_absolute_URIs_with_empty_path(): Unit = {
    expectURI(new URI("http://foo:bar"), true, false)(
        authority = "foo:bar",
        path = "",
        scheme = "http",
        schemeSpecificPart = "//foo:bar")()
  }

  @Test def should_parse_absolute_URIs_with_IPv6(): Unit = {
    val uri = new URI("http://hans@[ffff::0:128.4.5.3]:345/~hans/")
    expectURI(uri, true, false)(
        scheme = "http",
        host = "[ffff::0:128.4.5.3]",
        userInfo = "hans",
        port = 345,
        path = "/~hans/",
        authority = "hans@[ffff::0:128.4.5.3]:345",
        schemeSpecificPart = "//hans@[ffff::0:128.4.5.3]:345/~hans/")()
  }

  @Test def should_parse_absolute_URIs_without_authority(): Unit = {
    expectURI(new URI("file:/~/calendar"), true, false)(
        scheme = "file",
        path = "/~/calendar",
        schemeSpecificPart = "/~/calendar")()
  }

  @Test def should_parse_absolute_URIs_with_empty_authority(): Unit = {
    expectURI(new URI("file:///~/calendar"), true, false)(
        scheme = "file",
        path = "/~/calendar",
        schemeSpecificPart = "///~/calendar")()
  }

  @Test def should_parse_opaque_URIs(): Unit = {
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

  @Test def should_parse_relative_URIs(): Unit = {
    expectURI(new URI("docs/guide/collections/designfaq.html#28"), false, false)(
        path = "docs/guide/collections/designfaq.html",
        fragment = "28",
        schemeSpecificPart = "docs/guide/collections/designfaq.html")()
    expectURI(new URI("../../../demo/jfc/SwingSet2/src/SwingSet2.java"), false, false)(
        path = "../../../demo/jfc/SwingSet2/src/SwingSet2.java",
        schemeSpecificPart = "../../../demo/jfc/SwingSet2/src/SwingSet2.java")()
  }

  @Test def should_parse_relative_URIs_with_IPv4(): Unit = {
    expectURI(new URI("//123.5.6.3:45/bar"), false, false)(
        authority = "123.5.6.3:45",
        host = "123.5.6.3",
        port = 45,
        path = "/bar",
        schemeSpecificPart = "//123.5.6.3:45/bar")()
  }

  @Test def should_parse_relative_URIs_with_registry_based_authority(): Unit = {
    expectURI(new URI("//foo:bar"), false, false)(
        authority = "foo:bar",
        path = "",
        schemeSpecificPart = "//foo:bar")()
  }

  @Test def should_parse_relative_URIs_with_escapes(): Unit = {
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

  @Test def should_parse_relative_URIs_with_fragment_only(): Unit = {
    expectURI(new URI("#foo"), false, false)(
        fragment = "foo",
        path = "",
        schemeSpecificPart = "")()
  }

  @Test def should_parse_relative_URIs_with_query_and_fragment(): Unit = {
    expectURI(new URI("?query=1#foo"), false, false)(
        query = "query=1",
        fragment = "foo",
        path = "",
        schemeSpecificPart = "?query=1")()
  }

  @Test def should_provide_compareTo(): Unit = {
    val x = new URI("http://example.com/asdf%6a")
    val y = new URI("http://example.com/asdf%6A")
    val z = new URI("http://example.com/asdfj")
    val rel = new URI("/foo/bar")

    assertTrue(x.compareTo(y) > 0)
    assertTrue(x.compareTo(z) < 0)
    assertTrue(y.compareTo(z) < 0)
    assertEquals(0, x.compareTo(x))
    assertEquals(0, y.compareTo(y))
    assertEquals(0, z.compareTo(z))
    assertTrue(x.compareTo(rel) > 0)
    assertTrue(y.compareTo(rel) > 0)
    assertTrue(z.compareTo(rel) > 0)
    assertEquals(0, rel.compareTo(rel))
  }

  @Test def should_provide_equals(): Unit = {
    val x = new URI("http://example.com/asdf%6a")
    val y = new URI("http://example.com/asdf%6A")
    val z = new URI("http://example.com/asdfj")

    assertTrue(x == y)
    assertFalse(x == z)
    assertFalse(y == z)
    assertTrue(x == x)
    assertTrue(y == y)
    assertTrue(z == z)

    assertNotEquals(new URI("foo:helloWorld%6b%6C"), new URI("foo:helloWorld%6C%6b"))
  }

  @Test def should_provide_normalize(): Unit = {
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
    expectURI(new URI("").normalize, false, false)(
        path = "",
        schemeSpecificPart = "")()

    val x = new URI("http://www.example.com/foo/bar")
    assertTrue(x.normalize eq x)
  }

  @Test def should_provide_resolve__JavaDoc_examples(): Unit = {
    val base = "http://java.sun.com/j2se/1.3/"
    val relative1 = "docs/guide/collections/designfaq.html#28"
    val resolved1 =
      "http://java.sun.com/j2se/1.3/docs/guide/collections/designfaq.html#28"
    val relative2 = "../../../demo/jfc/SwingSet2/src/SwingSet2.java"
    val resolved2 =
      "http://java.sun.com/j2se/1.3/demo/jfc/SwingSet2/src/SwingSet2.java"

    assertEquals(resolved1, new URI(base).resolve(relative1).toString)
    assertEquals(resolved2, new URI(resolved1).resolve(relative2).toString)
    assertEquals("/a/", new URI("").resolve("/a/").toString)
    assertEquals("/a/", new URI("/a/").resolve("").toString)
  }

  @Test def should_provide_resolve_RFC2396_examples(): Unit = {
    val base = new URI("http://a/b/c/d;p?q")
    def resTest(ref: String, trg: String): Unit =
      assertEquals(trg, base.resolve(ref).toString)

    // Normal examples
    resTest("g:h", "g:h")
    resTest("g", "http://a/b/c/g")
    resTest("./g", "http://a/b/c/g")
    resTest("g/", "http://a/b/c/g/")
    resTest("/g", "http://a/g")
    resTest("//g", "http://g")
    resTest("?y", "http://a/b/c/?y")
    resTest("g?y", "http://a/b/c/g?y")
    resTest("#s", "http://a/b/c/d;p?q#s")
    resTest("g#s", "http://a/b/c/g#s")
    resTest("g?y#s", "http://a/b/c/g?y#s")
    resTest(";x", "http://a/b/c/;x")
    resTest("g;x", "http://a/b/c/g;x")
    resTest("g;x?y#s", "http://a/b/c/g;x?y#s")
    resTest(".", "http://a/b/c/")
    resTest("./", "http://a/b/c/")
    resTest("..", "http://a/b/")
    resTest("../", "http://a/b/")
    resTest("../g", "http://a/b/g")
    resTest("../..", "http://a/")
    resTest("../../", "http://a/")
    resTest("../../g", "http://a/g")

    // Abnormal examples
    resTest("../../../g", "http://a/../g")
    resTest("../../../../g", "http://a/../../g")
    resTest("/./g", "http://a/./g")
    resTest("/../g", "http://a/../g")
    resTest("g.", "http://a/b/c/g.")
    resTest(".g", "http://a/b/c/.g")
    resTest("g..", "http://a/b/c/g..")
    resTest("..g", "http://a/b/c/..g")
    resTest("./../g", "http://a/b/g")
    resTest("./g/.", "http://a/b/c/g/")
    resTest("g/./h", "http://a/b/c/g/h")
    resTest("g/../h", "http://a/b/c/h")
    resTest("g;x=1/./y", "http://a/b/c/g;x=1/y")
    resTest("g;x=1/../y", "http://a/b/c/y")
    resTest("g?y/./x", "http://a/b/c/g?y/./x")
    resTest("g?y/../x", "http://a/b/c/g?y/../x")
    resTest("g#s/./x", "http://a/b/c/g#s/./x")
    resTest("g#s/../x", "http://a/b/c/g#s/../x")
    resTest("http:g", "http:g")
  }

  @Test def should_provide_resolve_when_authority_is_empty__issue_2048(): Unit = {
    val base = new URI("http://foo/a")
    def resTest(ref: String, trg: String): Unit =
      assertEquals(trg, base.resolve(ref).toString)

    resTest("///a", "http://foo/a")
    resTest("/b", "http://foo/b")
    resTest("/b/../d", "http://foo/b/../d")
  }

  @Test def should_provide_normalize__examples_derived_from_RFC_relativize(): Unit = {
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

  @Test def should_provide_relativize(): Unit = {
    val x = new URI("http://f%4Aoo@asdf/a")
    val y = new URI("http://fJoo@asdf/a/b/")
    val z = new URI("http://f%4aoo@asdf/a/b/")
    assertTrue(x.relativize(y) eq y)
    assertEquals("b/", x.relativize(z).toString())

    def relTest(base: String, trg: String, exp: String): Unit =
      assertEquals(exp, new URI(base).relativize(new URI(trg)).toString())

    relTest("http://a.ch/a", "http://a.ch/a/b", "b")
    relTest("http://a.ch/a/", "http://a.ch/a/b", "b")
    relTest("https://a.ch/a", "http://a.ch/a/b", "http://a.ch/a/b")
    relTest("/a/b/c", "/a/b/c/d/e", "d/e")
    relTest("/a/b/c/", "/a/b/c/d/e", "d/e")
    relTest("/a/b/c/", "/a/b/c/foo:e/d", "foo:e/d") // see bug JDK-7037120
    relTest("../a/b", "../a/b/c", "c")
    relTest("../a/b", "", "")
    relTest("", "../a/b", "../a/b")
    relTest("file:///a", "file:///a/b/", "b/")
    relTest("file:/c", "file:///c/d/", "d/")
  }

  @Test def should_provide_hashCode(): Unit = {
    if (!executingInJVM) { // Fails on JDK6 and JDK7
      assertEquals(new URI("http://example.com/asdf%6a").hashCode,
          new URI("http://example.com/asdf%6A").hashCode)
    }
  }

  @Test def should_allow_non_ASCII_characters(): Unit = {
    expectURI(new URI("http://cs.dbpedia.org/resource/Víno"), true, false)(
        scheme = "http",
        host = "cs.dbpedia.org",
        path = "/resource/Víno",
        authority = "cs.dbpedia.org",
        schemeSpecificPart = "//cs.dbpedia.org/resource/Víno")()
  }

  @Test def should_decode_UTF_8(): Unit = {
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

  @Test def should_support_toASCIIString(): Unit = {
    def cmp(base: String, encoded: String): Unit =
      assertEquals(encoded, new URI(base).toASCIIString())

    cmp("http://cs.dbpedia.org/resource/Víno",
        "http://cs.dbpedia.org/resource/V%C3%ADno")
    cmp("http://こaんにちは/",
        "http://%E3%81%93a%E3%82%93%E3%81%AB%E3%81%A1%E3%81%AF/")
    cmp("foo://bar/\uD800\uDCF5/",
        "foo://bar/%F0%90%83%B5/")
  }

  @Test def should_replace_when_bad_surrogates_are_present(): Unit = {
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

    if (!executingInJVM) { // Fails on JDK6 and JDK7
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
  }

  @Test def should_throw_on_bad_escape_sequences(): Unit = {
    expectThrows(classOf[URISyntaxException], new URI("http://booh/%E"))
    expectThrows(classOf[URISyntaxException], new URI("http://booh/%Ep"))
  }
}
