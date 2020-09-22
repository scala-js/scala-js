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

import java.io._
import java.nio.charset.StandardCharsets
import java.{util => ju}
import java.util.Properties

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._

import org.scalajs.testsuite.utils.AssertThrows._
import org.scalajs.testsuite.utils.Platform._

import Utils._

class PropertiesTest {

  @Test def setProperty(): Unit = {
    val prop = new Properties()
    prop.setProperty("a", "A")
    assertEquals("A", prop.get("a"))
    prop.setProperty("a", "AA")
    prop.setProperty("b", "B")
    assertEquals("AA", prop.get("a"))
    assertEquals("B", prop.get("b"))

    val prop2 = new Properties(prop)
    prop2.setProperty("a", "AAA")
    assertEquals("AAA", prop2.get("a"))
  }

  @Test def getProperty(): Unit = {
    val prop = new Properties()

    assertNull(prop.getProperty("a"))
    prop.setProperty("a", "A")
    assertEquals("A", prop.getProperty("a"))
    assertNull(prop.getProperty("aa"))

    assertEquals("A", prop.getProperty("a", "B"))
    assertEquals("B", prop.getProperty("b", "B"))

    // Tests with default properties
    prop.setProperty("b", "B")

    val prop2 = new Properties(prop)
    prop2.setProperty("b", "BB")
    prop2.setProperty("c", "C")
    assertEquals("A", prop2.getProperty("a"))
    assertEquals("BB", prop2.getProperty("b"))
    assertEquals("C", prop2.getProperty("c"))
  }

  @Test def propertyNames(): Unit = {
    val prop = new Properties()
    assertTrue(enumerationIsEmpty(prop.propertyNames()))
    prop.setProperty("a", "A")
    prop.setProperty("b", "B")
    prop.setProperty("c", "C")
    assertEquals(3, enumerationSize(prop.propertyNames()))
    assertEnumSameElementsAsSet[Any]("a", "b", "c")(prop.propertyNames())

    val prop2 = new Properties(prop)
    prop.setProperty("c", "CC")
    prop.setProperty("d", "D")
    assertEquals(4, enumerationSize(prop2.propertyNames()))
    assertEnumSameElementsAsSet[Any]("a", "b", "c", "d")(prop2.propertyNames())
  }

  @Test def propertyNamesIsNotAffectedByOverriddenPropertyNamesInDefaults(): Unit = {
    val defaults = new java.util.Properties {
      override def propertyNames(): ju.Enumeration[_] =
        ju.Collections.emptyEnumeration[String]()
    }
    defaults.setProperty("foo", "bar")

    val props = new Properties(defaults)
    props.setProperty("foobar", "babar")
    assertEnumSameElementsAsSet[Any]("foo", "foobar")(props.propertyNames())
  }

  @Test def propertyNamesWithBadContents(): Unit = {
    assumeTrue("Assumed compliant asInstanceOf", hasCompliantAsInstanceOfs)

    val prop = new Properties()
    prop.setProperty("a", "A")
    prop.setProperty("b", "B")
    prop.setProperty("c", "C")

    prop.put(1.asInstanceOf[AnyRef], "2")
    assertThrows(classOf[Throwable], prop.propertyNames())
    prop.remove(1.asInstanceOf[AnyRef])

    prop.put("1", 1.asInstanceOf[AnyRef])
    assertEnumSameElementsAsSet[Any]("a", "b", "c", "1")(prop.propertyNames())
    prop.remove("1")

    val prop2 = new Properties(prop)
    prop.setProperty("c", "CC")
    prop.setProperty("d", "D")

    prop2.put(1.asInstanceOf[AnyRef], "2")
    assertThrows(classOf[Throwable], prop2.propertyNames())
    prop2.remove(1.asInstanceOf[AnyRef])

    prop2.put("1", 1.asInstanceOf[AnyRef])
    assertEnumSameElementsAsSet[Any]("a", "b", "c", "d", "1")(prop2.propertyNames())
  }

  @Test def stringPropertyNames(): Unit = {
    val prop = new Properties()
    assertEquals(0, prop.stringPropertyNames().size)
    prop.setProperty("a", "A")
    prop.setProperty("b", "B")
    prop.setProperty("c", "C")
    assertEquals(3, prop.stringPropertyNames().size)
    assertCollSameElementsAsSet("a", "b", "c")(prop.stringPropertyNames())

    val prop2 = new Properties(prop)
    prop.setProperty("c", "CC")
    prop.setProperty("d", "D")
    assertEquals(4, prop2.stringPropertyNames().size)
    assertCollSameElementsAsSet("a", "b", "c", "d")(prop2.stringPropertyNames())
  }

  @Test def stringPropertyNamesIsNotAffectedByOverriddenStringPropertyNamesInDefaults(): Unit = {
    val defaults = new java.util.Properties {
      override def stringPropertyNames(): ju.Set[String] =
        ju.Collections.emptySet[String]()
    }
    defaults.setProperty("foo", "bar")

    val props = new Properties(defaults)
    props.setProperty("foobar", "babar")
    assertCollSameElementsAsSet("foo", "foobar")(props.stringPropertyNames())
  }

  @Test def stringPropertyNamesWithBadContents(): Unit = {
    assumeTrue("Assumed compliant asInstanceOf", hasCompliantAsInstanceOfs)

    val prop = new Properties()
    prop.setProperty("a", "A")
    prop.setProperty("b", "B")
    prop.setProperty("c", "C")

    prop.put(1.asInstanceOf[AnyRef], "2")
    assertCollSameElementsAsSet("a", "b", "c")(prop.stringPropertyNames())
    prop.remove(1.asInstanceOf[AnyRef])

    prop.put("1", 1.asInstanceOf[AnyRef])
    assertCollSameElementsAsSet("a", "b", "c")(prop.stringPropertyNames())
    prop.remove("1")

    val prop2 = new Properties(prop)
    prop.setProperty("c", "CC")
    prop.setProperty("d", "D")

    prop2.put(1.asInstanceOf[AnyRef], "2")
    assertCollSameElementsAsSet("a", "b", "c", "d")(prop2.stringPropertyNames())
    prop2.remove(1.asInstanceOf[AnyRef])

    prop2.put("1", 1.asInstanceOf[AnyRef])
    assertCollSameElementsAsSet("a", "b", "c", "d")(prop2.stringPropertyNames())
  }

  // ported from Harmony

  @Test def nonStringValues(): Unit = {
    assumeTrue("Assumed compliant asInstanceOf", hasCompliantAsInstanceOfs)
    val properties = new Properties

    properties.put("age", new Integer(18))
    assertNull(properties.getProperty("age"))
    assertThrows(classOf[ClassCastException],
        properties.list(new PrintWriter(new ByteArrayOutputStream())))
  }

  @Test def list(): Unit = {

    def assertResult(props: Properties, result: String): Unit = {
      val out = new ByteArrayOutputStream()
      val ps = new PrintStream(out)
      props.list(ps)
      ps.close()
      assertEquals(out.toString.trim, result.trim)
    }

    assertResult(new Properties(), "-- listing properties --\n")

    val prop1 = new Properties()
    prop1.put("name", "alice")
    val result1 =
      """
        |-- listing properties --
        |name=alice
      """.stripMargin
    assertResult(prop1, result1)

    // Undocumented feature
    // https://bugs.java.com/bugdatabase/view_bug.do?bug_id=5089823
    // https://bugs.java.com/bugdatabase/view_bug.do?bug_id=4622226
    val prop2 = new Properties()
    prop2.put("k40", "v0000000001111111111222222222233333333334")
    val result2 =
      """-- listing properties --
        |k40=v000000000111111111122222222223333333...
      """.stripMargin
    assertResult(prop2, result2)

    val prop3 = new Properties()
    prop3.put("k0000000001111111111222222222233333333334", "v40")
    val result3 =
      """-- listing properties --
        |k0000000001111111111222222222233333333334=v40
      """.stripMargin
    assertResult(prop3, result3)
  }

  private val dummyProps =
    """#commented.key=dummy_value
      |key1=value1
      |key2=value1
      |""".stripMargin

  @Test def loadInputStream(): Unit = {
    val prop = loadStream(dummyProps)

    assertEquals("value1", prop.getProperty("key1"))
    assertNull(prop.getProperty("commented.key"))
    assertEquals("default_value",
        prop.getProperty("commented.key", "default_value"))
  }

  @Test def loadInputStreamForEmptyKeys(): Unit = {
    val prop = loadStream("=")
    assertEquals("", prop.get(""))

    val prop1 = loadStream(" = ")
    assertEquals("", prop1.get(""))
  }

  @Test def loadInputStreamHandleWhitespace(): Unit = {
    val prop = loadStream(" a= b")
    assertEquals("b", prop.get("a"))

    val prop1 = loadStream(" a b")
    assertEquals("b", prop1.get("a"))
  }

  @Test def loadInputStreamHandleSpecialChars(): Unit = {
    val prop = loadStream("#\u008d\u00d2\na=\u008d\u00d3")
    assertEquals("\u008d\u00d3", prop.get("a"))

    val prop1 = loadStream("#properties file\r\nfred=1\r\n#last comment")
    assertEquals("1", prop1.get("fred"))
  }

  def checkLoadFromFile(prop: Properties): Unit = {
    assertEquals("\n \t \f", prop.getProperty(" \r"))
    assertEquals("a", prop.getProperty("a"))
    assertEquals("bb as,dn   ", prop.getProperty("b"))
    assertEquals(":: cu", prop.getProperty("c\r \t\nu"))
    assertEquals("bu", prop.getProperty("bu"))
    assertEquals("d\r\ne=e", prop.getProperty("d"))
    assertEquals("fff", prop.getProperty("f"))
    assertEquals("g", prop.getProperty("g"))
    assertEquals("", prop.getProperty("h h"))
    assertEquals("i=i", prop.getProperty(" "))
    assertEquals("   j", prop.getProperty("j"))
    assertEquals("   c", prop.getProperty("space"))
    assertEquals("\\", prop.getProperty("dblbackslash"))
    // added for new implementation
    assertEquals("foo,   ", prop.getProperty("trailing"))
    assertEquals("", prop.getProperty("bar"))
    assertEquals("""baz \  """, prop.getProperty("notrailing"))
  }

  @Test def loadInputStreamWithFileInput(): Unit = {
    // String input for Scala.js
    val is = new ByteArrayInputStream(filestr.getBytes())
    val prop = new Properties()
    prop.load(is)
    is.close()
    checkLoadFromFile(prop)
  }

  @Test def loadReader(): Unit = {
    val prop = loadReader(dummyProps)

    assertEquals("value1", prop.getProperty("key1"))
    assertNull(prop.getProperty("commented.key"))
    assertEquals("default_value",
        prop.getProperty("commented.key", "default_value"))
  }

  @Test def loadReaderHandleSpecialChars(): Unit = {
    val prop = loadReader("#\u008d\u00d2\na=\u008d\u00d3")
    assertEquals("\u008d\u00d3", prop.get("a"))

    val prop1 = loadReader("#properties file\r\nfred=1\r\n#last comment")
    assertEquals("1", prop1.get("fred"))
  }

  @Test def loadReaderWithFileInput(): Unit = {
    // string input for Scala.js
    val is = new ByteArrayInputStream(filestr.getBytes())
    val prop = new Properties()
    prop.load(new InputStreamReader(is))
    is.close()
    checkLoadFromFile(prop)
  }

  @Test def storeOutputStreamCommentsLoadInputStreamRoundtrip(): Unit = {
    val prop1 = new Properties()
    prop1.put("Property A", " aye\\\f\t\n\r\b")
    prop1.put("Property B", "b ee#!=:")
    prop1.put("Property C", "see")
    val header1 =
      "A Header\rLine2\nLine3\r\nLine4\n!AfterExclaim\r\n#AfterPound\nWow!"
    val out1 = storeStream(prop1, header1)

    val prop2 = loadByteArrayOutputStream(out1)
    assertAll(expected = prop1, actual = prop2)
    // Avoid variable Date output which is last line in comment
    // Matches JVM output
    val commentsWithoutDate =
      """|#A Header
         |#Line2
         |#Line3
         |#Line4
         |!AfterExclaim
         |#AfterPound
         |#Wow!""".stripMargin
    assertTrue(out1.toString().startsWith(commentsWithoutDate))
  }

  @Test def checkPropertiesFormattedCorrectly(): Unit = {
    val prop1 = new Properties()
    prop1.put("Property C", "see")
    val out1 = storeStream(prop1)
    assertTrue(out1.toString().trim().endsWith("""Property\ C=see"""))

    val prop2 = new Properties()
    prop2.put("Property B", "b ee#!=:")
    val out2 = storeStream(prop2)
    assertTrue(out2.toString().trim().endsWith("""Property\ B=b ee\#\!\=\:"""))

    val prop3 = new Properties()
    prop3.put("Property A", " aye\\\f\t\n\r\b")
    val out3 = storeStream(prop3)
    // JVM outputs \b as \u0008 and you can't just add \u0008 to the end
    val result = new StringBuilder("""Property\ A=\ aye\\\f\t\n\r""")
      .appendAll(Array('\\', 'u', '0', '0', '0', '8'))
    assertTrue(out3.toString().trim().endsWith(result.toString))
  }

  @Test def storeWriterComments(): Unit = {
    val prop1 = new Properties()
    prop1.put("Property A", " aye\\\f\t\n\r\b")
    prop1.put("Property B", "b ee#!=:")
    prop1.put("Property C", "see")
    val out = storeWriter(prop1, "A Header")
    val prop2 = loadByteArrayOutputStream(out)
    assertAll(prop1, prop2)
  }

  @Test def checkUnicodeParsing(): Unit = {
    val is = new ByteArrayInputStream(
      Array('h', '\\', 'u', '0', '0', '2', '0', 'h'))
    val prop = new Properties()
    prop.load(is)
    assertEquals("", prop.get("h h"))
  }

  // helper functions

  def storeStream(props: Properties,
      header: String = ""): ByteArrayOutputStream = {
    val out = new ByteArrayOutputStream()
    props.store(out, header)
    out
  }

  def storeWriter(props: Properties,
      header: String = ""): ByteArrayOutputStream = {
    val out = new ByteArrayOutputStream()
    props.store(new OutputStreamWriter(out), header)
    out.close()
    out
  }

  def loadByteArrayOutputStream(out: ByteArrayOutputStream): Properties = {
    val props = new Properties()
    props.load(new ByteArrayInputStream(out.toByteArray))
    props
  }

  def assertAll(expected: Properties, actual: Properties): Unit = {
    val e = expected.propertyNames()
    while (e.hasMoreElements) {
      val nextKey = e.nextElement().asInstanceOf[String]
      assertEquals(actual.getProperty(nextKey), expected.getProperty(nextKey))
    }
  }

  def loadStream(in: String): Properties = {
    val prop = new java.util.Properties()
    prop.load(new ByteArrayInputStream(
        in.getBytes(StandardCharsets.ISO_8859_1)))
    prop
  }

  def loadReader(in: String): Properties = {
    val prop = new java.util.Properties()
    prop.load(new InputStreamReader(new ByteArrayInputStream(
        in.getBytes(StandardCharsets.UTF_8))))
    prop
  }

  // scalastyle doesn't like embedded tabs or trailing spaces in the string
  lazy val filestr = {
    raw"""
      |
      |   \ \r \n \t \f
      |
      |! dshfjklahfjkldashgjl;as
      |     #jdfagdfjagkdjfghksdajfd
      |
      |!!properties
      |
      |a=a
      |b bb as,dn${"   "}
      |c\r\ \t\nu =:: cu
      |bu= b\
      |${"\t\t"}u
      |d=d\r\ne=e
      |f   :f\
      |f\
      |${"\t\t\t"}f
      |g${"\t\t"}g
      |h\ h
      |\   i=i
      |j=\   j
      |space=\   c
      |
      |dblbackslash=\\
      |
      |# jvm does not trim trailing space so no line continuation
      |trailing = foo, \${"  "}
      |bar
      |notrailing = baz \\${"  "}
      |
    """.stripMargin
  }
}
