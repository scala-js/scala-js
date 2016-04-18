package org.scalajs.testsuite.compiler

import org.junit.Assert._
import org.junit.Assume._
import org.junit.{BeforeClass, Test}

import org.scalajs.testsuite.utils.Platform._

/** The test counter */
private[testsuite] object TC {
  var testNum: Int = _

  def is(x: Int): Boolean = testNum == x
}

/** Exception to test source maps. Not a ControlThrowable, because it has
 *  NoStackTrace which would defeat its purpose
 */
case class TestException(lineNo: Int) extends Exception

/**
 * Template to generate source-map tests, verifying that the line numbers
 * reported in the source-mapped stacktraces match up with the line number
 * that the error originated from.
 *
 * /two-star/s in this file are replaced with a code-snippet to throw
 * an exception if `testNum` is set to the /two-star/'s index. The
 * exception is then caught and its stacktrace checked to see
 * that it reports the line number expected (stored in the error
 * message). /three-star/s in are replaced with a dangling else (to
 * allow throwing in expression position).
 * `@Test def workTest(): Unit = ...` is replaced by the list of all tests
 * (in a single line) up to the number of /n-star/s in the file.
 */
object SourceMapTest {
  @BeforeClass def beforeClass(): Unit = {
    assumeTrue("Assumed source-maps", sourceMaps)
  }
}

class SourceMapTest {

  @Test def workTest(): Unit = sys.error("stubs")

  def test(i: Int): Unit = {
    TC.testNum = i

    try {
      run()
      sys.error("No exception thrown")
    } catch {
      case e @ TestException(lineNo) =>
        def normFileName(e: StackTraceElement): String =
          e.getFileName.replace('\\', '/')

        val trace0 = e.getStackTrace.toList
        val trace1 = trace0.dropWhile(
          normFileName(_).endsWith("/scala/scalajs/runtime/StackTrace.scala"))
        val trace2 = trace1.dropWhile(
          normFileName(_).endsWith("/java/lang/Throwables.scala"))

        val topSte = trace2.head
        assertTrue(normFileName(topSte).contains("/SourceMapTest.scala"))

        val throwSte = if (topSte.getLineNumber == 19) {
          // line where `case class TestException is written` above
          val throwSte = trace2.tail.head
          assertTrue(normFileName(throwSte).contains("/SourceMapTest.scala"))
          throwSte
        } else {
          /* In fullOpt, it may happen that the constructor of
           * TestException is inlined, in which case there is no trace of
           * it anymore. The first stack element in SourceMapTest.scala is
           * therefore the one we're interested in.
           */
          topSte
        }

        assertEquals(lineNo, throwSte.getLineNumber)
    }
  }

  def get(json: JsValue, index: Int) = {
    /**/
    /**/json.asInstanceOf[JsArray].value(index).value
  }

  def get(json: JsValue, index: Int, fieldName: String) = {
    /**/
    /**//***/json.asInstanceOf[JsArray].value(index).asInstanceOf[JsObject].value(fieldName).value
  }
  def run() = {
    /**/
    /**/val ugly =
      """
        |[
        |    "JSON Test Pattern pass1",
        |    {"object with 1 member":["array with 1 element"]},
        |    {},
        |    [],
        |    -42,
        |    true,
        |    false,
        |    null,
        |    {
        |        "integer": 1234567890,
        |        "real": -9876.543210,
        |        "e": 0.123456789e-12,
        |        "E": 1.234567890E+34,
        |        "":  23456789012E66,
        |        "zero": 0,
        |        "one": 1,
        |        "space": " ",
        |        "quote": "\"",
        |        "backslash": "\\",
        |        "controls": "\b\f\n\r\t",
        |        "slash": "/ & \/",
        |        "alpha": "abcdefghijklmnopqrstuvwyz",
        |        "ALPHA": "ABCDEFGHIJKLMNOPQRSTUVWYZ",
        |        "digit": "0123456789",
        |        "0123456789": "digit",
        |        "special": "`1~!@#$%^&*()_+-={':[,]}|;.</>?",
        |        "hex": "\u0123\u4567\u89AB\uCDEF\uabcd\uef4A",
        |        "true": true,
        |        "false": false,
        |        "null": null,
        |        "array":[  ],
        |        "object":{  },
        |        "address": "50 St. James Street",
        |        "url": "http://www.JSON.org/",
        |        "comment": "// /* <!-- --",
        |        "# -- --> */": " ",
        |        " s p a c e d " :[1,2 , 3
        |
        |,
        |
        |4 , 5        ,          6           ,7        ],"compact":[1,2,3,4,5,6,7],
        |        "jsontext": "{\"object with 1 member\":[\"array with 1 element\"]}",
        |        "quotes": "&#34; \u005Cu0022 %22 0x22 034 &#x22;",
        |        "\/\\\"\uCAFE\uBABE\uAB98\uFCDE\ubcda\uef4A\b\f\n\r\t`1~!@#$%^&*()_+-=[]{}|;:',./<>?"
        |: "A key can be any string"
        |    },
        |    0.5 ,98.6
        |,
        |99.44
        |,
        |
        |1066,
        |1e1,
        |0.1e1,
        |1e-1,
        |1e00,2e+00,2e-00
        |,"rosebud"]
      """.stripMargin/**/
    /**/val json = new Json()/**/
    /**/val parsed = json.read(ugly)/**/
    /**/
    /**/val unparsed = json.write(parsed)/**/
    /**/val reparsed = json.read(unparsed)/**/

    for (json <- Seq(parsed, reparsed)){/**/
      assert(get(json, 0) == "JSON Test Pattern pass1")
      /**/
      assert(get(json, 5) == true)
      assert(get(json, 6) == false)
      assert(get(json, 8, "real") == "-9876.543210")/**/
      /**/assert(get(json, 8, "comment") == "// /* <!-- --")
      assert(get(json, 8, "jsontext") == "{\"object with 1 member\":[\"array with 1 element\"]}")
      assert(get(json, 19) == "rosebud")/**/
    }
    /**/
  }
}

sealed trait JsValue {
  def value: Any
}

case class JsString(value: java.lang.String) extends JsValue

case class JsObject(value: Map[String, JsValue]) extends JsValue

case class JsArray(value: Seq[JsValue]) extends JsValue

case class JsNumber(value: java.lang.String) extends JsValue

sealed trait JsBoolean extends JsValue {
  def value: Boolean
}

case object JsFalse extends JsBoolean {
  def value = {/**/false}
}

case object JsTrue extends JsBoolean {
  def value = {/**/true}
}

case object JsNull extends JsValue {
  def value = {null}
}

trait Writer{
  /**/
  def writeToBuffer(v: JsValue, sb: StringBuffer): Unit = v match {
    case JsString(s) =>
      /**/sb.append('"')/**/
    /**/var i = 0/**/
      while(i < s.length){/**/
        /**/s.charAt(i) match {
          case '\\' => /**/sb.append("\\\\")/**/
          case '"' => sb.append("\\\"")
          case '/' => /**/sb.append("\\/")/**/
          case '\b' => sb.append("\\b")
          case '\t' => sb.append("\\t")
          case '\n' => /**/sb.append("\\n")/**/
          case '\f' => sb.append("\\f")
          case '\r' => sb.append("\\r")
          case c =>
            if (c < ' '){
              val t = "000" + Integer.toHexString(c)
              sb.append("\\u" + t.takeRight(4))
            }else{
              sb.append(c.toString)
            }
        }
        i += 1
      }
      /**/
      sb.append('"')
    /**/
    case JsObject(kvs) =>
      /**/
      sb.append("{")
      /**/
      var first = true
      kvs.foreach(kv => {
        /**/
        val (k, v) = kv
        if (first)
          first = false
        else
          sb.append(", ")

        /**/
        writeToBuffer(JsString(k), sb)
        sb.append(": ")
        /**/
        writeToBuffer(v, sb)
      })
      sb.append("}")

    case JsArray(vs) => /**/
      sb.append("[")
      if (vs.length > 0) writeToBuffer(vs(0), sb)
      var i = 1
      while(i < vs.length){
        sb.append(", ")
        writeToBuffer(vs(i), sb)
        i += 1
      }
      sb.append("]")
    case JsNumber(d) => sb.append(d)
    case JsFalse => sb.append("false")
    case JsTrue => sb.append("true")
    case JsNull => sb.append("null")
  }
  /**/
}
class Writer2 extends Writer{
  /**/
  def write(v: JsValue): String = {
    /**/
    val sb = new StringBuffer()
    writeToBuffer(v, sb)
    sb.toString
  }
  /**/
}
class Json extends Writer2{

  /**
   * Self-contained JSON parser adapted from
   *
   * https://github.com/nestorpersist/json
   */
  def read(s: String): JsValue = {

    // *** Character Kinds
    /**/
    type CharKind = Int
    val Letter = 0
    val Digit = 1
    val Minus = 2
    val Quote = 3
    val Colon = 4
    val Comma = 5
    val Lbra = 6
    val Rbra = 7
    val Larr = 8
    val Rarr = 9
    val Blank = 10
    val Other = 11
    val Eof = 12
    val Slash = 13

    // *** Token Kinds

    type TokenKind = Int
    val ID = 0
    val STRING = 1
    val NUMBER = 2
    val BIGNUMBER = 3
    val FLOATNUMBER = 4
    val COLON = 5
    val COMMA = 6
    val LOBJ = 7
    val ROBJ = 8
    val LARR = 9
    val RARR = 10
    val BLANK = 11
    val EOF = 12
    /**/
    // *** Character => CharKind Map ***

    val charKind = (0 to 255).toArray.map {
      case c if 'a'.toInt <= c && c <= 'z'.toInt => Letter
      case c if 'A'.toInt <= c && c <= 'Z'.toInt => Letter
      case c if '0'.toInt <= c && c <= '9'.toInt => Digit
      case '-' => /**/Minus
      case ',' => /**/Comma
      case '"' => /**/Quote
      case ':' => /**/Colon
      case '{' => /**/Lbra
      case '}' => Rbra
      case '[' => Larr
      case ']' => Rarr
      case ' ' => Blank
      case '\t' => Blank
      case '\n' => Blank
      case '\r' => Blank
      case '/' => Slash
      case _ => Other
    }

    // *** Character Escapes
    /**/
    val escapeMap = Map[Int, String](
      '\\'.toInt -> "\\",
      '/'.toInt -> "/",
      '\"'.toInt -> "\"",
      'b'.toInt -> "\b",
      'f'.toInt -> "\f",
      'n'.toInt -> "\n",
      'r'.toInt -> "\r",
      't'.toInt -> "\t"
    )
    // *** Import Shared Data ***

    // *** INPUT STRING ***

    // array faster than accessing string directly using charAt
    //final  val s1 = s.toCharArray()
    val size = s.size

    // *** CHARACTERS ***

    var pos = 0

    var ch: Int = 0
    var chKind: CharKind = 0
    var chLinePos: Int = 0
    var chCharPos: Int = 0

    def chNext() = {/**/
      if (pos < size) {/**/
        //ch = s1(pos).toInt
        /**/ch = s.charAt(pos)/**/
        /**/chKind = /***/if (ch < 255) {/**/
          /**//***/charKind(ch)
          } else {/**/
          /**//***/Other
          }/**/
        pos += 1
        if (ch == '\n'.toInt) {
          chLinePos += 1
          chCharPos = 1
        } else {/**/
          chCharPos += 1/**/
        }
      } else {
        ch = -1
        pos = size + 1
        chKind = Eof
      }
    }/**/
    /**/
    /**/
    /**/def chError(msg: String): Nothing = {
      throw new Json.Exception(msg, s, chLinePos, chCharPos)
    }

    def chMark = pos - 1

    def chSubstr(first: Int, delta: Int = 0) = {
      s.substring(first, pos - 1 - delta)
    }

    // *** LEXER ***

    var tokenKind = BLANK
    var tokenValue = ""
    var linePos = 1
    var charPos = 1

    def getDigits() = {
      while (chKind == Digit) chNext()
    }

    def handleDigit() {
      val first = chMark
      getDigits()
      val k1 = if (ch == '.'.toInt) {
        chNext()
        getDigits()
        BIGNUMBER
      } else {
        NUMBER
      }
      val k2 = if (ch == 'E'.toInt || ch == 'e'.toInt) {
        chNext()
        if (ch == '+'.toInt) {
          chNext()
        } else if (ch == '-'.toInt) {
          chNext()
        }
        getDigits()
        FLOATNUMBER
      } else {
        k1
      }
      /**/tokenKind = k2/**/
      /**/tokenValue = chSubstr(first)/**/
      /**/}/**/
    /**/
    def handleRaw() {
      chNext()
      val first = chMark
      var state = 0
      do {
        if (chKind == Eof) chError("EOF encountered in raw string")
        state = (ch, state) match {
          case ('}', _) => 1
          case ('"', 1) => 2
          case ('"', 2) => 3
          case ('"', 3) => 0
          case _ => 0
        }

        chNext()
      } while (state != 3)
      tokenKind = STRING
      tokenValue = chSubstr(first, 3)
    }

    def handle(i: Int) = {
      chNext()
      tokenKind = i
      tokenValue = ""
    }

    def tokenNext() {
      do {
        linePos = chLinePos
        charPos = chCharPos
        val kind: Int = chKind
        kind match {
          case Letter =>
            val first = chMark
            while (chKind == Letter || chKind == Digit) {
              chNext()
            }
            tokenKind = ID
            tokenValue = chSubstr(first)

          case Digit => handleDigit()
          case Minus =>
            chNext()
            handleDigit()
            tokenValue = "-" + tokenValue

          case Quote =>
            val sb = new StringBuilder(50)
            chNext()
            var first = chMark
            while (ch != '"'.toInt && ch >= 32) {
              if (ch == '\\'.toInt) {
                sb.append(chSubstr(first))
                chNext()
                escapeMap.get(ch) match {
                  case Some(s) =>
                    sb.append(s)
                    chNext()

                  case None =>
                    if (ch != 'u'.toInt) chError("Illegal escape")
                    chNext()
                    var code = 0
                    for (i <- 1 to 4) {
                      val ch1 = ch.toChar.toString
                      val i = "0123456789abcdef".indexOf(ch1.toLowerCase)
                      if (i == -1) chError("Illegal hex character")
                      code = code * 16 + i
                      chNext()
                    }
                    sb.append(code.toChar.toString)
                }
                first = chMark
              } else {
                chNext()
              }
            }
            if (ch != '"') chError("Unexpected string character: " + ch.toChar)

            sb.append(chSubstr(first))

            tokenKind = STRING

            tokenValue = sb.toString()
            chNext()
            if (tokenValue.length() == 0 && ch == '{') {
              handleRaw()
            }

          case Colon => handle(COLON)/**/
          case Comma => handle(COMMA)/**/
          case Lbra => handle(LOBJ)/**/
          case Rbra => handle(ROBJ)/**/
          case Larr => handle(LARR)/**/
          case Rarr => handle(RARR)/**/
          case Blank =>
            do chNext() while (chKind == Blank)
            tokenKind = BLANK
            tokenValue = ""

          case Other => chError("Unexpected character: " + ch.toChar + " " + ch)
          case Eof =>
            chNext()
            tokenKind = EOF
            tokenValue = ""

          case Slash =>
            if (chKind != Slash) chError("Expecting Slash")
            do chNext() while (ch != '\n' && chKind != Eof)
            tokenKind = BLANK
            tokenValue = ""

        }
      } while (tokenKind == BLANK)
    }
    /**/
    def tokenError(msg: String): Nothing = {
      throw new Json.Exception(msg, s, linePos, charPos)
    }
    /**/
    // *** PARSER ***

    def handleEof() = tokenError("Unexpected eof")
    def handleUnexpected(i: String) = tokenError(s"Unexpected input: [$i]")

    def handleArray(): JsArray = {
      tokenNext()
      var result = List.empty[JsValue]
      while (tokenKind != RARR) {/**/
        result = getJson() :: result
        /**/tokenKind match{
          case COMMA => /**/tokenNext()
          case RARR => // do nothing
          case _ => tokenError("Expecting , or ]")
        }
      }
      tokenNext()
      JsArray(result.reverse)
    }

    def handleObject(): JsObject = {
      tokenNext()
      var result = List.empty[(String, JsValue)]
      /**/
      while (tokenKind != ROBJ) {
        if (tokenKind != STRING && tokenKind != ID) tokenError("Expecting string or name")
        val name = tokenValue
        tokenNext()
        if (tokenKind != COLON) tokenError("Expecting :")
        tokenNext()
        result = (name -> getJson()) :: result
        tokenKind match{
          case COMMA => tokenNext()
          case ROBJ => // do nothing
          case _ => tokenError("Expecting , or }")
        }
      }
      tokenNext()
      JsObject(result.toMap)
    }
    def handleNumber(name: String, f: String => Unit) = {
      try {
        f(tokenValue)
      } catch {
        case _: Throwable => tokenError("Bad " + name)
      }
      val old = tokenValue
      tokenNext()

      JsNumber(old)
    }
    def getJson(): JsValue = {
      val kind: Int = tokenKind
      val result: JsValue = kind match {
        case ID =>
          val result: JsValue = tokenValue match {
            case "true" => JsTrue
            case "false" => JsFalse
            case "null" => JsNull
            case _ => tokenError("Not true, false, or null")
          }

          tokenNext()
          result

        case STRING =>
          val result = tokenValue
          tokenNext()
          JsString(result)

        case NUMBER => handleNumber("NUMBER", _.toLong)
        case BIGNUMBER => handleNumber("BIGNUMBER", _.toDouble)
        case FLOATNUMBER => handleNumber("FLOATNUMBER", _.toDouble)
        case COLON => handleUnexpected(":")
        case COMMA => handleUnexpected(",")
        case LOBJ => handleObject()
        case ROBJ => handleUnexpected("}")
        case LARR => handleArray()
        case RARR => handleUnexpected("]")
        case EOF => handleEof()
      }
      result
    }
    def parse(): JsValue = {
      chNext()
      tokenNext()
      val result = getJson
      if (tokenKind != EOF) tokenError("Excess input")
      result
    }
    parse()
  }
}

object Json {
  class Exception(val msg: String,
                  val input: String,
                  val line: Int,
                  val char: Int)
    extends scala.Exception(s"JsonParse Error: $msg line $line [$char] in $input")
}
