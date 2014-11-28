/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.niocharset

import scala.language.implicitConversions

import scala.util._

import java.nio._
import java.nio.charset._

import scala.scalajs.js
import js.JSConverters._

import org.scalajs.jasminetest.JasmineTest

class BaseCharsetTest(val charset: Charset) extends JasmineTest {
  import BaseCharsetTest._

  protected val AllErrorActions = Seq(
      CodingErrorAction.IGNORE,
      CodingErrorAction.REPLACE,
      CodingErrorAction.REPORT)

  protected val ReportActions = Seq(
      CodingErrorAction.REPORT)

  protected def testDecode(in: ByteBuffer)(
      outParts: OutPart[CharBuffer]*): Unit = {

    def testOneConfig(malformedAction: CodingErrorAction,
        unmappableAction: CodingErrorAction): Unit = {

      val decoder = charset.newDecoder()
      decoder.onMalformedInput(malformedAction)
      decoder.onUnmappableCharacter(unmappableAction)

      val actualTry = Try {
        in.mark()
        val buf =
          try decoder.decode(in)
          finally in.reset()
        val actualChars = new Array[Char](buf.remaining())
        buf.get(actualChars)
        actualChars
      }

      val expectedTry = Try {
        val expectedChars = Array.newBuilder[Char]
        outParts foreach {
          case BufferPart(buf) =>
            val bufArray = new Array[Char](buf.remaining)
            buf.mark()
            try buf.get(bufArray)
            finally buf.reset()
            expectedChars ++= bufArray
          case Malformed(len) =>
            malformedAction match {
              case CodingErrorAction.IGNORE  =>
              case CodingErrorAction.REPLACE =>
                expectedChars ++= decoder.replacement()
              case CodingErrorAction.REPORT  =>
                throw new MalformedInputException(len)
            }
          case Unmappable(len) =>
            unmappableAction match {
              case CodingErrorAction.IGNORE  =>
              case CodingErrorAction.REPLACE =>
                expectedChars ++= decoder.replacement()
              case CodingErrorAction.REPORT  =>
                throw new UnmappableCharacterException(len)
            }
        }
        expectedChars.result()
      }

      (actualTry, expectedTry) match {
        case (Failure(actualEx: MalformedInputException),
            Failure(expectedEx: MalformedInputException)) =>
          expect(actualEx.getInputLength()).toEqual(expectedEx.getInputLength())

        case (Failure(actualEx: UnmappableCharacterException),
            Failure(expectedEx: UnmappableCharacterException)) =>
          expect(actualEx.getInputLength()).toEqual(expectedEx.getInputLength())

        case (Success(actualChars), Success(expectedChars)) =>
          expect(actualChars.map(_.toInt).toJSArray).toEqual(
              expectedChars.map(_.toInt).toJSArray)

        case _ =>
          // For the error message
          expect(actualTry.asInstanceOf[js.Any]).toBe(
              expectedTry.asInstanceOf[js.Any])
      }
    }

    val hasAnyMalformed = outParts.exists(_.isInstanceOf[Malformed])
    val hasAnyUnmappable = outParts.exists(_.isInstanceOf[Unmappable])

    for {
      malformedAction  <- if (hasAnyMalformed)  AllErrorActions else ReportActions
      unmappableAction <- if (hasAnyUnmappable) AllErrorActions else ReportActions
    } {
      testOneConfig(malformedAction, unmappableAction)
    }
  }

  protected def testEncode(in: CharBuffer)(
      outParts: OutPart[ByteBuffer]*): Unit = {

    def testOneConfig(malformedAction: CodingErrorAction,
        unmappableAction: CodingErrorAction): Unit = {

      val encoder = charset.newEncoder()
      encoder.onMalformedInput(malformedAction)
      encoder.onUnmappableCharacter(unmappableAction)

      val actualTry = Try {
        in.mark()
        val buf =
          try encoder.encode(in)
          finally in.reset()
        val actualBytes = new Array[Byte](buf.remaining())
        buf.get(actualBytes)
        actualBytes
      }

      val expectedTry = Try {
        val expectedBytes = Array.newBuilder[Byte]
        outParts foreach {
          case BufferPart(buf) =>
            val bufArray = new Array[Byte](buf.remaining)
            buf.mark()
            try buf.get(bufArray)
            finally buf.reset()
            expectedBytes ++= bufArray
          case Malformed(len) =>
            malformedAction match {
              case CodingErrorAction.IGNORE  =>
              case CodingErrorAction.REPLACE =>
                expectedBytes ++= encoder.replacement()
              case CodingErrorAction.REPORT  =>
                throw new MalformedInputException(len)
            }
          case Unmappable(len) =>
            unmappableAction match {
              case CodingErrorAction.IGNORE  =>
              case CodingErrorAction.REPLACE =>
                expectedBytes ++= encoder.replacement()
              case CodingErrorAction.REPORT  =>
                throw new UnmappableCharacterException(len)
            }
        }
        expectedBytes.result()
      }

      (actualTry, expectedTry) match {
        case (Failure(actualEx: MalformedInputException),
            Failure(expectedEx: MalformedInputException)) =>
          expect(actualEx.getInputLength()).toEqual(expectedEx.getInputLength())

        case (Failure(actualEx: UnmappableCharacterException),
            Failure(expectedEx: UnmappableCharacterException)) =>
          expect(actualEx.getInputLength()).toEqual(expectedEx.getInputLength())

        case (Success(actualBytes), Success(expectedBytes)) =>
          expect(actualBytes.toJSArray).toEqual(expectedBytes.toJSArray)

        case _ =>
          // For the error message
          expect(actualTry.asInstanceOf[js.Any]).toBe(
              expectedTry.asInstanceOf[js.Any])
      }
    }

    val hasAnyMalformed = outParts.exists(_.isInstanceOf[Malformed])
    val hasAnyUnmappable = outParts.exists(_.isInstanceOf[Unmappable])

    for {
      malformedAction  <- if (hasAnyMalformed)  AllErrorActions else ReportActions
      unmappableAction <- if (hasAnyUnmappable) AllErrorActions else ReportActions
    } {
      testOneConfig(malformedAction, unmappableAction)
    }
  }
}

object BaseCharsetTest {
  sealed abstract class OutPart[+BufferType <: Buffer]
  final case class BufferPart[BufferType <: Buffer](buf: BufferType) extends OutPart[BufferType]
  final case class Malformed(length: Int) extends OutPart[Nothing]
  final case class Unmappable(length: Int) extends OutPart[Nothing]

  object OutPart {
    implicit def fromBuffer[BufferType <: Buffer](buf: BufferType): BufferPart[BufferType] =
      BufferPart(buf)
  }

  implicit class Interpolators(val sc: StringContext) extends AnyVal {
    def bb(args: Any*): ByteBuffer = {
      val strings = sc.parts.iterator
      val expressions = args.iterator
      val buf = Array.newBuilder[Byte]

      def appendStr(s: String): Unit = {
        val s1 = s.replace(" ", "")
        require(s1.length % 2 == 0)
        for (i <- 0 until s1.length by 2)
          buf += java.lang.Integer.parseInt(s1.substring(i, i+2), 16).toByte
      }

      appendStr(strings.next())
      while (strings.hasNext) {
        expressions.next() match {
          case b: Byte            => buf += b
          case bytes: Array[Byte] => buf ++= bytes
          case bytes: Seq[_]      =>
            buf ++= bytes.map(_.asInstanceOf[Number].byteValue())
        }
        appendStr(strings.next())
      }

      ByteBuffer.wrap(buf.result())
    }

    def cb(args: Any*): CharBuffer =
      CharBuffer.wrap(sc.s(args: _*))
  }
}
