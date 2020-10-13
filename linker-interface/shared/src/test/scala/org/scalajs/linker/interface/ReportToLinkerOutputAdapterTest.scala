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

package org.scalajs.linker.interface

import scala.collection.mutable
import scala.concurrent._

import java.net.URI
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets.UTF_8

import org.junit.Test
import org.junit.Assert._

import org.scalajs.junit.async._

import org.scalajs.linker.interface.unstable._

@deprecated("Mark deprecated to silence warnings", "never/always")
class ReportToLinkerOutputAdapterTest {
  import ReportToLinkerOutputAdapterTest._
  import scala.concurrent.ExecutionContext.Implicits.global

  private val dummyReport = new ReportImpl(List(
      new ReportImpl.ModuleImpl(
          moduleID = "dummy",
          jsFileName = "main.js",
          sourceMapName = Some("main.js.map"),
          moduleKind = ModuleKind.NoModule
      )
  ))

  @Test
  def testReplaceLinks(): AsyncResult = await {
    val writeOut = new WriteOnlyOutputDirectory()

    val legacyOutput = LinkerOutput(new OutputFileImpl("js", writeOut))
      .withSourceMap(new OutputFileImpl("sm", writeOut))
      .withSourceMapURI(new URI("http://example.org/my-source-map-uri"))
      .withJSFileURI(new URI("http://example.org/my-js-file-uri"))

    val readOut = new ReadOnlyOutputDirectory(
        "main.js" -> raw"""
          |console.log("hello");
          |//# sourceMappingURL=main.js.map
          |// some other comment
          |""".stripMargin,
        "main.js.map" -> raw"""{
          |  "file": "main.js",
          |  "other key": 1
          |}""".stripMargin)

    for {
      _ <- ReportToLinkerOutputAdapter.convert(dummyReport, readOut, legacyOutput)
    } yield {
      assertEquals(writeOut.content.size, 2)

      assertEquals(raw"""
          |console.log("hello");
          |//# sourceMappingURL=http://example.org/my-source-map-uri
          |// some other comment
          |""".stripMargin,
          writeOut.content("js"))
      assertEquals(raw"""{"file": "http://example.org/my-js-file-uri",
          |  "other key": 1
          |}""".stripMargin,
          writeOut.content("sm"))
    }
  }

  @Test
  def testAddLinks(): AsyncResult = await {
    val writeOut = new WriteOnlyOutputDirectory()

    val legacyOutput = LinkerOutput(new OutputFileImpl("js", writeOut))
      .withSourceMap(new OutputFileImpl("sm", writeOut))
      .withSourceMapURI(new URI("http://example.org/my-source-map-uri"))
      .withJSFileURI(new URI("http://example.org/my-js-file-uri"))

    val readOut = new ReadOnlyOutputDirectory(
        "main.js" -> raw"""
          |console.log("hello");
          |""".stripMargin,
        "main.js.map" -> raw"""{
          |  "other key": 1
          |}""".stripMargin)

    for {
      _ <- ReportToLinkerOutputAdapter.convert(dummyReport, readOut, legacyOutput)
    } yield {
      assertEquals(writeOut.content.size, 2)

      assertEquals(raw"""
          |console.log("hello");
          |
          |//# sourceMappingURL=http://example.org/my-source-map-uri
          |""".stripMargin,
          writeOut.content("js"))
      assertEquals(raw"""{"file": "http://example.org/my-js-file-uri",
          |  "other key": 1
          |}""".stripMargin,
          writeOut.content("sm"))
    }
  }

  @Test
  def testRemoveLinks(): AsyncResult = await {
    val writeOut = new WriteOnlyOutputDirectory()

    val legacyOutput = LinkerOutput(new OutputFileImpl("js", writeOut))
      .withSourceMap(new OutputFileImpl("sm", writeOut))

    val readOut = new ReadOnlyOutputDirectory(
        "main.js" -> raw"""
          |console.log("hello");
          |//# sourceMappingURL=main.js.map
          |// some other comment
          |""".stripMargin,
        "main.js.map" -> raw"""{
          |  "file": "main.js",
          |  "other key": 1
          |}""".stripMargin)

    for {
      _ <- ReportToLinkerOutputAdapter.convert(dummyReport, readOut, legacyOutput)
    } yield {
      assertEquals(writeOut.content.size, 2)

      assertEquals(raw"""
          |console.log("hello");
          |
          |// some other comment
          |""".stripMargin,
          writeOut.content("js"))
      assertEquals(raw"""{
          |  "other key": 1
          |}""".stripMargin,
          writeOut.content("sm"))
    }
  }

  @Test
  def testNoLinks(): AsyncResult = await {
    val writeOut = new WriteOnlyOutputDirectory()

    val legacyOutput = LinkerOutput(new OutputFileImpl("js", writeOut))
      .withSourceMap(new OutputFileImpl("sm", writeOut))

    val readOut = new ReadOnlyOutputDirectory(
        "main.js" -> raw"""
          |console.log("hello");
          |""".stripMargin,
        "main.js.map" -> raw"""{
          |  "other key": 1
          |}""".stripMargin)

    for {
      _ <- ReportToLinkerOutputAdapter.convert(dummyReport, readOut, legacyOutput)
    } yield {
      assertEquals(writeOut.content.size, 2)

      assertEquals(raw"""
          |console.log("hello");
          |""".stripMargin,
          writeOut.content("js"))
      assertEquals(raw"""{
          |  "other key": 1
          |}""".stripMargin,
          writeOut.content("sm"))
    }
  }
}

object ReportToLinkerOutputAdapterTest {
  private class ReadOnlyOutputDirectory(fileContents: Map[String, String])
      extends OutputDirectoryImpl {
    def this(fileContents: (String, String)*) = this(fileContents.toMap)

    def writeFull(name: String, buf: ByteBuffer)(
        implicit ec: ExecutionContext): Future[Unit] = {
      throw new AssertionError("should not be called")
    }

    def readFull(name: String)(
        implicit ec: ExecutionContext): Future[ByteBuffer] = {
      Future.successful(ByteBuffer.wrap(fileContents(name).getBytes(UTF_8)))
    }

    def listFiles()(implicit ec: ExecutionContext): Future[List[String]] =
      Future.successful(fileContents.keys.toList)

    def delete(name: String)(implicit ec: ExecutionContext): Future[Unit] =
      throw new AssertionError("should not be called")
  }

  private class WriteOnlyOutputDirectory extends OutputDirectoryImpl {
    val content: mutable.Map[String, String] = mutable.Map.empty

    def writeFull(name: String, buf: ByteBuffer)(
        implicit ec: ExecutionContext): Future[Unit] = {
      content(name) = UTF_8.decode(buf).toString()
      Future.successful(())
    }

    def readFull(name: String)(
        implicit ec: ExecutionContext): Future[ByteBuffer] = {
      throw new AssertionError("should not be called")
    }

    def listFiles()(implicit ec: ExecutionContext): Future[List[String]] =
      throw new AssertionError("should not be called")

    def delete(name: String)(implicit ec: ExecutionContext): Future[Unit] =
      throw new AssertionError("should not be called")
  }
}
