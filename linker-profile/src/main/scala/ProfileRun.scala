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

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

import scala.io.StdIn

import java.io.IOException
import java.nio.file.{Path, Files, FileVisitResult, SimpleFileVisitor}
import java.nio.file.attribute.BasicFileAttributes

import org.scalajs.logging._

import org.scalajs.linker._
import org.scalajs.linker.interface._

import org.scalajs.testing.adapter.TestAdapterInitializer

import buildinfo.BuildInfo.testClasspath

object ProfileRun {
  def main(args: Array[String]): Unit = {
    val overallCache = StandardImpl.irFileCache()
    val cache = overallCache.newCache

    val linker = StandardImpl.linker(StandardConfig())

    // Use a real FS (not Jimfs) to get real I/O numbers.
    val outputDir = Files.createTempDirectory("sjs-linker-profile-")

    try {
      waitForUser("Ready to link, please start profiling.")
      link(cache, linker, outputDir)
      System.gc()
      waitForUser("Done linking. Please take a heap dump now and stop profiling.")
    } finally {
      Files.walkFileTree(outputDir, DeletingPathVisitor)
    }
  }

  private def link(cache: IRFileCache.Cache, linker: Linker, outputDir: Path): Unit = {
    val moduleInitializers = Seq(
      ModuleInitializer.mainMethod(
          TestAdapterInitializer.ModuleClassName,
          TestAdapterInitializer.MainMethodName)
    )

    val result = PathIRContainer
      .fromClasspath(testClasspath.map(_.toPath()))
      .map(_._1)
      .flatMap(cache.cached _)
      .flatMap(linker.link(_, moduleInitializers, PathOutputDirectory(outputDir), new ScalaConsoleLogger))

    Await.result(result, Duration.Inf)
  }

  private def waitForUser(prompt: String): Unit = {
    print(f"$prompt Hit enter to continue...")

    StdIn.readLine()
  }

  private object DeletingPathVisitor extends SimpleFileVisitor[Path] {
    override def visitFile(file: Path, attr: BasicFileAttributes): FileVisitResult = {
      Files.delete(file)
      FileVisitResult.CONTINUE
    }

    override def postVisitDirectory(directory: Path, exc: IOException): FileVisitResult = {
      Files.delete(directory)
      FileVisitResult.CONTINUE
    }
  }
}
