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

package org.scalajs.linker

import scala.annotation.tailrec
import scala.concurrent._

import java.io._
import java.nio._
import java.nio.file._
import java.nio.file.attribute._
import java.util.EnumSet
import java.util.zip.{ZipInputStream, ZipEntry}

import org.scalajs.linker.irio._
import org.scalajs.linker.standard.{IRContainerImpl, IRFileImpl}

abstract class IRContainerPlatformExtensions private[linker] () {
  def fromPathClasspath(classpath: Seq[Path])(
      implicit ec: ExecutionContext): Future[(Seq[IRContainer], Seq[Path])] = Future {
    val containers = Seq.newBuilder[IRContainer]
    val paths = Seq.newBuilder[Path]

    val dirVisitor = new SimpleFileVisitor[Path] {
      override def visitFile(path: Path, attrs: BasicFileAttributes): FileVisitResult = {
        if (path.getFileName().toString().endsWith(".sjsir")) {
          containers += IRContainer.fromIRFile(
              new IRFilePlatformExtensions.PathIRFileImpl(path, attrs.lastModifiedTime()))
          paths += path
        }
        super.visitFile(path, attrs)
      }
    }

    blocking {
      for (entry <- classpath if Files.exists(entry)) {
        val attrs = Files.readAttributes(entry, classOf[BasicFileAttributes])

        if (attrs.isDirectory()) {
          Files.walkFileTree(entry, EnumSet.of(FileVisitOption.FOLLOW_LINKS), Int.MaxValue, dirVisitor)
        } else if (entry.getFileName().toString().endsWith(".jar")) {
          containers += new JarIRContainer(entry, attrs.lastModifiedTime())
          paths += entry
        } else {
          throw new IllegalArgumentException("Illegal classpath entry " + entry)
        }
      }
    }

    (containers.result(), paths.result())
  }

  private final class JarIRContainer(path: Path, lastModified: FileTime)
      extends IRContainerImpl(path.toString, Some(lastModified.toString)) {
    def sjsirFiles(implicit ec: ExecutionContext): Future[List[IRFile]] =
      Future(blocking(read()))

    private def read(): List[IRFile] = {
      val stream = new ZipInputStream(new BufferedInputStream(Files.newInputStream(path)))
      try {
        val buf = new Array[Byte](4096)

        @tailrec
        def readAll(out: OutputStream): Unit = {
          val read = stream.read(buf)
          if (read != -1) {
            out.write(buf, 0, read)
            readAll(out)
          }
        }

        def makeVF(e: ZipEntry) = {
          val size = e.getSize
          val out =
            if (0 <= size && size <= Int.MaxValue) new ByteArrayOutputStream(size.toInt)
            else new ByteArrayOutputStream()

          try {
            readAll(out)
            IRFileImpl.fromMem(s"${path.toString}:${e.getName}", version, out.toByteArray)
          } finally {
            out.close()
          }
        }

        Iterator.continually(stream.getNextEntry())
          .takeWhile(_ != null)
          .filter(_.getName.endsWith(".sjsir"))
          .map(makeVF)
          .toList
      } finally {
        stream.close()
      }
    }
  }
}
