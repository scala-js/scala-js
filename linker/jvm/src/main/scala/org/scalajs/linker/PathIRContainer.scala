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

import scala.concurrent._

import java.nio._
import java.nio.file._
import java.nio.file.attribute._
import java.util.EnumSet

import org.scalajs.linker.standard.{IRContainerImpl, MemIRFileImpl}

object PathIRContainer {
  def fromClasspath(classpath: Seq[Path])(
      implicit ec: ExecutionContext): Future[(Seq[IRContainer], Seq[Path])] = Future {
    val containers = Seq.newBuilder[IRContainer]
    val paths = Seq.newBuilder[Path]

    blocking {
      for (entry <- classpath if Files.exists(entry)) {
        val attrs = Files.readAttributes(entry, classOf[BasicFileAttributes])

        if (attrs.isDirectory()) {
          walkIR(entry) { (path, attrs) =>
            containers += IRContainer.fromIRFile(
                new PathIRFile.PathIRFileImpl(path, attrs.lastModifiedTime()))
            paths += path
          }
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
    def sjsirFiles(implicit ec: ExecutionContext): Future[List[IRFile]] = Future {
      val files = List.newBuilder[IRFile]

      blocking {
        // Open zip/jar file as filesystem.
        val fs = FileSystems.newFileSystem(path, null)
        try {
          val i = fs.getRootDirectories().iterator()
          while (i.hasNext()) {
            walkIR(i.next()) { (entry, _) =>
              // We copy the contents of the file, otherwise we'd have to keep
              // the zip file system open (and it's unclear if it would be
              // faster).
              files += new MemIRFileImpl(
                  s"${path.toString}:${entry.toString}", version,
                  Files.readAllBytes(entry))
            }
          }
        } finally {
          fs.close()
        }
      }

      files.result()
    }
  }

  private def walkIR(path: Path)(visit: (Path, BasicFileAttributes) => Unit): Unit = {
    val dirVisitor = new SimpleFileVisitor[Path] {
      override def visitFile(path: Path, attrs: BasicFileAttributes): FileVisitResult = {
        if (path.getFileName().toString().endsWith(".sjsir")) {
          visit(path, attrs)
        }
        super.visitFile(path, attrs)
      }
    }

    Files.walkFileTree(path, EnumSet.of(FileVisitOption.FOLLOW_LINKS), Int.MaxValue, dirVisitor)
  }
}
