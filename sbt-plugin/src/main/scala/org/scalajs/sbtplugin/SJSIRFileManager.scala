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

package org.scalajs.sbtplugin

import java.io.File
import java.nio.file.Files

import scala.collection.mutable

import sbt._

import xsbti.compile.ClassFileManager

/** A class file manager that prunes .sjsir files as needed.
 *
 *  This makes sure that, when a .class file must be deleted, the
 *  corresponding .sjsir file is also deleted, as well as maintaining the
 *  transactional aspect of incremental compilation in the presence of
 *  compilation errors in an intermediate run (as product files need to be
 *  backed up and later restored).
 *
 *  This code is adapted from Zinc `TransactionalClassFileManager`.
 *  We need to duplicate the logic since forwarding to the default class
 *  file manager doesn't work: we need to backup sjsir files in a different
 *  temporary directory as class files.
 *
 *  We stole this from `TastyFileManager` in dotty, at
 *  https://github.com/lampepfl/dotty/blob/0.17.0-RC1/sbt-dotty/src/dotty/tools/sbtplugin/TastyFileManager.scala
 *
 *  There are plans to upstream a generic version of this code in zinc in the
 *  future: https://github.com/sbt/zinc/issues/579
 */
private[sbtplugin] final class SJSIRFileManager extends ClassFileManager {
  private[this] var _tempDir: Option[File] = None

  private[this] def tempDir: File = {
    _tempDir.getOrElse {
      val t = Files.createTempDirectory("backup").toFile()
      _tempDir = Some(t)
      t
    }
  }

  private[this] val generatedSJSIRFiles = new mutable.HashSet[File]
  private[this] val movedSJSIRFiles = new mutable.HashMap[File, File]

  /* The incremental compiler works along the following general protocol:
   * 1. it identifies the set of files that have been changed/added/removed
   * 2. it compiles that set of files
   *    a) it first `delete`s the products of changed and removed files
   *    b) then it compiles the files
   *    c) it registers produced files as `generated`
   * 3. it detects whether a new round of compilation is necessary, and with
   *    which files; if necessary, it loops back to 2.
   * 4. when no new round is necessary, it `complete`s with `success = true` if
   *    the whole cycle is successful, or `success = false` otherwise.
   *
   * Consequently, it is quite possible that, within one cycle, a given file is
   * `generated` during one iteration, and `delete`d in a subsequent one. In
   * this case, we would need to revert back to the file not being present in
   * the final transactional output on `success = false`. This is why we have
   * the `!generatedSJSIRFiles(t)` test in `delete()` below.
   */

  override def delete(classes: Array[File]): Unit = {
    val files = sjsirFiles(classes)
    val toBeBackedUp = files
      .filter(t => !movedSJSIRFiles.contains(t) && !generatedSJSIRFiles(t))
    for (c <- toBeBackedUp)
      movedSJSIRFiles.put(c, backupFile(c))
    IO.deleteFilesEmptyDirs(files)
  }

  override def generated(classes: Array[File]): Unit =
    generatedSJSIRFiles ++= sjsirFiles(classes)

  override def complete(success: Boolean): Unit = {
    if (!success) {
      IO.deleteFilesEmptyDirs(generatedSJSIRFiles)
      for ((orig, tmp) <- movedSJSIRFiles)
        IO.move(tmp, orig)
    }

    generatedSJSIRFiles.clear()
    movedSJSIRFiles.clear()
    for (tempDir <- _tempDir) {
      IO.delete(tempDir)
      _tempDir = None
    }
  }

  private def sjsirFiles(classes: Array[File]): Array[File] = {
    classes.flatMap { classFile =>
      if (classFile.getPath.endsWith(".class")) {
        val f = new File(classFile.getPath.stripSuffix(".class") + ".sjsir")
        if (f.exists) List(f)
        else Nil
      } else {
        Nil
      }
    }
  }

  private def backupFile(c: File): File = {
    val target = File.createTempFile("sbt", ".sjsir", tempDir)
    IO.move(c, target)
    target
  }
}
