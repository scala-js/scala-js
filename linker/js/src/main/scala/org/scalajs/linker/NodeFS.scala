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

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.typedarray._

private[linker] object NodeFS {
  type CB[T] = js.Function2[js.Error, T, Unit]

  def cbFuture[A](op: CB[A] => Unit): Future[A] = {
    val promise = Promise[A]()

    def cb(err: js.Error, v: A): Unit = {
      import js.DynamicImplicits.truthValue

      if (err.asInstanceOf[js.Dynamic])
        promise.failure(new js.JavaScriptException(err))
      else
        promise.success(v)
    }

    op(cb _)

    promise.future
  }

  object ReadDirOpt extends js.Object {
    val withFileTypes: Boolean = true
  }

  trait Stats extends js.Object {
    val mtime: js.UndefOr[js.Date]
    def isDirectory(): Boolean
  }

  trait Dirent extends js.Object {
    val name: String
    def isDirectory(): Boolean
  }

  @JSImport("fs", "open")
  @js.native
  def open(path: String, flags: String, callback: CB[Int]): Unit = js.native

  @JSImport("fs", "close")
  @js.native
  def close(fd: Int, callback: CB[Unit]): Unit = js.native

  @JSImport("fs", "read")
  @js.native
  def read(fd: Int, buffer: TypedArray[_, _], offset: Int, length: Int, position: Int,
      callback: CB[Int]): Unit = js.native

  @JSImport("fs", "writeFile")
  @js.native
  def writeFile(path: String, data: TypedArray[_, _], callback: CB[Unit]): Unit = js.native

  @JSImport("fs", "readdir")
  @js.native
  def readdir(path: String, opts: ReadDirOpt.type,
      cb: CB[js.Array[Dirent]]): Unit = js.native

  @JSImport("fs", "readFile")
  @js.native
  def readFile(path: String, cb: CB[Uint8Array]): Unit = js.native

  @JSImport("fs", "stat")
  @js.native
  def stat(path: String, cb: CB[Stats]): Unit = js.native
}
