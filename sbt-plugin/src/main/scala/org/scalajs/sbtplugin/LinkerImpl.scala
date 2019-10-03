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

import scala.concurrent._

import java.lang.reflect.{Method, Modifier}
import java.io.File
import java.net.URLClassLoader
import java.nio.file.Path

import org.scalajs.linker.interface._

/** Abstract implementation of a linker as needed by the sbt plugin.
 *
 *  @note This trait does not guarantee full compatibility: Methods may be added
 *      / removed in the future. Use [[LinkerImpl.Forwarding]] to override things
 *      selectively in a compatible manner.
 */
trait LinkerImpl {
  def clearableLinker(cfg: StandardConfig): ClearableLinker

  def irFileCache(): IRFileCache

  def irContainers(classpath: Seq[Path])(
      implicit ec: ExecutionContext): Future[(Seq[IRContainer], Seq[Path])]

  def outputFile(path: Path): LinkerOutput.File
}

object LinkerImpl {
  /** Returns the default implementation.
   *
   *  It loads a StandardLinker via reflection.
   */
  def default(files: Seq[File]): LinkerImpl = {
    val urls = files.map(_.toURI.toURL).toArray
    val loader = new URLClassLoader(urls, new FilteringClassLoader(getClass.getClassLoader))
    new Reflect(loader)
  }

  /** A [[LinkerImpl]] that forwards everything to `parent`.
   *
   *  This is useful if only parts of the linker implementation need to be
   *  replaced. A subclass only overriding these can be created, ensuring easier
   *  transition when methods get added.
   */
  class Forwarding(parent: LinkerImpl) extends LinkerImpl {
    def clearableLinker(cfg: StandardConfig): ClearableLinker =
      parent.clearableLinker(cfg)

    def irFileCache(): IRFileCache =
      parent.irFileCache()

    def irContainers(classpath: Seq[Path])(
        implicit ec: ExecutionContext): Future[(Seq[IRContainer], Seq[Path])] =
      parent.irContainers(classpath)

    def outputFile(path: Path): LinkerOutput.File =
      parent.outputFile(path)
  }

  private final class FilteringClassLoader(parent: ClassLoader)
      extends ClassLoader(parent) {
    private val parentPrefixes = List(
      "java.",
      "scala.",
      "org.scalajs.linker.interface.",
      "org.scalajs.logging.",
      "org.scalajs.ir."
    )

    override def loadClass(name: String, resolve: Boolean): Class[_] = {
      if (parentPrefixes.exists(name.startsWith _))
        super.loadClass(name, resolve)
      else
        null
    }
  }

  private final class Reflect(loader: ClassLoader) extends LinkerImpl {
    private def loadMethod(clazz: String, method: String, result: Class[_], params: Class[_]*): Method = {
      val m = Class.forName("org.scalajs.linker." + clazz, true, loader).getMethod(method, params: _*)
      require(Modifier.isStatic(m.getModifiers()))
      require(result.isAssignableFrom(m.getReturnType()))
      m
    }

    private def invoke[T](method: Method, args: AnyRef*): T =
      method.invoke(null, args: _*).asInstanceOf[T]

    /* We load everything eagerly to fail immediately, not only when the methods
     * are invoked.
     */
    private val clearableLinkerMethod =
      loadMethod("StandardImpl", "clearableLinker", classOf[ClearableLinker], classOf[StandardConfig])

    private val irFileCacheMethod =
      loadMethod("StandardImpl", "irFileCache", classOf[IRFileCache])

    private val irContainersMethod = {
      loadMethod("PathIRContainer", "fromClasspath", classOf[Future[_]],
          classOf[Seq[Path]], classOf[ExecutionContext])
    }

    private val outputFileMethod =
      loadMethod("PathOutputFile", "atomic", classOf[LinkerOutput.File], classOf[Path])

    def clearableLinker(cfg: StandardConfig): ClearableLinker =
      invoke(clearableLinkerMethod, cfg)

    def irFileCache(): IRFileCache =
      invoke(irFileCacheMethod)

    def irContainers(classpath: Seq[Path])(
        implicit ec: ExecutionContext): Future[(Seq[IRContainer], Seq[Path])] = {
      invoke(irContainersMethod, classpath, ec)
    }

    def outputFile(path: Path): LinkerOutput.File =
      invoke(outputFileMethod, path)
  }
}
