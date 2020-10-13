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
import org.scalajs.linker.interface.unstable.OutputFileImpl

/** Abstract implementation of a linker as needed by the sbt plugin.
 *
 *  @note
 *    **Unstable API**: this API is subject to backward incompatible changes in
 *    future minor versions of Scala.js.
 */
trait LinkerImpl {
  def clearableLinker(cfg: StandardConfig): ClearableLinker

  final def irFileCache(): IRFileCache = irFileCache(IRFileCacheConfig())

  def irFileCache(cfg: IRFileCacheConfig): IRFileCache

  def irContainers(classpath: Seq[Path])(
      implicit ec: ExecutionContext): Future[(Seq[IRContainer], Seq[Path])]

  def outputDirectory(path: Path): OutputDirectory

  @deprecated("Use outputDirectory instead", "1.3.0")
  final def outputFile(path: Path): LinkerOutput.File =
    new OutputFileImpl(path.getFileName().toString(), outputDirectory(path.getParent()))
}

/** Factory methods and concrete implementations of `LinkerImpl`.
 *
 *  @note
 *    **Unstable API**: this API is subject to backward incompatible changes in
 *    future minor versions of Scala.js.
 */
object LinkerImpl {
  /** Returns an implementation of the standard linker loaded via reflection. */
  def reflect(classpath: Seq[File]): LinkerImpl.Reflect = {
    val urls = classpath.map(_.toURI.toURL).toArray
    val loader = new URLClassLoader(urls, new FilteringClassLoader(getClass.getClassLoader))
    new Reflect(loader)
  }

  /** A [[LinkerImpl]] that forwards everything to `parent`.
   *
   *  This is useful if only parts of the linker implementation need to be
   *  replaced. A subclass only overriding these can be created, ensuring easier
   *  transition when methods get added.
   *
   *  @note
   *    **Unstable API**: this API is subject to backward incompatible changes
   *    in future minor versions of Scala.js.
   */
  class Forwarding(parent: LinkerImpl) extends LinkerImpl {
    def clearableLinker(cfg: StandardConfig): ClearableLinker =
      parent.clearableLinker(cfg)

    def irFileCache(cfg: IRFileCacheConfig): IRFileCache =
      parent.irFileCache(cfg)

    def irContainers(classpath: Seq[Path])(
        implicit ec: ExecutionContext): Future[(Seq[IRContainer], Seq[Path])] =
      parent.irContainers(classpath)

    def outputDirectory(path: Path): OutputDirectory =
      parent.outputDirectory(path)
  }

  private final class FilteringClassLoader(parent: ClassLoader)
      extends ClassLoader(parent) {
    private val parentPrefixes = List(
      "java.",
      "scala.",
      "org.scalajs.linker.interface.",
      "org.scalajs.logging.",
      "org.scalajs.ir.",
      /*
       * A workaround for the OpenJDK bug 6265952 (#3921)
       * https://bugs.java.com/bugdatabase/view_bug.do?bug_id=6265952
       *
       * It manifests as a `java.lang.NoClassDefFoundError` being thrown,
       * claiming the class `MethodAccessorImpl` is missing. The package of the
       * class is implementation specific. The currently known packages are
       * listed as prefixes below.
       *
       * The bug is triggered when calling `java.lang.Method#invoke` if both of
       * the following conditions are met:
       *
       * - this is the 15th (or later) time `invoke` is called on this instance,
       * - the `ClassLoader` of the method's owning class does not make
       *   `MethodAccessorImpl` available.
       *
       * Depending on the JDK implementation, the system property
       * `sun.reflect.inflationThreshold` controls the invocation count
       * threshold and can serve as a temporary workaround (e.g. set
       * `-Dsun.reflect.inflationThreshold=30`)
       *
       * To work around the issue, this `ClassLoader` delegates loading of
       * classes in these internal packages to the parent `ClassLoader`.
       * Additional package prefixes may need to be added in the future if the
       * internal package names change or another implementation uses a
       * different name.
       */
      "sun.reflect.",
      "jdk.internal.reflect."
    )

    override def loadClass(name: String, resolve: Boolean): Class[_] = {
      if (parentPrefixes.exists(name.startsWith _))
        super.loadClass(name, resolve)
      else
        null
    }
  }

  /** A `LinkerImpl` that loads the linker via reflection.
   *
   *  Instances can be created with the [[LinkerImpl.reflect]] method.
   *
   *  @note
   *    **Unstable API**: this API is subject to backward incompatible changes
   *    in future minor versions of Scala.js.
   */
  final class Reflect private[LinkerImpl] (val loader: ClassLoader)
      extends LinkerImpl {

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
      loadMethod("StandardImpl", "irFileCache", classOf[IRFileCache], classOf[IRFileCacheConfig])

    private val irContainersMethod = {
      loadMethod("PathIRContainer", "fromClasspath", classOf[Future[_]],
          classOf[Seq[Path]], classOf[ExecutionContext])
    }

    private val outputDirectoryMethod =
      loadMethod("PathOutputDirectory", "apply", classOf[OutputDirectory], classOf[Path])

    def clearableLinker(cfg: StandardConfig): ClearableLinker =
      invoke(clearableLinkerMethod, cfg)

    def irFileCache(cfg: IRFileCacheConfig): IRFileCache =
      invoke(irFileCacheMethod, cfg)

    def irContainers(classpath: Seq[Path])(
        implicit ec: ExecutionContext): Future[(Seq[IRContainer], Seq[Path])] = {
      invoke(irContainersMethod, classpath, ec)
    }

    def outputDirectory(path: Path): OutputDirectory =
      invoke(outputDirectoryMethod, path)
  }
}
