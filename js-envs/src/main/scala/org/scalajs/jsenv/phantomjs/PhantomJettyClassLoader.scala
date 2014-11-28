package org.scalajs.jsenv.phantomjs

import org.scalajs.core.tools.io.IO

/** A special [[ClassLoader]] to load the Jetty 8 dependency of [[PhantomJSEnv]]
 *  in a private space.
 *
 *  It loads everything that belongs to [[JettyWebsocketManager]] itself (while
 *  retrieving the requested class file from its parent.
 *  For all other classes, it first tries to load them from [[jettyLoader]],
 *  which should only contain the Jetty 8 classpath.
 *  If this fails, it delegates to its parent.
 *
 *  The rationale is, that [[JettyWebsocketManager]] and its dependees can use
 *  the classes on the Jetty 8 classpath, while they remain hidden from the rest
 *  of the Java world. This allows to load another version of Jetty in the same
 *  JVM for the rest of the project.
 */
final class PhantomJettyClassLoader(jettyLoader: ClassLoader,
    parent: ClassLoader) extends ClassLoader(parent) {

  def this(loader: ClassLoader) =
    this(loader, ClassLoader.getSystemClassLoader())

  /** Classes needed to bridge private jetty classpath and public PhantomJS
   *  Basically everything defined in JettyWebsocketManager.
   */
  private val bridgeClasses = Set(
    "org.scalajs.jsenv.phantomjs.JettyWebsocketManager",
    "org.scalajs.jsenv.phantomjs.JettyWebsocketManager$WSLogger",
    "org.scalajs.jsenv.phantomjs.JettyWebsocketManager$ComWebSocketListener",
    "org.scalajs.jsenv.phantomjs.JettyWebsocketManager$$anon$1",
    "org.scalajs.jsenv.phantomjs.JettyWebsocketManager$$anon$2"
  )

  override protected def loadClass(name: String, resolve: Boolean): Class[_] = {
    if (bridgeClasses.contains(name)) {
      // Load bridgeClasses manually since they must be associated to this
      // class loader, rather than the parent class loader in order to find the
      // jetty classes

      // First check if we have loaded it already
      Option(findLoadedClass(name)) getOrElse {
        val wsManager =
          parent.getResourceAsStream(name.replace('.', '/') + ".class")

        if (wsManager == null) {
          throw new ClassNotFoundException(name)
        } else {
          val buf = IO.readInputStreamToByteArray(wsManager)
          defineClass(name, buf, 0, buf.length)
        }
      }
    } else {
      try {
        jettyLoader.loadClass(name)
      } catch {
        case _: ClassNotFoundException =>
          super.loadClass(name, resolve)
      }
    }
  }
}
