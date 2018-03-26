/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2018, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */

package org.scalajs.io

import java.net.URI

private[scalajs] object URIUtils {

  /** Relativize target URI w.r.t. base URI */
  def relativize(base0: URI, trgt0: URI): URI = {
    val base = base0.normalize
    val trgt = trgt0.normalize

    if (base.isOpaque || !base.isAbsolute || base.getRawPath == null ||
        trgt.isOpaque || !trgt.isAbsolute || trgt.getRawPath == null ||
        base.getScheme != trgt.getScheme  ||
        base.getRawAuthority != trgt.getRawAuthority) {
      trgt
    } else {
      val trgtCmps = trgt.getRawPath.split("/", -1)

      // Disregard the last element, since it is the filename
      // (or empty string for a directory).
      val baseCmps = base.getRawPath.split("/", -1).init

      val prefixLen = (trgtCmps zip baseCmps).takeWhile(t => t._1 == t._2).size

      val newPathCmps =
        List.fill(baseCmps.size - prefixLen)("..") ++ trgtCmps.drop(prefixLen)

      val newPath = newPathCmps.mkString("/")

      // Relative URI does not have scheme or authority
      new URI(null, null, newPath, trgt.getRawQuery, trgt.getRawFragment)
    }
  }

  /** Adds an empty authority to URIs with the "file" scheme without authority.
   *  Some browsers don't fetch URIs without authority correctly.
   */
  def fixFileURI(uri: URI): URI =
    if (uri.getScheme() != "file" || uri.getAuthority() != null) uri
    else new URI("file", "", uri.getPath(), uri.getQuery(), uri.getFragment())

}
