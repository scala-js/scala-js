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

package org.scalajs.linker.backend.javascript

import java.net.URI

private[backend] object SourceFileUtil {

  /** Relatively hacky way to get a Web-friendly URI to the source file */
  def webURI(relativizeBase: Option[URI], uri: URI): String = {
    val relURI = relativizeBase.fold(uri)(relativizeURI(_, uri))
    fixFileURI(relURI).toASCIIString
  }

  /** Relativize target URI w.r.t. base URI */
  private def relativizeURI(base0: URI, trgt0: URI): URI = {
    val base = base0.normalize
    val trgt = trgt0.normalize

    if (base.isOpaque || !base.isAbsolute || base.getRawPath == null ||
        trgt.isOpaque || !trgt.isAbsolute || trgt.getRawPath == null ||
        base.getScheme != trgt.getScheme ||
        base.getRawAuthority != trgt.getRawAuthority) {
      trgt
    } else {
      val trgtCmps = trgt.getRawPath.split("/", -1)

      // Disregard the last element, since it is the filename
      // (or empty string for a directory).
      val baseCmps = base.getRawPath.split("/", -1).init

      val prefixLen = (trgtCmps.zip(baseCmps)).takeWhile(t => t._1 == t._2).size

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
  private def fixFileURI(uri: URI): URI =
    if (uri.getScheme() != "file" || uri.getAuthority() != null) uri
    else new URI("file", "", uri.getPath(), uri.getQuery(), uri.getFragment())
}
