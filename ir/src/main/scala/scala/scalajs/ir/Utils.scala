/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js IR                **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2014, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.ir

import java.net.URI

object Utils {

  /** Relativize target URI w.r.t. base URI */
  def relativize(base0: URI, trgt0: URI): URI = {
    val base = base0.normalize
    val trgt = trgt0.normalize

    if (base.isOpaque || !base.isAbsolute || base.getRawPath == null ||
        trgt.isOpaque || !trgt.isAbsolute || trgt.getRawPath == null ||
        base.getScheme != trgt.getScheme  ||
        base.getRawAuthority != trgt.getRawAuthority)
      trgt
    else {
      val trgtCmps = trgt.getRawPath.split('/')
      val baseCmps = base.getRawPath.split('/')

      val prefixLen = (trgtCmps zip baseCmps).takeWhile(t => t._1 == t._2).size

      val newPathCmps =
        List.fill(baseCmps.size - prefixLen)("..") ++ trgtCmps.drop(prefixLen)

      val newPath = newPathCmps.mkString("/")

      // Relative URI does not have scheme or authority
      new URI(null, null, newPath, trgt.getRawQuery, trgt.getRawFragment)
    }
  }

}
