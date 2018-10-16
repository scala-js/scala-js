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

package org.scalajs.linker.backend.closure

import java.net.URI

private[closure] object URIUtil {
  def sourceURIToString(relativizeBaseURI: Option[URI], uri: URI): String = {
    import org.scalajs.io.URIUtils._

    val relURI = relativizeBaseURI.fold(uri)(relativize(_, uri))
    fixFileURI(relURI).toASCIIString
  }
}
