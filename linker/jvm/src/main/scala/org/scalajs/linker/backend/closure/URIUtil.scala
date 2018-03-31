package org.scalajs.linker.backend.closure

import java.net.URI

private[closure] object URIUtil {
  def sourceURIToString(relativizeBaseURI: Option[URI], uri: URI): String = {
    import org.scalajs.io.URIUtils._

    val relURI = relativizeBaseURI.fold(uri)(relativize(_, uri))
    fixFileURI(relURI).toASCIIString
  }
}
