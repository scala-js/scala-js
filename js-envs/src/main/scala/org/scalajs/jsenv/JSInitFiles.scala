package org.scalajs.jsenv

import org.scalajs.io.VirtualJSFile

trait JSInitFiles {
  /** JS files used to setup VM */
  protected def initFiles(): Seq[VirtualJSFile] = Nil
}
