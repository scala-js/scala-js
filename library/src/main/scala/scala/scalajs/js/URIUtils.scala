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

package scala.scalajs.js

import scala.scalajs.js
import scala.scalajs.js.annotation._

/** Methods related to URIs, provided by ECMAScript 5.1. */
@js.native
@JSGlobalScope
object URIUtils extends js.Object {

  /** Decodes a Uniform Resource Identifier (URI).
   *  @see [[encodeURI]]
   */
  def decodeURI(encodedURI: String): String = js.native

  /** Decodes a Uniform Resource Identifier (URI) component.
   *  @see [[encodeURIComponent]]
   */
  def decodeURIComponent(encodedURIComponent: String): String = js.native

  /** Encodes a Uniform Resource Identifier (URI).
   *  @see [[decodeURI]]
   */
  def encodeURI(uri: String): String = js.native

  /** Encodes a Uniform Resource Identifier (URI) component.
   *  @see [[decodeURIComponent]]
   */
  def encodeURIComponent(uriComponent: String): String = js.native

}
