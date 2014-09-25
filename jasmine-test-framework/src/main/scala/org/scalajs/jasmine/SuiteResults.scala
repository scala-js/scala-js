package org.scalajs.jasmine

import scala.scalajs.js

trait SuiteResults extends js.Object {
  val passedCount: Int = js.native
  val failedCount: Int = js.native
  val totalCount: Int = js.native
}
