package org.scalajs.jasmine

import scala.scalajs.js

trait JasmineContext extends js.Object {
  val currentSpec: Spec = ???
  val currentSuite: Suite = ???
  def versionString(): String = ???
  def currentRunner(): TestRunner = ???
  var updateInterval: Int = ???
  def addReporter(reporter: js.Any): Unit = ???
  def execute(): Unit = ???
}
