package org.scalajs.testinterface.internal

import scala.scalajs.js
import js.Dynamic.{literal => lit}
import js.JSConverters._

import sbt.testing._

object EventSerializer {

  def serialize(ev: Event): js.Dynamic = {
    val res = lit(
        fullyQualifiedName = ev.fullyQualifiedName,
        fingerprint = FingerprintSerializer.serialize(ev.fingerprint),
        selector = SelectorSerializer.serialize(ev.selector),
        status = ev.status.name(),
        durationLS = ev.duration().toInt,
        durationMS = (ev.duration() >>> 32).toInt)

    val optT = ev.throwable()
    if (optT.isDefined)
      res.throwable = ThrowableSerializer.serialize(optT.get())

    res
  }

}
