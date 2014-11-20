package org.scalajs.testinterface.internal

import scala.scalajs.js
import js.Dynamic.{literal => lit}

import sbt.testing._

object SelectorSerializer {

  def serialize(sel: Selector): js.Dynamic = sel match {
    case sel: SuiteSelector => lit(
        selType = "SuiteSelector")
    case sel: TestSelector => lit(
        selType = "TestSelector",
        testName = sel.testName)
    case sel: NestedSuiteSelector => lit(
        selType = "NestedSuiteSelector",
        suiteId = sel.suiteId)
    case sel: NestedTestSelector => lit(
        selType = "NestedTestSelector",
        suiteId = sel.suiteId,
        testName = sel.testName)
    case sel: TestWildcardSelector => lit(
        selType = "TestWildcardSelector",
        testWildcard = sel.testWildcard)
    case _ =>
      throw new IllegalArgumentException(
          s"Unknown Selector type: ${sel.getClass}")
  }

  def deserialize(obj: js.Dynamic): Selector = {
    obj.selType.asInstanceOf[String] match {
      case "SuiteSelector" =>
          new SuiteSelector()
        case "TestSelector" =>
          new TestSelector(
              obj.testName.asInstanceOf[String])
        case "NestedSuiteSelector" =>
          new NestedSuiteSelector(
              obj.suiteId.asInstanceOf[String])
        case "NestedTestSelector" =>
          new NestedTestSelector(
              obj.suiteId.asInstanceOf[String],
              obj.testName.asInstanceOf[String])
        case "TestWildcardSelector" =>
          new TestWildcardSelector(
              obj.testWildcard.asInstanceOf[String])
        case tpe =>
          throw new IllegalArgumentException(s"Unknown Selector type: $tpe")
    }
  }

}
