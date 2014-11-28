/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.testadapter

import sbt.testing._

import org.scalajs.core.tools.json._

private[testadapter] object SelectorSerializers {

  implicit object SelectorSerializer extends JSONSerializer[Selector] {
    def serialize(sel: Selector): JSON = {
      val bld = new JSONObjBuilder()
      sel match {
        case sel: SuiteSelector => bld
          .fld("selType", "SuiteSelector")
        case sel: TestSelector => bld
          .fld("selType", "TestSelector")
          .fld("testName", sel.testName)
        case sel: NestedSuiteSelector => bld
          .fld("selType", "NestedSuiteSelector")
          .fld("suiteId", sel.suiteId)
        case sel: NestedTestSelector => bld
          .fld("selType", "NestedTestSelector")
          .fld("suiteId", sel.suiteId)
          .fld("testName", sel.testName)
        case sel: TestWildcardSelector => bld
          .fld("selType", "TestWildcardSelector")
          .fld("testWildcard", sel.testWildcard)
        case _ =>
          throw new IllegalArgumentException(
              s"Unknown Selector type: ${sel.getClass}")
      }
      bld.toJSON
    }
  }

  implicit object SelectorDeserializer extends JSONDeserializer[Selector] {
    def deserialize(x: JSON): Selector = {
      val obj = new JSONObjExtractor(x)
      obj.fld[String]("selType") match {
        case "SuiteSelector" =>
          new SuiteSelector()
        case "TestSelector" =>
          new TestSelector(
              obj.fld[String]("testName"))
        case "NestedSuiteSelector" =>
          new NestedSuiteSelector(
              obj.fld[String]("suiteId"))
        case "NestedTestSelector" =>
          new NestedTestSelector(
              obj.fld[String]("suiteId"),
              obj.fld[String]("testName"))
        case "TestWildcardSelector" =>
          new TestWildcardSelector(
              obj.fld[String]("testWildcard"))
        case tpe =>
          throw new IllegalArgumentException(s"Unknown Selector type: $tpe")
      }
    }
  }

}
