import scala.scalajs.js
import scala.scalajs.js.Dynamic.global
import scala.scalajs.js.JSConverters._

import org.scalajs.jasminetest.JasmineTest

object CollectionTest extends JasmineTest {

  describe("Array") {

    it("should be able to map and filter integers") {
      val array = Array(5, 7, 2, 6, -30, 33, 66, 76, 75, 0)
      val result = array.filter(_.toInt % 3 != 0).map(x => x*x)
      expect(result.toJSArray).toEqual(js.Array(25, 49, 4, 76*76))
    }
  }
}
