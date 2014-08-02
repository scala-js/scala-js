import scala.language.implicitConversions
import scala.scalajs.js
import scala.scalajs.js.Dynamic.global
import scala.scalajs.test.JasmineTest
import org.scalajs.jasmine.MatchResult
import org.scalajs.jasmine.JasmineExpectation

object CustomMatcherTest extends JasmineTest {

  implicit def pimpExpectation(exp: JasmineExpectation) = 
    exp.asInstanceOf[CustomExpectation]

  val toStartWith =  (result: MatchResult, expected: Any) => {
    result.actual.toString.startsWith(expected.toString)
  }

  beforeEach {
    currentSpec.addMatchers(js.Dictionary("toStartWith" -> toStartWith))
  }

  describe("JasmineTest") {

    it("should allow using custom matchers") {
      expect("Scala.js is awesome!") toStartWith "Scala.js"
    }
  }

  trait CustomExpectation extends JasmineExpectation {
    def toStartWith(exp: String): Unit
  }
}
