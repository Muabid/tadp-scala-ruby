import Parsers._
import Estados._
import org.scalatest.{FreeSpec, Matchers}

class ParserTest extends FreeSpec with Matchers {
  def assertParsesSucceededWithResult[T](actualResult: T, expectedResult: T): Unit = {
    actualResult shouldBe(expectedResult)
  }

  def assertParseFailed[T](actualResult: ⇒ T): Unit = {
    assertThrows[ParserException](actualResult)
  }

  "Parsers" - {
    "when any parser test" - {
      "parses an empty list of notes" in {
        assertParsesSucceededWithResult(new AnyCharParser.parsear(""), Incorrecto)
      }
    }
  }
}
