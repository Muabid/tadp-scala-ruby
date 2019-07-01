import TiposParser._
import org.scalatest.{FreeSpec, Matchers}

import scala.util.Success

class OperationsTest extends FreeSpec with Matchers {
  def assertParsesSucceededWithResult[T](actualResult: T, expectedResult: T): Unit = {
    actualResult shouldBe (expectedResult)
  }

  def assertParseFailed[T](actualResult: ⇒ T): Unit = {
    assertThrows[ParserException] (actualResult)
  }

  "Operations" - {
    "satisfies" - {
      " al aplicar alon a un parser satisfecho con una funcion que rompe si la longitud no es 4, el resultado es success" in {
        val tiene4Caracteres = (str:String) => str.length == 4
        val satisfiedParser = new CharParser('a').satisfies(tiene4Caracteres)
        assertParsesSucceededWithResult(satisfiedParser.apply("alon"), Success('a', 1))
      }
      "al aplicar blon a un parser satisfecho con una funcion que rompe si la longitud no es 4, el resultado es Failure por el parser" in {
        val tiene4Caracteres = (str:String) => str.length == 4
        val satisfiedParser = new CharParser('a').satisfies(tiene4Caracteres)
        assertParseFailed(satisfiedParser.apply("blon").get)
      }
      "al aplicar alon a un parser satisfecho con una funcion que rompe si la longitud no es 5, el resultado es Failure por la condicion" in {
        val tiene5Caracteres = (str:String) => str.length == 5
        val satisfiedParser = new CharParser('a').satisfies(tiene5Caracteres)
        assertParseFailed(satisfiedParser.apply("alon").get)
      }
      "al aplicar alon a un parser concat satisfecho con la condicion de lenght ==4 con 1 parser de string al y uno de char o da success" in {
        val tiene4Caracteres = (str:String) => str.length == 4
        val satisfiedParser = (new StringParser("al") <> new CharParser('o')).satisfies(tiene4Caracteres)
        assertParsesSucceededWithResult(satisfiedParser.apply("alon"),Success((("al",'o'),3)))
      }
      "al aplicar alon a un parser leftmost satisfecho con la condicion head == b con 1 parser de string al y uno de char o da Failure" in {
        val empiezaCon_b = (str:String) => str.charAt(0) == 'b'
        val satisfiedParser = (new StringParser("al") <~ new CharParser('o')).satisfies(empiezaCon_b)
        assertParseFailed(satisfiedParser.apply("alon").get)
      }
      "al aplicar blon a un parser leftmost satisfecho con la condicion head == b con 1 parser de string al y uno de char o da Succes" in {
        val empiezaCon_b = (str:String) => str.charAt(0) == 'b'
        val satisfiedParser = (new StringParser("bl") <~ new CharParser('o')).satisfies(empiezaCon_b)
        assertParsesSucceededWithResult(satisfiedParser.apply("blon"),Success(("bl",3)))
      }


    }
  }
}
