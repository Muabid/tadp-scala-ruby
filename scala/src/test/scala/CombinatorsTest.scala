import TiposParser._
import org.scalatest.{FreeSpec, Matchers}

import scala.util.Success
import TiposParser._
import sun.security.jca.JCAUtil.CachedSecureRandomHolder

class CombinatorsTest extends FreeSpec with Matchers {
  def assertParsesSucceededWithResult[T](actualResult: T, expectedResult: T): Unit = {
    actualResult shouldBe (expectedResult)
  }

  def assertParseFailed[T](actualResult: ⇒ T): Unit = {
    assertThrows[ParserException] (actualResult)
  }

  "Combinators" - {
    "<|>" - {
      " al comparar el parser char(a) y char[b] dando para que se parsee la palabra alo se devuelve success de a" in {
        val orParser=new CharParser('a') <|> new CharParser('b')
        assertParsesSucceededWithResult(orParser.apply("alo"), Success('a',1))
      }
      " al comparar el parser digit(a) y char[b] dando para que se parsee la palabra bal se devuelve success de b" in {
        val orParser= DigitParser <|> new CharParser('b')
        assertParsesSucceededWithResult(orParser.apply("bal"), Success('b',1))
      }
      " al comparar el parser char(a) y alphaNum dando para que se parsee el string /// se devuelve failure" in {
        val orParser= new CharParser('a') <|>  AlphaNumParser
        assertParseFailed(orParser.apply("///").get)
      }
      " al comparar un char parser de a y un stringparser de hola, devuelve success de a " in {
        val orParser= new CharParser('a') <|>  new StringParser("hola")
        assertParsesSucceededWithResult(orParser.apply("aperro"),Success('a',1))
      }
  }
    "<>" -{
      " al comparar el parser char(a) y char[a] con un palabra que empieze con a devuelve una tupla de a" in {
        val concatParser=new CharParser('a') <> new CharParser('l')
        assertParsesSucceededWithResult(concatParser.apply("alo"), Success(('a','l'),2))
      }
      " al combinar el parser string(hola) y string(carita del pacman) con el texto hola amiguito :v da una tupla de (hola,:v)" in {
        val concatParser=new StringParser("hola") <> new StringParser(":v")
        assertParsesSucceededWithResult(concatParser.apply("hola:v"), Success(("hola",":v"),6))
      }
      " al combinar el parser string(hola) y digit con el texto holam da failure por culpa del digito" in {
        val concatParser=new StringParser("hola") <> DigitParser
        assertParseFailed(concatParser.apply("holam").get)
      }
      " al combinar el parser digit y string(hola) con el texto sigue dando failure ya que no importa el orden de los factores" in {
        val concatParser=DigitParser <> new StringParser("hola")
        assertParseFailed(concatParser.apply("holam").get)
      }
      " al combinar el parser alphanum y voidParser con el texto dame El Pack :v da una tupla de (d,unit)" in {
        val concatParser=AlphaNumParser <> VoidParser
        assertParsesSucceededWithResult(concatParser.apply("dame El Pack :v"), Success(('d',Unit.box()),2))
      }


    }

      "~>" -{
      " al combinar el parser letter con digit en el texto 'h123' devuelve una tupla con ('1',2)" in {
        val rightMostParser=LetterParser ~> DigitParser
        assertParsesSucceededWithResult(rightMostParser.apply("h123"), Success('1',2))
      }
      "al combinar un leterparser y stringParser(MOTO) apply(oMOTOcicleta) da success" in {
        val rightMostParser=LetterParser ~> new StringParser("MOTO")
        assertParsesSucceededWithResult(rightMostParser.apply("oMOTOCICLETA"), Success(("MOTO",5)))
      }
        "al combinarString parser de moto y moto devuele succes mo" in {
          val rightMostParser=new StringParser("MOTO") ~> new StringParser("MOTO")
          assertParsesSucceededWithResult(rightMostParser.apply("MOTOMOTO"), Success(("MOTO",8)))
        }
        "al falllar en el primer parseo rompe con el error del primer parseo" in {
          val rightMostParser=LetterParser ~> new StringParser("MOTO")
          assertParseFailed(rightMostParser.apply("111caca").get)
        }
        "al fallar en el segundo rompe con el segundo error" in {
          val rightMostParser= DigitParser ~> LetterParser
          assertParseFailed(rightMostParser.apply("111caca").get)
        }
      }
    "<~" -{
      " al combinar el parser letter con digit en el texto 'h123' devuelve una tupla con ('1',2)" in {
        val rightMostParser=LetterParser <~ DigitParser
        assertParsesSucceededWithResult(rightMostParser.apply("h123"), Success('h',2))
      }
      "al combinar un leterparser y stringParser(MOTO) apply(oMOTOcicleta) da success" in {
        val rightMostParser=LetterParser <~ new StringParser("MOTO")
        assertParsesSucceededWithResult(rightMostParser.apply("oMOTOCICLETA"), Success(('o',5)))
      }
      "al combinarString parser de moto y moto devuele succes mo" in {
        val rightMostParser=new StringParser("MOTO") <~ new StringParser("MOTO")
        assertParsesSucceededWithResult(rightMostParser.apply("MOTOMOTO"), Success(("MOTO",8)))
      }
      "al falllar en el primer parseo rompe con el error del primer parseo" in {
        val rightMostParser=LetterParser <~ new StringParser("MOTO")
        assertParseFailed(rightMostParser.apply("111caca").get)
      }
      "al fallar en el segundo rompe con el segundo error" in {
        val rightMostParser= DigitParser <~ LetterParser
        assertParseFailed(rightMostParser.apply("111caca").get)
      }
    }
}
}
