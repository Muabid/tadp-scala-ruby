import TiposParser._
import org.scalatest.{FreeSpec, Matchers}

import scala.util.Success
import TiposParser._

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
        assertParsesSucceededWithResult(orParser.apply("alo"), Success('a'))
      }
      " al comparar el parser digit(a) y char[b] dando para que se parsee la palabra bal se devuelve success de b" in {
        val orParser= DigitParser <|> new CharParser('b')
        assertParsesSucceededWithResult(orParser.apply("bal"), Success('b'))
      }
      " al comparar el parser char(a) y alphaNum dando para que se parsee el string /// se devuelve failure" in {
        val orParser= new CharParser('a') <|>  AlphaNumParser
        assertParseFailed(orParser.apply("///").get)
      }
  }
    "<>" -{
      " al comparar el parser char(a) y char[a] con un palabra que empieze con a devuelve una tupla de a" in {
        val concatParser=new CharParser('a') <> new CharParser('l')
        assertParsesSucceededWithResult(concatParser.apply("alo"), Success(('a','l')))
      }
      " al combinar el parser string(hola) y string(carita del pacman) con el texto hola amiguito :v da una tupla de (hola,:v)" in {
        val concatParser=new StringParser("hola") <> new StringParser(":v")
        assertParsesSucceededWithResult(concatParser.apply("hola:v"), Success(("hola",":v")))
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
        assertParsesSucceededWithResult(concatParser.apply("dame El Pack :v"), Success(('d',Unit.box())))
      }


    }
}
}
