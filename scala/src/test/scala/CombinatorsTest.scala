import TiposParser._
import org.scalatest.{FreeSpec, Matchers}

import scala.util.Success
import TiposParser._

class CombinatorsTest extends FreeSpec with Matchers {
  def assertParsesSucceededWithResult[T](actualResult: T, expectedResult: T): Unit = {
    actualResult shouldBe (expectedResult)
  }

  def assertParseFailed[T](actualResult: ⇒ T): Unit = {
    assertThrows[ParserException](actualResult)
  }

  "Combinators" - {
    "<|>" - {
      " al comparar el parser char(a) y char[b] dando para que se parsee la palabra alo se devuelve success de a" in {
        val parserNuevo=new CharParser('a') <|> new CharParser('b')
        assertParsesSucceededWithResult(parserNuevo.apply("alo"), Success('a'))
      }
      " al comparar el parser digit(a) y char[b] dando para que se parsee la palabra bal se devuelve success de b" in {
        val parserNuevo= DigitParser <|> new CharParser('b')
        assertParsesSucceededWithResult(parserNuevo.apply("bal"), Success('b'))
      }
      " al comparar el parser char(a) y alphaNum dando para que se parsee el string /// se devuelve failure" in {
        val parserNuevo= new CharParser('a') <|>  AlphaNumParser
        assertParseFailed(parserNuevo.apply("///").get)
      }
  }
}
}
