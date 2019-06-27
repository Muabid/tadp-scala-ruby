import TiposParser._
import org.scalatest.{FreeSpec, Matchers}

import scala.util.{Failure, Success}

class ParserTest extends FreeSpec with Matchers {
  def assertParsesSucceededWithResult[T](actualResult: T, expectedResult: T): Unit = {
    actualResult shouldBe(expectedResult)
  }

  def assertParseFailed[T](actualResult: ⇒ T): Unit = {
    assertThrows[ParserException](actualResult)
  }

  "Parsers" - {
    "AnyChar" - {
      " un string que tiene un caracter da success" in {
        assertParsesSucceededWithResult(AnyCharParser.apply("hola"),Success('h'))
      }
      " un string vacio da failure" in {
        assertParseFailed(AnyCharParser.apply("").get)
      }
    }
    "CharParser" - {
      "un parser de c que recibe un chau da success" in {
        assertParsesSucceededWithResult(new CharParser('c').apply("chau"),Success('c'))
      }
      "un parser de c que recibe Hola da Failure" in {
        assertParseFailed(new CharParser('c').apply("Hola").get)
      }
      "un parser de c que recibe aCCCcccc da Failure" in {
        assertParseFailed(new CharParser('c').apply("aCCCcccc").get)
      }
    }
    "VoidParser" -{
      "al contener algo devuelve success de unit" in {
        assertParsesSucceededWithResult(VoidParser.apply("chau"),Success(Unit.box()))
      }
      "al contener algo vacio da failure" in {
        assertParseFailed(VoidParser.apply("").get)
      }
    }
    "LetterParser"- {
      "al parsear chau devuelve success de c" in {
        assertParsesSucceededWithResult(LetterParser.apply("chau"),Success('c'))
      }
      "al parsear hola devuelve success de h" in {
        assertParsesSucceededWithResult(LetterParser.apply("hola"),Success('h'))
      }
      "al parsear Doom devuelve success de D" in {
        assertParsesSucceededWithResult(LetterParser.apply("Doom"),Success('D'))
      }
      "al parsear 1234 da failure" in {
        assertParseFailed(LetterParser.apply("1234").get)
      }
      "al parsear 1234a devuelve success de a" in {
        assertParsesSucceededWithResult(LetterParser.apply("1234a"),Success('a'))
      }
    }
    "DigitParser"-{
      "al parsear 1234 devuelve Success 1" in{
        assertParsesSucceededWithResult(DigitParser.apply("1234"),Success('1'))
      }
      "al parsear a1234 devuelve Success 1" in{
        assertParsesSucceededWithResult(DigitParser.apply("a1234"),Success('1'))
      }
      "al parsear abcd da Failure" in{
        assertParseFailed(DigitParser.apply("abcd").get)
      }
    }
    "AlphaNumParser"-{
      "al parsear 1234 devuelve Success 1" in{
        assertParsesSucceededWithResult(AlphaNumParser.apply("1234"),Success('1'))
      }
      "al parsear a1234 devuelve Success a" in{
        assertParsesSucceededWithResult(AlphaNumParser.apply("a1234"),Success('a'))
      }
      "al parsear 1asss12 devuelve Success 1" in{
        assertParsesSucceededWithResult(AlphaNumParser.apply("1asss12"),Success('1'))
      }
      "al parsear un string vacio devuelve failure" in{
        assertParseFailed(AlphaNumParser.apply("").get)
      }
    }
    "StringParser"-{
      "en un Parser de bob esponja si se parsea bob devuelve success de bob" in{
        assertParsesSucceededWithResult(new StringParser("bob esponja").apply("bob"),Success("bob"))
      }
      "en un Parser de bob el constructor si se parsea constructor devuelve success de constructor" in{
        assertParsesSucceededWithResult(new StringParser("bob el constructor").apply("constructor"),Success("constructor"))
      }
      "en un Parser de bob esponja si se parsea calamardo devuelve failure" in{
        assertParseFailed(new StringParser("bob esponja").apply("calamardo").get)
      }
      "en un Parser de bob esponja si se parsea vacio devuelve success vacio" in{
        assertParsesSucceededWithResult(new StringParser("bob el constructor").apply(""),Success(""))
      }
      "en un Parser de vacio si se parsea vacio devuelve success vacio" in{
        assertParsesSucceededWithResult(new StringParser("").apply(""),Success(""))
      }
    }
  }
}
