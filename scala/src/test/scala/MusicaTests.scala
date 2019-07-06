import Musica._
import TiposParser._
import PlayAudio._
import org.scalatest.{FreeSpec, Matchers}

import scala.util.Success

class MusicaTests extends FreeSpec with Matchers {
  def assertParsesSucceededWithResult[T](actualResult: T, expectedResult: T): Unit = {
    actualResult shouldBe (expectedResult)
  }

  def assertParseFailed[T](actualResult: ⇒ T): Unit = {
    assertThrows[ParserException] (actualResult)
  }

  "Reproductor" - {
    "Parser de musica" - {
      "el string A se parsea y devuelve success de a" in {
        assertParsesSucceededWithResult(NotaParser.apply("A"), Success(A,1))
        assertParsesSucceededWithResult(NotaParser.apply("B"), Success(B,1))
        assertParsesSucceededWithResult(NotaParser.apply("C"), Success(C,1))
        assertParsesSucceededWithResult(NotaParser.apply("D"), Success(D,1))
        assertParsesSucceededWithResult(NotaParser.apply("E"), Success(E,1))
        assertParsesSucceededWithResult(NotaParser.apply("F"), Success(F,1))
        assertParsesSucceededWithResult(NotaParser.apply("G"), Success(G,1))
      }
      "el string notab se parsea y devuelve success de nota bemol" in {
        assertParsesSucceededWithResult(NotaParser.apply("Ab"), Success(A.bemol,2))
        assertParsesSucceededWithResult(NotaParser.apply("Bb"), Success(B.bemol,2))
    //  assertParsesSucceededWithResult(NotaParser.apply("Cb"), Success(C.bemol,2))
        assertParsesSucceededWithResult(NotaParser.apply("Db"), Success(D.bemol,2))
        assertParsesSucceededWithResult(NotaParser.apply("Eb"), Success(E.bemol,2))
        assertParsesSucceededWithResult(NotaParser.apply("Fb"), Success(F.bemol,2))
        assertParsesSucceededWithResult(NotaParser.apply("Gb"), Success(G.bemol,2))
      }
      "el string nota# se parsea y devuelve success de nota sostenida" in {
        assertParsesSucceededWithResult(NotaParser.apply("A#"), Success(A.sostenido,2))
        assertParsesSucceededWithResult(NotaParser.apply("B#"), Success(B.sostenido,2))
        assertParsesSucceededWithResult(NotaParser.apply("C#"), Success(C.sostenido,2))
        assertParsesSucceededWithResult(NotaParser.apply("D#"), Success(D.sostenido,2))
        assertParsesSucceededWithResult(NotaParser.apply("E#"), Success(E.sostenido,2))
        assertParsesSucceededWithResult(NotaParser.apply("F#"), Success(F.sostenido,2))
        assertParsesSucceededWithResult(NotaParser.apply("G#"), Success(G.sostenido,2))
      }
  }
}
}
