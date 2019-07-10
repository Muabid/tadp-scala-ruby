import Musica._
import TiposParser._
import MusicParserV2._
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
    "NotaParser" - {
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
        assertParsesSucceededWithResult(NotaParser.apply("Cb"),Success((C,1)))
        assertThrows[IndexOutOfBoundsException] (C.bemol) // Que paso que no hay Cbemol?
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
    "SilencioParser" - {
      "al parsear _ da success de blanca" in{
        assertParsesSucceededWithResult(SilencioParser.apply("_"), Success(Silencio(Blanca),1))
      }
      "al parsear - da success de negra" in{
        assertParsesSucceededWithResult(SilencioParser.apply("-"), Success(Silencio(Negra),1))
      }
      "al parsear ~ da success de blanca" in{
        assertParsesSucceededWithResult(SilencioParser.apply("~"), Success(Silencio(Corchea),1))
      }
    }
    "TonoParser"-{
      "al parsear 8A da success" in{
        assertParsesSucceededWithResult(TonoParser.apply("8A"), Success(Tono(8,A),2))
      }
      "al parsear 9A# da success" in{
        assertParsesSucceededWithResult(TonoParser.apply("9A#"), Success(Tono(9,A.sostenido),3))
      }
      "al parsear 9Eb da success" in{
        assertParsesSucceededWithResult(TonoParser.apply("9Eb"), Success(Tono(9,E.bemol),3))
      }
  }
    "FiguraParser" -{
      "al parsear 1/1 da success" in{
        assertParsesSucceededWithResult(FiguraParser.apply("1/1"), Success(Redonda,3))
      }
      "al parsear 1/2 da success" in{
        assertParsesSucceededWithResult(FiguraParser.apply("1/2"), Success(Blanca,3))
      }
      "al parsear 1/4 da success" in{
        assertParsesSucceededWithResult(FiguraParser.apply("1/4"), Success(Negra,3))
      }
      "al parsear 1/8 da success" in{
        assertParsesSucceededWithResult(FiguraParser.apply("1/8"), Success(Corchea,3))
      }
      "al parsear 1/16 da success" in{
        assertParsesSucceededWithResult(FiguraParser.apply("1/16"), Success(SemiCorchea,4))
      }
    }
    "SonidoParser"-{
      "al parsear 5C#1/8 da success" in{
        assertParsesSucceededWithResult(SonidoParser.apply("5C#1/8"), Success(Sonido(Tono(5,C.sostenido),Corchea),6))
      }
      "al parsear 5C1/8 da success" in{
        assertParsesSucceededWithResult(SonidoParser.apply("5C1/8"), Success(Sonido(Tono(5,C),Corchea),5))
      }
      "al parsear 5D#1/2 da success" in{
        assertParsesSucceededWithResult(SonidoParser.apply("5D#1/2"), Success(Sonido(Tono(5,D.sostenido),Blanca),6))
      }
      "al parsear 1Bb1/16 da success" in{
        assertParsesSucceededWithResult(SonidoParser.apply("1Bb1/16"), Success(Sonido(Tono(1,B.bemol),SemiCorchea),7))
      }
      "al parsear 11Bb1/16 da failure " in{
        assertParseFailed(SonidoParser.apply("11Bb1/16").get)
      }
      "al parsear 1BbB1/16 da failure " in{
        assertParseFailed(SonidoParser.apply("1BbB1/16").get)
      }
      "al parsear 1Bb1/3 da failure " in{
        assertParseFailed(SonidoParser.apply("1Bb1/3").get)
      }
      "al parsear 1Ba1/3 da failure " in{
        assertParseFailed(SonidoParser.apply("1Ba1/3").get)
      }
      "al parsear 1Bb2/3 da failure " in{
        assertParseFailed(SonidoParser.apply("1Bb2/3").get)
      }
    }
    "AcordeParser" - {
      "al parsear 6A+6C#+6G1/8 da success" in{
        assertParsesSucceededWithResult(AcordeParser.apply("6A+6C#+6G1/8"), Success(Acorde(List(Tono(6,A),Tono(6,C.sostenido),Tono(6,G)),Corchea),12))
      }
      "al parsear 6G1/8 rompe por no ser un acorde" in{
        assertParseFailed(AcordeParser.apply("66G1/8").get)
      }
      "al parsear 6Cb+6Eb1/16 rompe por no ser un acorde" in{
        assertParsesSucceededWithResult(AcordeParser.apply("6C+6Eb1/16"), Success(Acorde(List(Tono(6,C),Tono(6,E.bemol)),SemiCorchea),10))
      }
      "al parsear 4AM1/8 da success" in {
        assertParsesSucceededWithResult(AcordeParser.apply("4AM1/8"), Success(Acorde(List(Tono(4,A), Tono(4,Cs), Tono(4,E)),Corchea),6))
      }
      "al parsear 4Am1/8 da success" in {
        assertParsesSucceededWithResult(AcordeParser.apply("4Am1/8"), Success(Acorde(List(Tono(4,A), Tono(4,C), Tono(4,E)),Corchea),6))
      }
    }
    "MusicaParser" - {
      "al parsear cancion bonus parte 1 devuelve success" in {
        assertParsesSucceededWithResult(MelodiaParser.apply("4AM1/8 5C1/8 5C#1/8 5C#1/8 5D#1/8 5C1/8 4A#1/8 4G#1/2"), Success((List(Acorde(List(Tono(4,A), Tono(4,Cs), Tono(4,E)),Corchea), Sonido(Tono(5,C),Corchea), Sonido(Tono(5,Cs),Corchea), Sonido(Tono(5,Cs),Corchea), Sonido(Tono(5,Ds),Corchea), Sonido(Tono(5,C),Corchea), Sonido(Tono(4,As),Corchea), Sonido(Tono(4,Gs),Blanca)),53)))
      }
      "al parsear cancion bonus parte 2 devuelve success" in {
        assertParsesSucceededWithResult(MelodiaParser.apply("- 4A#1/8 4A#1/8 5C1/4 5C#1/8 4A#1/4 4G#1/2 5G#1/4 5G#1/4 5D#1/2"), Success((List(Silencio(Negra), Sonido(Tono(4,As),Corchea), Sonido(Tono(4,As),Corchea), Sonido(Tono(5,C),Negra), Sonido(Tono(5,Cs),Corchea), Sonido(Tono(4,As),Negra), Sonido(Tono(4,Gs),Blanca), Sonido(Tono(5,Gs),Negra), Sonido(Tono(5,Gs),Negra), Sonido(Tono(5,Ds),Blanca)),63)))
      }
      "al parsear el feliz cumple parte 1 devuelve success" in {
        assertParsesSucceededWithResult(MelodiaParser.apply("4C1/4 4C1/4 4D1/2 4C1/4 4F1/2 4E1/2 4C1/8 4C1/4 4D1/2"), Success((List(Sonido(Tono(4,C),Negra), Sonido(Tono(4,C),Negra), Sonido(Tono(4,D),Blanca), Sonido(Tono(4,C),Negra), Sonido(Tono(4,F),Blanca), Sonido(Tono(4,E),Blanca), Sonido(Tono(4,C),Corchea), Sonido(Tono(4,C),Negra), Sonido(Tono(4,D),Blanca)),53)))
      }
      "al parsear el feliz cumple parte 2 devuelve success" in {
        assertParsesSucceededWithResult(MelodiaParser.apply("4C1/2 4G1/2 4F1/2 4C1/8 4C1/4 5C1/2 4A1/2 4F1/8 4F1/4 4E1/2 4D1/2"), Success((List(Sonido(Tono(4,C),Blanca), Sonido(Tono(4,G),Blanca), Sonido(Tono(4,F),Blanca), Sonido(Tono(4,C),Corchea), Sonido(Tono(4,C),Negra), Sonido(Tono(5,C),Blanca), Sonido(Tono(4,A),Blanca), Sonido(Tono(4,F),Corchea), Sonido(Tono(4,F),Negra), Sonido(Tono(4,E),Blanca), Sonido(Tono(4,D),Blanca)),65)))
      }

      "al parsear el feliz cumple completo devuelve success" in {
        assertParsesSucceededWithResult(MelodiaParser.apply("4C1/4 4C1/4 4D1/2 4C1/4 4F1/2 4E1/2 4C1/8 4C1/4 4D1/2 4C1/2 4G1/2 4F1/2 4C1/8 4C1/4 5C1/2 4A1/2 4F1/8 4F1/4 4E1/2 4D1/2"), Success((List(Sonido(Tono(4,C),Negra), Sonido(Tono(4,C),Negra), Sonido(Tono(4,D),Blanca), Sonido(Tono(4,C),Negra), Sonido(Tono(4,F),Blanca), Sonido(Tono(4,E),Blanca), Sonido(Tono(4,C),Corchea), Sonido(Tono(4,C),Negra), Sonido(Tono(4,D),Blanca), Sonido(Tono(4,C),Blanca), Sonido(Tono(4,G),Blanca), Sonido(Tono(4,F),Blanca), Sonido(Tono(4,C),Corchea), Sonido(Tono(4,C),Negra), Sonido(Tono(5,C),Blanca), Sonido(Tono(4,A),Blanca), Sonido(Tono(4,F),Corchea), Sonido(Tono(4,F),Negra), Sonido(Tono(4,E),Blanca), Sonido(Tono(4,D),Blanca)),119)))
      }
      "al parsear la cancion bonus completa devuelve success" in {
        assertParsesSucceededWithResult(MelodiaParser.apply("4AM1/8 5C1/8 5C#1/8 5C#1/8 5D#1/8 5C1/8 4A#1/8 4G#1/2 - 4A#1/8 4A#1/8 5C1/4 5C#1/8 4A#1/4 4G#1/2 5G#1/4 5G#1/4 5D#1/2"), Success((List(Acorde(List(Tono(4,A), Tono(4,Cs), Tono(4,E)),Corchea), Sonido(Tono(5,C),Corchea), Sonido(Tono(5,Cs),Corchea), Sonido(Tono(5,Cs),Corchea), Sonido(Tono(5,Ds),Corchea), Sonido(Tono(5,C),Corchea), Sonido(Tono(4,As),Corchea), Sonido(Tono(4,Gs),Blanca), Silencio(Negra), Sonido(Tono(4,As),Corchea), Sonido(Tono(4,As),Corchea), Sonido(Tono(5,C),Negra), Sonido(Tono(5,Cs),Corchea), Sonido(Tono(4,As),Negra), Sonido(Tono(4,Gs),Blanca), Sonido(Tono(5,Gs),Negra), Sonido(Tono(5,Gs),Negra), Sonido(Tono(5,Ds),Blanca)),117)))
      }

     }

}
}
