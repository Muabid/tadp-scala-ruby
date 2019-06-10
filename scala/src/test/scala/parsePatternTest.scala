import Musica._
import org.scalatest.{FreeSpec, Matchers}

class parsePatternTest extends FreeSpec with Matchers {
  def assertParsesSucceededWithResult[T](actualResult: T, expectedResult: T): Unit = {
    actualResult shouldBe (expectedResult)
  }

  def assertParseFailed[T](actualResult: ⇒ T): Unit = {
    assertThrows[ParserException](actualResult)
  }

  "MusicParser" - {
    "When given a pattern with notes" - {
      "it parses them all right" in {
        assertParsesSucceededWithResult(new MusicParser("2x(A B 3x(F G 2x(A))) F B E").parse(), List(A, B, F, G, A, A, F, G, A, A, F, G, A, A, A, B, F, G, A, A, F, G, A, A, F, G, A, A, F, B, E))
      }
    }
    "When given an even simpler pattern" - {
      "works fine too" in {
        assertParsesSucceededWithResult(new MusicParser("3x(A E B) F C A").parse(), List(A, E, B, A, E, B, A, E, B, F, C, A))
      }
    }
//    "When there is no implementation" - {
  //    "use the misterious artifact" in {
    //    assertParsesSucceededWithResult(???, ???)
    // }
   // }
  }
}
