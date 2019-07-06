import Musica._
import TiposParser._

object PlayAudio extends App {
  val sariaSong = "F A B B F A B B F A B E D D B C B G E E D D E G E F A B B F A B B F A B E D D B C E B G G D E G E"

//  Ahora convertir la partitura a la melodía y pasarle eso al AudioPlayer les toca hacerlo a ustedes.
    //parsear a una lista de notas
  AudioPlayer.reproducir(MelodiaParser.apply("4AM1/8 5C1/8 5C#1/8 5C#1/8 5D#1/8 5C1/8 4A#1/8 4G#1/2").get._1)
  AudioPlayer.reproducir(MelodiaParser.apply("- 4A#1/8 4A#1/8 5C1/4 5C#1/8 4A#1/4 4G#1/2 5G#1/4 5G#1/4 5D#1/2").get._1)

  object NotaParser extends Parser[Nota] {

    val aParser : Parser[Nota]= new CharParser('A').const(A)
    val bParser : Parser[Nota]= new CharParser('B').const(B)
    val cParser : Parser[Nota]= new CharParser('C').const(C)
    val dParser : Parser[Nota]= new CharParser('D').const(D)
    val eParser : Parser[Nota]= new CharParser('E').const(E)
    val fParser : Parser[Nota]= new CharParser('F').const(F)
    val gParser : Parser[Nota]= new CharParser('G').const(G)
    val sostenidoParser : Parser[Char]= new CharParser('#')
    val bemolParser : Parser[Char]= new CharParser('b')

    val notaSimpleParser :Parser[Nota] = aParser <|> bParser <|> cParser <|> dParser <|> eParser <|> fParser <|> gParser
    val notaSostenidaParser :Parser[Nota] = (notaSimpleParser <~ sostenidoParser).map(res => res.sostenido)
    val notaBemolParser : Parser [Nota] = (notaSimpleParser <~ bemolParser).map(res => res.bemol)

    def apply(str: String): ParseResult[Nota] = {
      (notaSostenidaParser <|> notaBemolParser <|> notaSimpleParser)(str)
    }
  }



  object SilencioParser extends Parser[Silencio] {
    val silencioBlancaParser : Parser[Silencio]= new CharParser('_').const(Silencio(Blanca))
    val silencioNegraParser : Parser[Silencio]= new CharParser('-').const(Silencio(Negra))
    val silencioCorcheaParser : Parser[Silencio]= new CharParser('~').const(Silencio(Corchea))
    def apply(str: String): ParseResult[Silencio] = {
      (silencioBlancaParser <|> silencioNegraParser <|> silencioCorcheaParser)(str)
    }
  }


  object TonoParser extends Parser[Tono]{
    def apply(str: String): ParseResult[Tono] = {
      (DigitParser <> NotaParser).map(res => Tono(res._1.asDigit,res._2))(str)
    }
  }

  object FiguraParser extends Parser[Figura]{
    val figuraRedondaParser : Parser[Figura]= new StringParser("1/1").const(Redonda)
    val figuraBlancaParser : Parser[Figura]= new StringParser("1/2").const(Blanca)
    val figuraNegraParser : Parser[Figura]= new StringParser("1/4").const(Negra)
    val figuraCorcheaParser : Parser[Figura]= new StringParser("1/8").const(Corchea)
    val figuraSemiCorcheaParser : Parser[Figura]= new StringParser("1/16").const(SemiCorchea)
    def apply(str: String): ParseResult[Figura] = {
      (figuraSemiCorcheaParser <|> figuraBlancaParser <|> figuraNegraParser <|> figuraCorcheaParser <|> figuraRedondaParser)(str)
    }
  }


  object SonidoParser extends Parser[Sonido]{
    def apply(str: String): ParseResult[Sonido] = {
      (TonoParser <> FiguraParser).map(res => Sonido(res._1,res._2))(str)
    }
  }

  object AcordeParser extends Parser[Acorde]{
    val acordeExplicitoParser : Parser[Acorde]= (TonoParser.sepBy(new CharParser('+')) <> FiguraParser).map(res => Acorde(res._1,res._2))
    val acordeMayorParser : Parser[Acorde]= ((TonoParser <~ new CharParser('M')) <> FiguraParser).map(res => res._1.nota.acordeMayor(res._1.octava,res._2))
    val acordeMenorParser : Parser[Acorde]= ((TonoParser <~ new CharParser('m')) <> FiguraParser).map(res => res._1.nota.acordeMenor(res._1.octava,res._2))
    def apply(str: String): ParseResult[Acorde] = {
      (acordeMayorParser <|> acordeMenorParser <|> acordeExplicitoParser)(str)
    }
  }


  object MelodiaParser extends Parser[Melodia]{
    def apply(str: String): ParseResult[Melodia] = {
      (AcordeParser <|> SilencioParser <|> SonidoParser ).sepBy(new CharParser(' '))(str)
    }
  }



}
