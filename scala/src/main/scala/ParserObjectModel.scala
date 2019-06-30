import Musica.A
import scala.util
import scala.util.{Failure, Success, Try}

// Todos reciben un string y devuelven un... estado? "Error de parseo" o "Parseo exitoso"

//ATENTI => estos parsers se van a combinar en funciones de orden superior. Entonces un parser deberia poder tomar
//el estado que devuelve el parser anterior para saber si debe o no actuar
/*
object ObjectParsers {


  trait FirstChar {
    def headCumpleLaCondicion(f: Char => Boolean, stringAChequear: String): Try[Char] = {
      f(stringAChequear.head) match {
        case true => Success(stringAChequear.head)
        case false => Failure(new ParserException("el head no cumple la condicion"))
      }
    }
  }

  trait SingleCharParser{
    def readenChars(v1: String): Int ={
      1
    }
  }

  class ParseResult[T](val value: Try[T], val readChars: Int){
    def cloneParser(newValue: Try[T]): ParseResult[T] ={
      new ParseResult[T](newValue,readChars)
    }
  }

  trait Parser[T] extends Function[String,ParseResult[T]] {

    def parseLogic(v1:String): Try[T]
    def readenChars(v1:String): Int

    def apply(v1: String) : ParseResult[T] = {
      new ParseResult(parseLogic(v1),readenChars(v1))
    }

    def verificarVacio(string: String): Try[String] = {
      string match {
        case "" => Failure(new ParserException("El string estaba vacio"))
        case _ => Success(string)
      }
    }
    def <|>[T](segundoParser: Parser[T]):Parser[T] = {

      val primerParser=this

      object ParserOr extends Parser [T]{

        def parseLogic(str:String): Try[T] ={
          primerParser.apply(str).value  match {
            case Success(x: T) => Success(x)
            case Failure(_) => segundoParser.apply(str).value
          }
        }

        def readenChars(str: String): Int = {
          primerParser.apply(str).value  match {
            case Success(x: T) => primerParser.apply(str).readChars
            case Failure(_) => segundoParser.apply(str).readChars
          }
        }

      }
      ParserOr
    }

    def <>[T](segundoParser:Parser[T]) : Parser[(T,T)] = {
      val primerParser = this
      object ConcatParser extends Parser [(T,T)]{

        def parseLogic(str:String): Try[(T,T)] ={
          val resPrimerParser=primerParser.apply(str)

          resPrimerParser.value match {
            case Success(x) => segundoParser.apply(str.substring(resPrimerParser.readChars)).value match{
              case Success(y) => Success((x,y))
              case Failure(y) => Failure(y)
            }
            case Failure(x) => Failure(x)
          }

        }

        def readenChars(str: String): Int = {
          1
        }

      }
      ConcatParser
    }



  def ~>[A](parser:Parser[A]) :Parser[A]={
    (str: String) => this.apply(str) match{
      case Success(x) => parser.apply(x.toString)
    }
  }


}

object AnyCharParser extends Parser[Char] with SingleCharParser {
  def parseLogic(stringAParsear: String): Try[Char] = {
    verificarVacio(stringAParsear).map(_.head)
  }
}

class CharParser( caracter: Char) extends Parser[Char] with SingleCharParser{
  def parseLogic(stringAParsear: String): Try[Char] = {
    stringAParsear.indexOf(caracter) match {
      case 0 => Success(caracter)
      case _ =>  Failure( new ParserException("No contiene el caracter"))
    }
  }
}

object VoidParser extends  Parser[Unit] with SingleCharParser{
  def parseLogic(stringAParsear: String): Try[Unit] = {
    verificarVacio(stringAParsear) match {
      case Success(_) => Success(Unit)
      case Failure(error: ParserException) => Failure(error)
    }
  }
}

object LetterParser extends  Parser[Char] with FirstChar with SingleCharParser{
  def parseLogic(stringAParsear: String): Try[Char] = {
    headCumpleLaCondicion(esLetra, stringAParsear)
  }
  def esLetra(c: Char): Boolean = c.toString.matches("""[a-zA-ZñÑáéíóúÁÉÍÓÚ\s]+""")
}

object DigitParser extends  Parser[Char] with FirstChar with SingleCharParser{
  def parseLogic(stringAParsear: String): Try[Char] = {
    headCumpleLaCondicion(esDigito, stringAParsear)
  }
  def esDigito(c: Char): Boolean = c.isDigit
}

object AlphaNumParser extends Parser[Char] with FirstChar with SingleCharParser{
  def parseLogic(stringAParsear: String): Try[Char] = {
    (LetterParser <|> DigitParser).apply(stringAParsear).value
  }
}

class StringParser(stringAEncontrar: String) extends Parser[String] {
  def parseLogic(texto: String): Try[String] = {
    if( texto.startsWith(stringAEncontrar)){
      Success(stringAEncontrar)
    } else {
      Failure(new ParserException("No se contiene el string:"+ stringAEncontrar + " en el texto"))
    }
  }
  def readenChars(v1: String): Int ={
    v1.length
  }
}
}


 */