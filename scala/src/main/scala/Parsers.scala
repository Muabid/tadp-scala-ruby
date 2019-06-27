import scala.util
import scala.util.{Failure, Success, Try}
//Parsers.scala:

// Todos reciben un string y devuelven un... estado? "Error de parseo" o "Parseo exitoso"

//ATENTI => estos parsers se van a combinar en funciones de orden superior. Entonces un parser deberia poder tomar
//el estado que devuelve el parser anterior para saber si debe o no actuar
package object TiposParser {

  def esLetra(c: Char): Boolean = c.toString.matches("""[a-zA-ZñÑáéíóúÁÉÍÓÚ\s]+""")

  def esDigito(c: Char): Boolean = c.isDigit

  trait FirstChar {
    def devolverPrimerCharQueCumple(f: Char => Boolean, stringAChequear: String): Try[Char] = {
      for (caracter <- stringAChequear) {
        if (f(caracter)) {
          return Success(caracter)
        }
      }
      Failure(throw new ParserException("Ningun char devuelve la condicion"))
    }
  }

  trait Parser {

    def verificarVacio(string: String): Try[String] = {
      string match {
        case "" => Failure(new ParserException("El string estaba vacio"))
        case _ => Success(string)
      }
    }
  }
    object AnyCharParser extends Function[String, Try[Char]] with Parser {
      def apply(stringAParsear: String): Try[Char] = {
        verificarVacio(stringAParsear).map(_.head)
      }
    }

    class CharParser( caracter: Char) extends Function[String, Try[Char]] with Parser {
      def apply(stringAParsear: String): Try[Char] = {
        stringAParsear.indexOf(caracter) match {
          case 0 => Success(caracter)
          case _ =>  Failure( new ParserException("No contiene el caracter"))
        }
      }
    }

    object VoidParser extends Function[String, Try[Unit]] with Parser {
      def apply(stringAParsear: String): Try[Unit] = {
        verificarVacio(stringAParsear) match {
          case Success(_) => Success(Unit)
          case Failure(error: ParserException) => Failure(error)
        }
      }
    }

    object LetterParser extends Function[String, Try[Char]] with Parser with FirstChar {
      def apply(stringAParsear: String): Try[Char] = {
        devolverPrimerCharQueCumple(esLetra, stringAParsear)
      }
    }

    object DigitParser extends Function[String, Try[Char]] with Parser with FirstChar {
      def apply(stringAParsear: String): Try[Char] = {
        devolverPrimerCharQueCumple(esDigito, stringAParsear)
      }
    }

    object AlphaNumParser extends Function[String, Try[Char]] with Parser with FirstChar {
      def apply(stringAParsear: String): Try[Char] = {
        DigitParser.apply(stringAParsear) match {
          case Success(digito: Char) => Success(digito)
          case Failure(_) => LetterParser.apply(stringAParsear) match {
            case Success(letra: Char) => Success(letra)
            case Failure(_) => Failure( new ParserException("El string no tiene ni digitos ni letras"))
          }
        }
      }
    }

    class StringParser(texto: String) extends Function[String, Try[String]] with Parser {
      def apply(stringAEncontrar: String): Try[String] = {
        texto.contains(stringAEncontrar) match {
          case true => Success(stringAEncontrar)
          case false => Failure(new ParserException("No se contiene el string en el texto"))
          case _ => ???
        }
      }
    }
}