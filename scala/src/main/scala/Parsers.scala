import TiposParser.DigitParser.devolverPrimerCharQueCumple

import scala.util
import scala.util.{Failure, Success, Try}
//Parsers.scala:

// Todos reciben un string y devuelven un... estado? "Error de parseo" o "Parseo exitoso"

//ATENTI => estos parsers se van a combinar en funciones de orden superior. Entonces un parser deberia poder tomar
//el estado que devuelve el parser anterior para saber si debe o no actuar
package object TiposParser {

  trait FirstChar {
    def devolverPrimerCharQueCumple(f: Char => Boolean, stringAChequear: String): Try[Char] = {
      for (caracter <- stringAChequear) {
        if (f(caracter)) {
          return Success(caracter)
        }
      }
      Failure(new ParserException("Ningun char devuelve la condicion"))
    }
    def esLetra(c: Char): Boolean = c.toString.matches("""[a-zA-ZñÑáéíóúÁÉÍÓÚ\s]+""")

    def esDigito(c: Char): Boolean = c.isDigit

    def esLetraODigito(c: Char): Boolean = esLetra(c) || esDigito(c)
  }

  trait Parser[T] extends Function[String,Try[T]]{
    def apply(v1: String): Try[T]

    def verificarVacio(string: String): Try[String] = {
      string match {
        case "" => Failure(new ParserException("El string estaba vacio"))
        case _ => Success(string)
      }
    }
  }

    object AnyCharParser extends Parser[Char] {
       def apply(stringAParsear: String): Try[Char] = {
        verificarVacio(stringAParsear).map(_.head)
      }
    }

    class CharParser( caracter: Char) extends Parser[Char] {
      def apply(stringAParsear: String): Try[Char] = {
        stringAParsear.indexOf(caracter) match {
          case 0 => Success(caracter)
          case _ =>  Failure( new ParserException("No contiene el caracter"))
        }
      }
    }

    object VoidParser extends  Parser[Unit] {
      def apply(stringAParsear: String): Try[Unit] = {
        verificarVacio(stringAParsear) match {
          case Success(_) => Success(Unit)
          case Failure(error: ParserException) => Failure(error)
        }
      }
    }

    object LetterParser extends  Parser[Char] with FirstChar {
      def apply(stringAParsear: String): Try[Char] = {
        devolverPrimerCharQueCumple(esLetra, stringAParsear)
      }
    }

    object DigitParser extends  Parser[Char] with FirstChar {
      def apply(stringAParsear: String): Try[Char] = {
        devolverPrimerCharQueCumple(esDigito, stringAParsear)
      }
    }

    object AlphaNumParser extends Parser[Char] with FirstChar {
      def apply(stringAParsear: String): Try[Char] = {
        devolverPrimerCharQueCumple(esLetraODigito, stringAParsear)
      }
    }

    class StringParser(texto: String) extends Parser[String] {
      def apply(stringAEncontrar: String): Try[String] = {
        texto.contains(stringAEncontrar) match {
          case true => Success(stringAEncontrar)
          case false => Failure(new ParserException("No se contiene el string en el texto"))
        }
      }
    }
}