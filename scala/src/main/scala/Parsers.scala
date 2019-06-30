import Musica.A

import scala.util
import scala.util.{Failure, Success, Try}
//Parsers.scala:

// Todos reciben un string y devuelven un... estado? "Error de parseo" o "Parseo exitoso"

//ATENTI => estos parsers se van a combinar en funciones de orden superior. Entonces un parser deberia poder tomar
//el estado que devuelve el parser anterior para saber si debe o no actuar
package object TiposParser {


  trait FirstChar {
    def headCumpleLaCondicion(f: Char => Boolean, stringAChequear: String): Try[Char] = {
      f(stringAChequear.head) match {
        case true => Success(stringAChequear.head)
        case false => Failure(new ParserException("el head no cumple la condicion"))
      }
    }
  }

  type ParseResult[T]=Try[(T,Int)]
  trait Parser[T] extends Function[String,ParseResult[T]] {

    def apply(v1: String): ParseResult[T]

    def verificarVacio(string: String): Try[String] = {
      string match {
        case "" => Failure(new ParserException("El string estaba vacio"))
        case _ => Success(string)
      }
    }
    def <|>[A](parser: Parser[A]):Parser[A] ={
      (str: String) => this.apply(str)  match {
        case Success(x) => Success(x)
        case Failure(_) => parser.apply(str)
      }
    }

    def <>(parser:Parser[T]) : Parser[(T,T)] = (str: String) =>{

      this.apply(str) match {
        case Success(x) => parser.apply(str.substring(x._2)) match {
          case Success(y) => Success(((x._1,y._1),x._2 + y._2))
          case Failure(y) => Failure(y)
        }
        case Failure(x) => Failure(x)
      }

    }

    def ~>[A](parser:Parser[A]) :Parser[A]={
      (str: String) => this.apply(str) match{
        case Success(x) => parser.apply(x.toString)
      }
    }


  }

    object AnyCharParser extends Parser[Char] {
       def apply(stringAParsear: String): ParseResult[Char] = {
        verificarVacio(stringAParsear).map(_.head).map((_,1))
      }
    }

    class CharParser( caracter: Char) extends Parser[Char] {
      def apply(stringAParsear: String): ParseResult[Char] = {
        stringAParsear.indexOf(caracter) match {
          case 0 => Success(caracter).map((_,1))
          case _ =>  Failure( new ParserException("No contiene el caracter"))
        }
      }
    }

    object VoidParser extends  Parser[Unit] {
      def apply(stringAParsear: String): ParseResult[Unit] = {
        verificarVacio(stringAParsear) match {
          case Success(_) => Success(Unit).map((_,1))
          case Failure(error) => Failure(error)
        }
      }
    }

  object LetterParser extends  Parser[Char] with FirstChar {
    def apply(stringAParsear: String): ParseResult[Char] = {
      headCumpleLaCondicion(esLetra, stringAParsear).map((_,1))
    }
    def esLetra(c: Char): Boolean = c.toString.matches("""[a-zA-ZñÑáéíóúÁÉÍÓÚ\s]+""")
  }

  object DigitParser extends  Parser[Char] with FirstChar {
    def apply(stringAParsear: String): ParseResult[Char] = {
      headCumpleLaCondicion(esDigito, stringAParsear).map((_,1))
    }
    def esDigito(c: Char): Boolean = c.isDigit
  }

  object AlphaNumParser extends Parser[Char] {
    def apply(stringAParsear: String): ParseResult[Char] = {
      (LetterParser <|> DigitParser).apply(stringAParsear)
    }
  }

    class StringParser(stringAEncontrar: String) extends Parser[String] {
      def apply(texto: String): ParseResult[String] = {
        if( texto.startsWith(stringAEncontrar)){
          Success(stringAEncontrar).map((_,stringAEncontrar.length))
        } else {
          Failure(new ParserException("No se contiene el string:"+ stringAEncontrar + " en el texto"))
        }
      }
    }
}