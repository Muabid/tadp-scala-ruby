import Musica.A
import TiposParser.VoidAutistParser

import scala.runtime.Nothing$
import scala.util
import scala.util.{Either, Failure, Success, Try}
//Parsers.scala:

// Todos reciben un string y devuelven un... estado? "Error de parseo" o "Parseo exitoso"

//ATENTI => estos parsers se van a combinar en funciones de orden superior. Entonces un parser deberia poder tomar
//el estado que devuelve el parser anterior para saber si debe o no actuar
package object TiposParser {

  type ParseResult[T] = Try[(T, Int)]

  trait Parser[T] extends Function[String, ParseResult[T]] {

    def apply(v1: String): ParseResult[T]

    def <|>[A](parser: Parser[A]): Parser[A] = {
      str: String =>
        this.apply(str) match {
          case Success(x: (A, Int)) => Success(x)
          case Failure(_) => parser.apply(str)
        }
    }

    def <>[A](parser: Parser[A]): Parser[(T, A)] = (str: String) => {

      this.apply(str) match {
        case Success((res1, charsPrimerParser)) => parser.apply(str.substring(charsPrimerParser)) match {
          case Success((res2, charsSegundoParser)) => Success(((res1, res2), charsPrimerParser + charsSegundoParser))
          case Failure(y) => Failure(y)
        }
        case Failure(x) => Failure(x)
      }

    }

    def ~>[A](parser: Parser[A]): Parser[A] = (str: String) => {
      (this <> parser).map((res) => res._2)(str)
    }

    def <~[A](parser: Parser[A]): Parser[T] = (str: String) => {
      (this <> parser).map((res) => res._1)(str)
    }

    def satisfies(condicion: T => Boolean): Parser[T] = (str: String) => {
      this.apply(str) match {
        case Success(x) => if (condicion(x._1)) Success(x) else Failure(new ParserException("No se cumple la condicion dada"))
        case Failure(x) => Failure(x)
      }
    }

    def opt: Parser[Option[T]] = (str: String) => {
      (this.map(res => Some(res)) <|> new VoidAutistParser(None))(str)
    }

    def * : Parser[List[T]] = (str: String) => {
      ((this <> this.*).map{ case (a,b) => {a::b}} <|> new VoidAutistParser(List()))(str)
    }

    def + : Parser[List[T]] = (str: String) => {
      this.*.apply(str) match {
        case Success((List(), 0)) => Failure(new ParserException("No se cumple la condicion dada"))
        case other => other
      }
    }

    def sepBy[A](nuevoParser: Parser[A]): Parser[List[T]] = (str: String) => {
      (this <~ nuevoParser).+.apply(str) match {
        case Success((res1,readed1)) => {
          this.apply(str.substring(readed1)) match {
            case Success((res2,readed2)) => Success((res1 :+ res2, readed2 + readed1))
            case Failure(_) => Failure(new ParserException("el ultimo grupo del string no se puede parsear"))
          }
        }
        case Failure(e) => this.map(res=> List(res)).apply(str)
      }
    }




    def const[A](valor: A): Parser[A] = (str: String) => {
      this.apply(str).map(resultado => (valor, resultado._2))
    }

    def map[A](f: T => A): Parser[A] = (str: String) => {
      this.apply(str).map(resultado => (f(resultado._1), resultado._2))
    }

  }

  object AnyCharParser extends Parser[Char] {
    def apply(str: String): ParseResult[Char] = {
      str match {
        case "" => Failure(new ParserException("String vacío"))
        case _ => Success(str).map(_.head).map((_, 1))
      }
    }
  }


  class CharParser(caracter: Char) extends Parser[Char] {
    def apply(stringAParsear: String): ParseResult[Char] = {
      AnyCharParser.satisfies(c => c == caracter)(stringAParsear)
    }
  }

  class VoidAutistParser[A](empty : A) extends Parser[A]{
    override def apply(v1: String): ParseResult[A] = Success((empty,0))
  }

  val VoidParser = AnyCharParser.const(())

  val LetterParser = AnyCharParser.satisfies(c => c.isLetter)

  val DigitParser = AnyCharParser.satisfies(c => c.isDigit)

  val AlphaNumParser = LetterParser <|> DigitParser

  class StringParser(stringAEncontrar: String) extends Parser[String] {
    def apply(texto: String): ParseResult[String] = {
      if (texto.startsWith(stringAEncontrar)) {
        Success(stringAEncontrar).map((_, stringAEncontrar.length))
      } else {
        Failure(new ParserException("No se contiene el string:" + stringAEncontrar + " en el texto"))
      }
    }
  }

}