import Musica.A

import scala.runtime.Nothing$
import scala.util
import scala.util.{Failure, Success, Try,Either}
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
        case Success(x: (A,Int)) => Success(x)
        case Failure(_) => parser.apply(str)
      }
    }

    def <>[A](parser:Parser[A]) : Parser[(T,A)] = (str: String) =>{

      this.apply(str) match {
        case Success(x) => parser.apply(str.substring(x._2)) match {
          case Success(y) => Success(((x._1,y._1),x._2 + y._2))
          case Failure(y) => Failure(y)
        }
        case Failure(x) => Failure(x)
      }

    }

    def ~>[A](parser:Parser[A]) :Parser[A]= (str: String) =>{
      this.apply(str) match {
        case Success(x) => parser.apply(str.substring(x._2)) match {
          case Success(y : (A,Int)) => Success(y._1,y._2 + x._2)
          case Failure(y) => Failure(y)
        }
        case Failure(x) => Failure(x)
      }
    }
    //TODO hacer que los combinator de las flechitas <DE MIERDA> NO REPITAN LOGICA

    def <~[A](parser:Parser[A]) :Parser[T]= (str: String) =>{
      this.apply(str) match {
        case Success(x) => parser.apply(str.substring(x._2)) match {
          case Success(y : (A,Int)) => Success(x._1,y._2 + x._2)
          case Failure(y) => Failure(y)
        }
        case Failure(x) => Failure(x)
      }
    }

    def satisfies(condicion: String => Boolean): Parser[T] = (str: String) =>{
      this.apply(str) match {
        case Success(x) => if (condicion(str)) Success(x) else Failure(new ParserException("No se cumple la condicion dada"))
        case Failure(x)=>Failure(x)
      }
    }

    def opt: Parser[Any] = (str: String) => {
      this.apply(str) match {
        case Success(x:(T,Int)) => Success(x)
        case Failure(_) => Success(("",0))
      }
    }

    def * : Parser[List[T]]=(str: String) => {
      this.apply(str) match {
        case Failure(_) => Success((List(),0))
        case Success(resultado: (T,Int)) => {
          Success(resultado._1 :: this.*(str.substring(resultado._2)).get._1, resultado._2 + this.*(str.substring(resultado._2)).get._2)
        }
      }
    }

    def + : Parser[List[T]]=(str: String) => {
      this.*.apply(str) match {
        case Success((List(),0)) => Failure(new ParserException("No se cumple la condicion dada"))
        case other => other
      }
    }
    //   digit.sepBy(new CharParser('-'))  1-2-3-4-5-6 => devuelve Success((List(1,2,3,4,5),6))

    def sepBy[A](nuevoParser: Parser[A]) : Parser[List[T]] = (str:String)=> {
      (this <~ nuevoParser).+.apply(str) match {

        case Success(tuplaKleene) => {
          this.apply(str.substring(tuplaKleene._2)) match {
            case Success(tuplaFinal) => if (tuplaFinal._2 + tuplaKleene._2 == str.length) Success(( tuplaKleene._1 :+ tuplaFinal._1  , tuplaFinal._2 + tuplaKleene._2)) else Failure(new ParserException("El string no estaba bien separado"))
            case Failure(_) => Failure(new ParserException("el ultimo grupo del string no se puede parsear"))
          }
        }
        case other => other
      }
    }



    def const(valor: Any) : Parser[Any] = (str: String) => {
      this.apply(str).map(resultado => (valor, resultado._2))
    }

    def map[A](f: T => A) : Parser[A] = (str:String) => {
      this.apply(str).map(resultado => (f(resultado._1),resultado._2))
    }


  }

    object AnyCharParser extends Parser[Char] {
       def apply(stringAParsear: String): ParseResult[Char] = {
        verificarVacio(stringAParsear).map(_.head).map((_,1))
      }
    }

    class CharParser( caracter: Char) extends Parser[Char] {
      def apply(stringAParsear: String): ParseResult[Char] = {
        verificarVacio(stringAParsear).flatMap(_ => stringAParsear.indexOf(caracter) match {
          case 0 => Success(caracter).map((_,1))
          case _ =>  Failure( new ParserException("No contiene el caracter"))
        })
      }
    }

    object VoidParser extends  Parser[Unit] {
      def apply(stringAParsear: String): ParseResult[Unit] = {
        verificarVacio(stringAParsear) match {
          case Success(_) => Success((Unit,1))
          case Failure(error) => Failure(error)
        }
      }
    }

  object LetterParser extends  Parser[Char] with FirstChar {
    def apply(stringAParsear: String): ParseResult[Char] = {
      verificarVacio(stringAParsear).flatMap((str:String) => headCumpleLaCondicion(esLetra, stringAParsear).map((_,1)))
    }
    def esLetra(c: Char): Boolean = ('a' to 'z') union ('A' to 'Z') contains c
  }

  object DigitParser extends  Parser[Char] with FirstChar {
    def apply(stringAParsear: String): ParseResult[Char] = {
      verificarVacio(stringAParsear).flatMap((str:String) => headCumpleLaCondicion(esDigito, stringAParsear).map((_,1)))
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