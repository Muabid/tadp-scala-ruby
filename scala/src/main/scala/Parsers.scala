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

    /**
    opt: convierte en opcional a un parser. Es decir, el nuevo parser siempre debería dar un resultado exitoso,
    pero si el parser original hubiese fallado, el resultado no contendrá ningún valor y no consumirá ningún caracter del input.
  Ejemplo:

        val talVezIn = string("in").opt
        val precedencia = talVezIn <> string("fija")

    precedencia parsea exitosamente las palabras "infija" y "fija"
    Si a precedencia le pasasemos “fija”, deberia devolver una tupla con un valor vacío y con el valor “fija”,
    porque talVezIn no habría consumido ningún carácter del texto original.
      */

    def opt: Parser[Any] = (str: String) => {
      this.apply(str) match {
        case Success(x:(T,Int)) => Success(x)
        case Failure(_) => Success(("",0))
      }
    }

    def * : Parser[List[T]]=(str: String) => {
      var charsLeidos = 0
        this.apply(str) match {
        case Failure(_) => Success((List(),0))
        case Success(resultado: (T,Int)) => {
          charsLeidos += resultado._2
          Success(resultado._1 :: this.*(str.substring(charsLeidos)).get._1, charsLeidos)
        }
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
        verificarVacio(stringAParsear).flatMap((x) => stringAParsear.indexOf(caracter) match {
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
    def esLetra(c: Char): Boolean = c.toString.matches("""[a-zA-ZñÑáéíóúÁÉÍÓÚ\s]+""")
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