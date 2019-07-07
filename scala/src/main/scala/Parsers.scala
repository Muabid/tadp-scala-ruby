import Musica.A

import scala.runtime.Nothing$
import scala.util
import scala.util.{Failure, Success, Try,Either}
//Parsers.scala:

// Todos reciben un string y devuelven un... estado? "Error de parseo" o "Parseo exitoso"

//ATENTI => estos parsers se van a combinar en funciones de orden superior. Entonces un parser deberia poder tomar
//el estado que devuelve el parser anterior para saber si debe o no actuar
package object TiposParser {

  type ParseResult[T]=Try[(T,Int)]
  trait Parser[T] extends Function[String,ParseResult[T]] {

    def apply(v1: String): ParseResult[T]

    def <|>[A](parser: Parser[A]):Parser[A] ={
      str: String => this.apply(str)  match {
        case Success(x: (A,Int)) => Success(x)
        case Failure(_) => parser.apply(str)
      }
    }

    def <>[A](parser:Parser[A]) : Parser[(T,A)] = (str: String) =>{

      this.apply(str) match {
        case Success((res1,charsPrimerParser)) => parser.apply(str.substring(charsPrimerParser)) match {
          case Success((res2,charsSegundoParser)) => Success(((res1,res2),charsPrimerParser + charsSegundoParser))
          case Failure(y) => Failure(y)
        }
        case Failure(x) => Failure(x)
      }

    }
    def ~>[A](parser:Parser[A]) :Parser[A]= (str: String) =>{
      (this <> parser).map((res) => res._2)(str)
    }

    def <~[A](parser:Parser[A]) :Parser[T]= (str: String) =>{
      (this <> parser).map((res) => res._1)(str)
    }

    def satisfies(condicion: String => Boolean): Parser[T] = (str: String) =>{
      this.apply(str) match {
        case Success(x) => if (condicion(str)) Success(x) else Failure(new ParserException("No se cumple la condicion dada"))
        case Failure(x)=>Failure(x)
      }
    }

    def opt: Parser[Option[T]] = (str: String) => {
      this.apply(str) match {
        case Success((resultado,charsLeidos)) => Success((Some(resultado),charsLeidos))
        case Failure(_) => Success((None,0))
      }
    }

    def * : Parser[List[T]]=(str: String) => {
      this.apply(str) match {
        case Failure(_) => Success((List(),0))
        case Success((resParseo,charsParseados)) =>{
          val (listaResultados,charsTotalesLista) = this.*(str.substring(charsParseados)).get
          Success(resParseo :: listaResultados, charsParseados + charsTotalesLista)
        }
      }
    }

    def + : Parser[List[T]]=(str: String) => {
      this.*.apply(str) match {
        case Success((List(),0)) => Failure(new ParserException("No se cumple la condicion dada"))
        case other => other
      }
    }

    def sepBy[A](nuevoParser: Parser[A]) : Parser[List[T]] = (str:String)=> {
      (this <~ nuevoParser).+.apply(str) match {

        case Success(tuplaKleene) => {
          this.apply(str.substring(tuplaKleene._2)) match {
            case Success(tuplaFinal) => Success(( tuplaKleene._1 :+ tuplaFinal._1  , tuplaFinal._2 + tuplaKleene._2))
            case Failure(_) => Failure(new ParserException("el ultimo grupo del string no se puede parsear"))
          }
        }
        case other => other
      }
    }



    def const[A](valor: A) : Parser[A] = (str: String) => {
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
      def verificarVacio(string: String): Try[String] = {
        string match {
          case "" => Failure(new ParserException("El string estaba vacio"))
          case _ => Success(string)
        }
      }
    }

    class CharParser( caracter: Char) extends Parser[Char] {
      def apply(stringAParsear: String): ParseResult[Char] = {
        AnyCharParser.satisfies(str => str.head == caracter)(stringAParsear)
      }
    }

    object VoidParser extends  Parser[Unit] {
      def apply(stringAParsear: String): ParseResult[Unit] = {
        AnyCharParser.const(())(stringAParsear)
      }
    }

  object LetterParser extends  Parser[Char]  {
    def apply(stringAParsear: String): ParseResult[Char] = {
      AnyCharParser.satisfies(str => str.head.isLetter)(stringAParsear)
    }
  }

  object DigitParser extends  Parser[Char] {
    def apply(stringAParsear: String): ParseResult[Char] = {
      AnyCharParser.satisfies(str => str.head.isDigit)(stringAParsear)
    }
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