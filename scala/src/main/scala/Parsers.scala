//Parsers.scala:

// Todos reciben un string y devuelven un... estado? "Error de parseo" o "Parseo exitoso"

//ATENTI => estos parsers se van a combinar en funciones de orden superior. Entonces un parser deberia poder tomar
//el estado que devuelve el parser anterior para saber si debe o no actuar
import Estados._
object TiposParser {

  def esLetra(c:Char) : Boolean = c.toString.matches("""[a-zA-ZñÑáéíóúÁÉÍÓÚ\s]+""")

  trait Parser{
    def devolverPrimerCharQueCumple(f: Char => Boolean, stringAChequear: String): Estado{
      for(caracter <- stringAChequear){
        if (f(caracter)){
          return Correcto(Some(caracter))
        }
      }
      return Incorrecto
    }

    def parsear(string: String): T

    def verificarVacio(string: String): Estado ={
      if string.equals("") Incorrecto
    }
  }

  object AnyCharParser extends Parser{
    def parsear(stringAParsear: String): Estado ={
      verificarVacio(stringAParsear)
      Correcto(Some(stringAParsear.head))
    }
  }
  object CharParser(caracter: Char) extends Parser{
    def parsear(stringAParsear: String): Estado ={
      if (stringAParsear.contains(caracter))  Correcto(Some(caracter.toString)) else Incorrecto
    }
  }
  object VoidParser extends Parser{
    def parsear(stringAParsear: String): Estado ={
      verificarVacio(stringAParsear)
      return Correcto(Unit)
    }
  }

  object LetterParser extends Parser {
    def parsear(stringAParsear: String): Estado ={
      devolverPrimerCharQueCumple(esLetra,stringAParsear)
    }
  }

  object DigitParser extends Parser{
    def parsear(stringAParsear: String): Estado ={
      verificarVacio(stringAParsear)
      for(caracter <- stringAParsear){
        //ver como meter el isDigit en "devolverPrimerCharQueCumple"
        if (caracter.isDigit){
          return Correcto(Some(caracter.toString))
        }
      }
      return Incorrecto
    }
  }
  object AlphaNumParser extends Parser{
    def parsear(stringAParsear: String): Estado ={
      stringAParsear match {
        ???
      }
    }
  }
  object StringParser extends Parser

}