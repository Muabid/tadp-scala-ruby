//Parsers.scala:

// Todos reciben un string y devuelven un... estado? "Error de parseo" o "Parseo exitoso"

//ATENTI => estos parsers se van a combinar en funciones de orden superior. Entonces un parser deberia poder tomar
//el estado que devuelve el parser anterior para saber si debe o no actuar
import Estados._
object TiposParser {

  def esLetra(c:Char) : Boolean = c.toString.matches("""[a-zA-ZñÑáéíóúÁÉÍÓÚ\s]+""")

  trait Parser{
    def verificarVacio(string: String): Estado ={
      if string.equals("") return Incorrecto
    }
  }

  case object AnyCharParser extends Parser{
    def parsear(stringAParsear: String): Estado ={
      verificarVacio(stringAParsear)
      return Correcto(Some(stringAParsear.take(1)))
    }
  }
  case object CharParser(caracter: Char) extends Parser{
    def parsear(stringAParsear: String): Estado ={
      verificarVacio(stringAParsear)
      var retornoParseo = if (stringAParsear.contains(caracter))  Correcto(Some(caracter.toString)) else Incorrecto
      return retornoParseo
    }
  }
  case object VoidParser extends Parser{
    def parsear(stringAParsear: String): Estado ={
      verificarVacio(stringAParsear)
      return Correcto(None)
    }
  }
  case object LetterParser extends Parser{
    def parsear(stringAParsear: String): Estado ={
      verificarVacio(stringAParsear)
      for(caracter <- stringAParsear){
        if (esLetra(caracter)){
          return Correcto(Some(caracter.toString))
        }
      }
      return Incorrecto
    }
  }
  case object DigitParser extends Parser{
    def parsear(stringAParsear: String): Estado ={
      verificarVacio(stringAParsear)
      for(caracter <- stringAParsear){
        if (caracter.isDigit){
          return Correcto(Some(caracter.toString))
        }
      }
      return Incorrecto
    }
  }
  case object AlphaNumParser extends Parser{
    def cumpleConLetraODigito()

    def parsear (stringAParsear: String): Estado ={
      ???
    }
  }
  case object StringParser extends Parser

}