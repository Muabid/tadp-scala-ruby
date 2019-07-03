import TiposParser._
import org.scalatest.{FreeSpec, Matchers}

import scala.util.Success

class OperationsTest extends FreeSpec with Matchers {
  def assertParsesSucceededWithResult[T](actualResult: T, expectedResult: T): Unit = {
    actualResult shouldBe (expectedResult)
  }

  def assertParseFailed[T](actualResult: ⇒ T): Unit = {
    assertThrows[ParserException] (actualResult)
  }

  "Operations" - {
    "satisfies" - {
      " al aplicar alon a un parser satisfecho con una funcion que rompe si la longitud no es 4, el resultado es success" in {
        val tiene4Caracteres = (str:String) => str.length == 4
        val satisfiedParser = new CharParser('a').satisfies(tiene4Caracteres)
        assertParsesSucceededWithResult(satisfiedParser.apply("alon"), Success('a', 1))
      }
      "al aplicar blon a un parser satisfecho con una funcion que rompe si la longitud no es 4, el resultado es Failure por el parser" in {
        val tiene4Caracteres = (str:String) => str.length == 4
        val satisfiedParser = new CharParser('a').satisfies(tiene4Caracteres)
        assertParseFailed(satisfiedParser.apply("blon").get)
      }
      "al aplicar alon a un parser satisfecho con una funcion que rompe si la longitud no es 5, el resultado es Failure por la condicion" in {
        val tiene5Caracteres = (str:String) => str.length == 5
        val satisfiedParser = new CharParser('a').satisfies(tiene5Caracteres)
        assertParseFailed(satisfiedParser.apply("alon").get)
      }
      "al aplicar alon a un parser concat satisfecho con la condicion de lenght ==4 con 1 parser de string al y uno de char o da success" in {
        val tiene4Caracteres = (str:String) => str.length == 4
        val satisfiedParser = (new StringParser("al") <> new CharParser('o')).satisfies(tiene4Caracteres)
        assertParsesSucceededWithResult(satisfiedParser.apply("alon"),Success((("al",'o'),3)))
      }
      "al aplicar alon a un parser leftmost satisfecho con la condicion head == b con 1 parser de string al y uno de char o da Failure" in {
        val empiezaCon_b = (str:String) => str.charAt(0) == 'b'
        val satisfiedParser = (new StringParser("al") <~ new CharParser('o')).satisfies(empiezaCon_b)
        assertParseFailed(satisfiedParser.apply("alon").get)
      }
      "al aplicar blon a un parser leftmost satisfecho con la condicion head == b con 1 parser de string al y uno de char o da Succes" in {
        val empiezaCon_b = (str:String) => str.charAt(0) == 'b'
        val satisfiedParser = (new StringParser("bl") <~ new CharParser('o')).satisfies(empiezaCon_b)
        assertParsesSucceededWithResult(satisfiedParser.apply("blon"),Success(("bl",3)))
      }
    }
    "opt" -{
      "al crear un parser opcional y darle algo invalido, da success vacio" in {
        val optionalParser = new CharParser('a').opt
        assertParsesSucceededWithResult(optionalParser.apply("blon"),Success(("",0)))
      }
      "al hacer el parser de tal vez in (ejemplo) y se parsea infija da success infija " in {
        val talVezIn = new StringParser("in").opt <> new StringParser("fija")
        assertParsesSucceededWithResult(talVezIn.apply("infija"),Success((("in","fija"),6)))
      }
      "al hacer el parser de tal vez in (ejemplo) y se parsea fija da success fija " in {
        val talVezIn = new StringParser("in").opt <> new StringParser("fija")
        assertParsesSucceededWithResult(talVezIn.apply("fija"),Success((("","fija"),4)))
      }
      "al parsear vamo los pibeh en el parcer concat de string con un char  opcional de h, da success" in {
        val optionalParser = new StringParser("vamo los pibe") <> new CharParser('h').opt
        assertParsesSucceededWithResult(optionalParser.apply("vamo los pibeh"),Success((("vamo los pibe",'h'),14)))
      }
      "al parsear vamo los pibe en el parcer concat de string con un char  opcional de h, da success" in {
        val optionalParser = new StringParser("vamo los pibe") <> new CharParser('h').opt
        assertParsesSucceededWithResult(optionalParser.apply("vamo los pibe"),Success((("vamo los pibe",""),13)))
      }
      "al parsear vacio en voidParser pero con optional da success" in {
        val optionalParser = VoidParser.opt
        assertParsesSucceededWithResult(optionalParser.apply(""),Success(("",0)))
      }
      "al definir una funcion en ruby sin parentesis da success" in {
        val optionalParser = new StringParser("def function ") <>
          new CharParser('(').opt <> new StringParser("parametro") <> new CharParser(')').opt
        assertParsesSucceededWithResult(optionalParser.apply("def function parametro"),Success((((("def function " ,""),"parametro"),""),22)))
      }
      "al definir una funcion en ruby con parentesis da success" in {
        val optionalParser = new StringParser("def function ") <>
          new CharParser('(').opt <> new StringParser("parametro") <> new CharParser(')').opt
        assertParsesSucceededWithResult(optionalParser.apply("def function (parametro)"),Success((((("def function " ,'('),"parametro"),')'),24)))
      }
    }
    "*" - {
      "kleene de un char(a) parsea los 4 chars de un string 'aaaa' y devuelve 4" in {
        val kleeneParser = new CharParser('a').*
        assertParsesSucceededWithResult(kleeneParser.apply("aaaa"),Success((List('a','a','a','a'),4)))
      }
      "kleene de un string('hola') parsea 2 'hola' seguidos en un string y devuelve 8 caracteres leidos" in {
        val kleeneParser = new StringParser("hola").*
        assertParsesSucceededWithResult(kleeneParser.apply("holahola"),Success((List("hola","hola"),8)))
      }
      /** DIRTY DEEDS DONE DIRTY CHEEP */
      "kleene de alpha parser parsea bien D4C y devuleve 3 caracteres leidos" in {
        val kleeneParser = AlphaNumParser.*
        assertParsesSucceededWithResult(kleeneParser.apply("D4C"),Success((List('D','4','C'),3)))
      }
      "string parser se queda corto con el string y devuelve solo lo que pudo parsear" in {
        val kleeneParser = new StringParser("alo").*
        assertParsesSucceededWithResult(kleeneParser.apply("alohomora"),Success((List("alo"),3)))
      }
      "char parser realiza 0 parseos porque hay solo un int" in {
        val kleeneParser = new CharParser('a').*
        assertParsesSucceededWithResult(kleeneParser.apply("5"),Success((List(),0)))
      }
    }

    "+" - {
      "clausula + rompe al aplicarse con un parser void y querer parsear ''" in {
        val plusParser = VoidParser.+
        assertParseFailed(plusParser.apply("").get)
      }
      "clausula + parsea 5 caracteres usando void parser a un string cualquiera" in {
        val plusParser = VoidParser.+
        assertParsesSucceededWithResult(plusParser.apply("aaffc"),Success((List((),(),(),(),()),5)))
      }
      "clausula + de letterParser devuelve todas las letras" in {
        val plusParser = LetterParser.+
        assertParsesSucceededWithResult(plusParser.apply("abc123"), Success((List('a','b','c'),3)))
      }
      "clausula + de anyCharParser devuelve todos los caracteres" in {
        val plusParser = AnyCharParser.+
        assertParsesSucceededWithResult(plusParser.apply("fff115"), Success((List('f','f','f','1','1','5'),6)))
      }
      "clausula + de stringParser de 'holis' devuelve holis holis holis y 15 caracteres leidos" in {
        val plusParser = new StringParser("holis").+
        assertParsesSucceededWithResult(plusParser.apply("holisholisholistarolis"), Success((List("holis","holis","holis"),15)))
      }
    }
    "const" - {
      "const de un numero devuelve un int" in {
        val constParser = new StringParser("123").const(true)
        assertParsesSucceededWithResult(constParser.apply("123fff"), Success(true,3))
      }
      "const falla y devuelve una excepcion de forma correscta" in {
        val constParser = DigitParser.const('f')
        assertParseFailed(constParser.apply("fafa").get)
      }
      "const parsera realiza un parseo exitoso y devuelve un Unit" in {
        val constParser = AlphaNumParser.const(Unit)
        assertParsesSucceededWithResult(constParser.apply("f5f5"),Success(Unit,1))
      }
      "const de kleene parsea bien y devuelve un nuemro" in {
        val constParser = DigitParser.+.const(7)
        assertParsesSucceededWithResult(constParser.apply("45435848"),Success(7,8))
      }
    }

  }
}
