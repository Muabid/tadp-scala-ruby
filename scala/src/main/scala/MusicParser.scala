import java.io.{PushbackReader, StringReader}
import Musica._
import scala.collection.mutable.ListBuffer

case class Note(name: String)

class MusicParser(input: String) {
  protected val inputStream = new PushbackReader(new StringReader(parsePattern(input)))

  protected def parseChar(): Char = {
    val parsed = inputStream.read()
    if (parsed == -1) throw new EOIParserException
    return parsed.toChar
  }

  protected def parsePattern(inputOriginal: String): String ={
    if(inputOriginal.exists(_.isDigit)){
      var inputTransformado:String=""
      val inputAntesDeAlgunDigito:String = PatternUtils.untilFirstPattern(inputOriginal)
      val vecesARepetir: Int =inputOriginal.charAt(inputAntesDeAlgunDigito.length()).asDigit
      val loDeAdentroDelParentesis:String = PatternUtils.insidePattern(inputOriginal.substring(inputAntesDeAlgunDigito.length()+3))
      val loDeLaDerechaDelParentesis:String = inputOriginal.substring(inputAntesDeAlgunDigito.length()+loDeAdentroDelParentesis.length()+5)

      inputTransformado=parsePattern(inputAntesDeAlgunDigito + (loDeAdentroDelParentesis+' ') * vecesARepetir + loDeLaDerechaDelParentesis)

      return inputTransformado
    }
    else{
      return inputOriginal
    }
  }

  protected def parseNote(): Nota = {
    var next: Char = ' '
    do next = parseChar() while (next == ' ')
      Nota.notas.find(_.toString == next.toString()).getOrElse(throw new NotANoteException(next))
  }

  def parse(): List[Nota] = {
    val result: ListBuffer[Nota] = ListBuffer()
    try while (true)
      result += parseNote()
    catch {
      case _: EOIParserException =>
    }
    return result.toList
  }
}

class ParserException(reason: String) extends Exception(reason)
class EOIParserException extends ParserException("reached end of input")
class NotANoteException(val read: Char) extends ParserException(s"Expected [A|B|C|D|E|F|G] but got $read")