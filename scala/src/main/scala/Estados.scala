package object Estados{
  trait Estado

  case object Correcto(stringDevuelto: Optional[String]) extends Estado
  case object Incorrecto extends Estado


}