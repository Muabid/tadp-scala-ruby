package object Estados{
  trait Estado

  case object Correcto(stringDevuelto: Optional[T]) extends Estado
  case object Incorrecto extends Estado


}