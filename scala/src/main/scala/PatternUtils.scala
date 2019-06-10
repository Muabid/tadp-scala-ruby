object PatternUtils {
  def insidePattern(str: String) : String ={
    var contador:Int=1
    var adentroDelParentesis:String=""
    for(caracter<-str){

      if(caracter == '('){
        contador+=1
      }
      if(caracter == ')'){
        contador-=1
      }
      if(contador == 0){
        return adentroDelParentesis
      }
      adentroDelParentesis= adentroDelParentesis + caracter
    }
    return adentroDelParentesis
  }

   def untilFirstPattern(str: String): String={
    var substring=""
    for (caracter <- str){
      if(!caracter.isDigit){
        substring = substring + caracter
      }
      else{
        return substring
      }
    }
    return substring
  }
}
