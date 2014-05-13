package scalearn.preprocessing.cleansing
    
object ImplicitConverter{

    implicit def stringToNumberSafely(value: String): ExtendString
        = new ExtendString(value)
                
}


class ExtendString(value: String){
    
    def intoInt = {
     
        try{
           value.toInt
        }catch{
           case e: NumberFormatException => 0
        }
        
    }

    def intoDouble = {
     
        try{
           value.toDouble
        }catch{
           case e: NumberFormatException => 0
        }
        
    }
    
}
