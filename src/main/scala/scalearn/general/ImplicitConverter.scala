package scalearn.general
    
object ImplicitConverter{

    implicit def stringToNumberSafely(value: String): ExtendString
        = new ExtendString(value)
                
    implicit def mathVector(vector: Vector[Double]): MathVector
        = new MathVector(vector)
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

class MathVector(vector: Vector[Double]){
    
    def **(that: Vector[Double]) = 
        vector
          .zip(that)
          .map( elt => elt._1 * elt._2 ) 
    
    def ++(that: Vector[Double]) = 
        vector
          .zip(that)
          .map( elt => elt._1 + elt._2 ) 

    def --(that: Vector[Double]) = 
        vector
          .zip(that)
          .map( elt => elt._1 - elt._2 ) 

    def *(scalar: Double) = vector.map( elt => elt * scalar ) 
    
    def +(scalar: Double) = vector.map( elt => elt + scalar ) 

    def -(scalar: Double) = vector.map( elt => elt - scalar ) 

}
