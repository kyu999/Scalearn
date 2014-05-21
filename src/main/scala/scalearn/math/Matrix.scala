package scalearn.math

import scala.collection.mutable.ArrayBuffer

trait Matrix {
  
    def zipmapTwice(f: ( (Double, Double) ) => Double,
                    x: Vector[Vector[Double]], y: Vector[Vector[Double]] )
        
        = x.zip(y).map( vector => 
             vector._1.zip(vector._2).map( elt => f(elt) ) )  
        
	def add(left: Vector[Vector[Double]], right: Vector[Vector[Double]])={
	
      val plus = { elt: (Double, Double) => elt._1 + elt._2 }
    
      zipmapTwice(plus, left, right) 
          
	}

	def subtract(left: Vector[Vector[Double]], right: Vector[Vector[Double]])={
	
      val minus = { elt: (Double, Double) => elt._1 - elt._2 }
    
      zipmapTwice(minus, left, right) 
          
	}
    	
	def multiply(x:Vector[Vector[Double]], y:Vector[Vector[Double]]) = {
				
      def helper(left:Vector[Double],right:Vector[Double])
        = left.zip(right).map(a=>a._1*a._2).sum
		
      x.map{a=>y.map{b=>helper(a,b)}}
        
	}
}
