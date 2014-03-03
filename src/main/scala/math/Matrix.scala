package math

trait Matrix {
  
	def multiply(x:Vector[Vector[Double]],y:Vector[Vector[Double]])={
				
		def helper(left:Vector[Double],right:Vector[Double])=
				left.zip(right).map(a=>a._1*a._2).reduce((a,b)=>a+b)
	  
	    x.map{a=>y.map{b=>helper(a,b)}}
	}
	
	


}