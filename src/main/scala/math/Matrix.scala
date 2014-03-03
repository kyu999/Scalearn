package math

trait Matrix {
  
	def add(left:Vector[Vector[Double]],right:Vector[Vector[Double]])={
	  def helper(x:Vector[Double],y:Vector[Double])={
	    x.zip(y).map(c=>c._1+c._2)
	  }
	  left.zip(right).map(a=>helper(a._1,a._2))
	}
	
	def subtract(left:Vector[Vector[Double]],right:Vector[Vector[Double]])={
	  def helper(x:Vector[Double],y:Vector[Double])={
	    x.zip(y).map(c=>c._1-c._2)
	  }
	  left.zip(right).map(a=>helper(a._1,a._2))
	}
	
	def multiply(x:Vector[Vector[Double]],y:Vector[Vector[Double]])={
				
		def helper(left:Vector[Double],right:Vector[Double])=
				left.zip(right).map(a=>a._1*a._2).reduce((a,b)=>a+b)
		
	    x.map{a=>y.map{b=>helper(a,b)}}
	}
		
	def inverse(x:Vector[Vector[Double]])={
	  //行列式を求めたりしなきゃあかん
	}
	
	


}