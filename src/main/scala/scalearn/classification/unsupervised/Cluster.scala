package scalearn.classification.unsupervised

import scala.collection.mutable.ArrayBuffer
import scalearn.preprocessing.TokenProbability

trait Cluster{

	val vectors:ArrayBuffer[Vector[Double]]
	//抽象メンバー。クラスでの引数に当たる

	val size = vectors.length
		
	def <+(that:Vector[Double]) = ( vectors += that )
	
	def ++(that:VectorCluster) = new VectorCluster( vectors ++ that.vectors)
	
	def center: Vector[Double] = { 
	  
	  var result: ArrayBuffer[Double] = ArrayBuffer()
	  
	  var lookraw = 0
      
	  val height = vectors.length
	  val width = vectors.head.length
	 
	  while(lookraw<width){
	      
	      var lookcol = 0
	      
	      var verticalsum = 0.0
	      
	      while(lookcol<height){
		    
		    verticalsum += vectors(lookcol)(lookraw)
		    lookcol+=1
		    
		  }
		  
		  result += verticalsum/height
		  lookraw += 1
		  
	   }
	  
	  result.toVector
	}  
  
    
}


case class VectorCluster(vectors:ArrayBuffer[Vector[Double]]) extends Cluster 


class ProbabilityCluster(tokenprobs:Vector[TokenProbability])// extends Cluster
{
  
  val center = tokenprobs.head		//これは一時的なものであって、完全に間違い
  
  val vectors:Vector[TokenProbability] = tokenprobs
    
  def ++(that:Vector[TokenProbability]) = new ProbabilityCluster(tokenprobs ++ that)
  
}
