package unsupervised

import scala.collection.mutable.ArrayBuffer
import preprocessing.TokenProbability

trait Cluster{

	val freq_vectors:ArrayBuffer[Vector[Double]]
	//抽象メンバー。クラスでの引数に当たる

	val size=freq_vectors.length
		
	def +(that:Vector[Double]) = freq_vectors+=that
	
    def ++(that:VectorCluster) = new VectorCluster(freq_vectors ++ that.freq_vectors)
	
	def center:Vector[Double] = { 
	  
	  var result:ArrayBuffer[Double]=ArrayBuffer()
	  
	  var lookraw=0
	  val height=freq_vectors.length
	  val width=freq_vectors.head.length
	 
	  while(lookraw<width){
		  
		  var lookcol=0
		  var verticalsum=0.0
		  		  
		  while(lookcol<height){
		    
		    verticalsum+=freq_vectors(lookcol)(lookraw)
		    lookcol+=1
		    
		  }
		  
		  result+=verticalsum/height
		  lookraw+=1
		  
	   }
	  
	  result.toVector
	}  
  
    
}


case class VectorCluster(freq_vectors:ArrayBuffer[Vector[Double]]) extends Cluster 


class ProbabilityCluster(tokenprobs:Vector[TokenProbability])// extends Cluster
{
  
  val center=tokenprobs.head		//これは一時的なものであって、完全に間違い
  
  val vectors:Vector[TokenProbability]=tokenprobs
    
  def ++(that:Vector[TokenProbability])=new ProbabilityCluster(tokenprobs ++ that)
  
}