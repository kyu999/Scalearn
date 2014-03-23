package preprocessing

import scala.collection.mutable.ArrayBuffer

trait Cluster{

  val tokenvectors:ArrayBuffer[FrequencyVector]
 
//  val center:FrequencyVector
  
  val size=tokenvectors.length
    
}

case class VectorCluster(tokenvectors:ArrayBuffer[FrequencyVector]) extends Cluster
{
 
	def center = { 
	  
	  var result:ArrayBuffer[Double]=ArrayBuffer()
	  
	  var lookraw=0
	  val height=tokenvectors.length
	  val width=tokenvectors.head.values.length
	 
	  while(lookraw<width){
		  
		  var lookcol=0
		  var verticalsum=0.0
		  		  
		  while(lookcol<height){
		    
		    verticalsum+=tokenvectors(lookcol).values(lookraw)
		    lookcol+=1
		    
		  }
		  
		  result+=verticalsum/height
		  lookraw+=1
		  
	   }
	  
	  result
	}
	        
	def +(that:FrequencyVector) = tokenvectors+=that
	
    def ++(that:VectorCluster) = new VectorCluster(tokenvectors ++ that.tokenvectors)
	
	
    
	
}

case class VectorCluster2(freq_vectors:ArrayBuffer[Vector[Double]])// extends Cluster
{
 
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
	        
	def +(that:Vector[Double]) = freq_vectors+=that
	
    def ++(that:VectorCluster2) = new VectorCluster2(freq_vectors ++ that.freq_vectors)
	
	
    
	
}

class ProbabilityCluster(tokenprobs:Vector[TokenProbability])// extends Cluster
{
  
  val center=tokenprobs.head		//これは一時的なものであって、完全に間違い
  
  val vectors:Vector[TokenProbability]=tokenprobs
    
  def ++(that:Vector[TokenProbability])=new ProbabilityCluster(tokenprobs ++ that)
  
}