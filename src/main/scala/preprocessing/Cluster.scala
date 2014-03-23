package preprocessing

import scala.collection.mutable.ListBuffer

trait Cluster{

  val tokenvectors:Vector[FrequencyVector]
 
//  val center:FrequencyVector
  
  val size=tokenvectors.length
    
}

case class VectorCluster(tokenvectors:Vector[FrequencyVector]) extends Cluster
{
 
	def center = { 
	  
	  var result:ListBuffer[Double]=ListBuffer()
	  
	  var grandok=true
	  var lookraw=0
	  val height=tokenvectors.length
	  val width=tokenvectors.head.values.length
	 
	  while(grandok){
		  
		  var verticalok=true
		  var lookcol=0
		  var verticalsum=0.0
		  
		  if(lookraw>=width-1) grandok=false
		  
		  while(verticalok){
		    
		    if(lookcol>=height-1) verticalok=false
		    verticalsum+=tokenvectors(lookcol).values(lookraw)
		    lookcol+=1
		    
		  }
		  
		  result+=verticalsum/height
		  lookraw+=1
		  
	   }
	  
	  result
	}
	        
    def ++(that:Vector[FrequencyVector]) = new VectorCluster(tokenvectors ++ that)
    
	
}

class ProbabilityCluster(tokenprobs:Vector[TokenProbability])// extends Cluster
{
  
  val center=tokenprobs.head		//これは一時的なものであって、完全に間違い
  
  val vectors:Vector[TokenProbability]=tokenprobs
    
  def ++(that:Vector[TokenProbability])=new ProbabilityCluster(tokenprobs ++ that)
  
}