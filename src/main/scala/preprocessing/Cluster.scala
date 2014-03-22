package preprocessing

trait Cluster{

  val values:Vector[TokenValues]
 
  val size=values.length
    
}

class VectorCluster(tokenvectors:Vector[TokenVector]) extends Cluster{
 
    val values:Vector[TokenVector] = tokenvectors
        
    def ++(that:Vector[TokenVector]) = new VectorCluster(tokenvectors ++ that)
    
	
}

class ProbabilityCluster(tokenprobs:Vector[TokenProbability]) extends Cluster{
  
  val values:Vector[TokenProbability]=tokenprobs
    
  def ++(that:Vector[TokenProbability])=new ProbabilityCluster(tokenprobs ++ that)
  
}