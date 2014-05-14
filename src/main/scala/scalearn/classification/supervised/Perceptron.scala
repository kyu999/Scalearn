package scalearn.classification.supervised

import scalearn.general.ImplicitConverter._
    
case class BinaryPerceptron(
    initialWeight: Vector[Double],    
    studyData: Vector[(Boolean,Vector[Double])],
    stepScale: Double
    ){
    
    // true => class A ; false => class B
        
    def study: Vector[(Boolean,Vector[Double])] = { 
        
      var studyWeights
        = initialWeight
          
      var miss = true
          
      while(miss){
          
        miss = false
            
        studyData.foreach{ data => 
            
            if( (data._2 ** studyWeights).sum < 0 && data._1){
              miss = true
              studyWeights = studyWeights ++ ( data._2 * stepScale ) 
            }
            
            else if( (data._2 ** studyWeights).sum > 0 && !data._1){
              miss = true
              studyWeights = studyWeights -- ( data._2 * stepScale )
            }
                          
            else{}
        }
          
      }
    
  /*      trainData._2
          .map{ vector => 
            norm(vector :* initialWeight)  
              }
*/        
        
        Vector(
            ( true , Vector(1.0,2.2,3.4,4.7) ),
            ( false , Vector(14.0,32.2,3.324,4.47) )
        )
    }
    
    
    /*
    def classify(
      subject: DenseVector[Double], 
      studyWeights: Vector[(Vector[Double],className)] //重みベクトル群
    ): (Double,className) = {  
        
        //識別関数群    
        val discriminants: Vector[(Double,Boolean)] = 
            studyWeights
                .map( vector_class => 
                  ( (vector_class._1 * subject).sum , vector_class._2 )
                    ) 
                    
        discriminants.max

    } 
    
    */
    
}