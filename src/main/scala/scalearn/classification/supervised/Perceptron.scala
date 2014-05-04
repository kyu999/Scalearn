package scalearn.classification.supervised
 
import breeze.linalg._
import breeze.numerics._

case class Perceptron(){

    type className = String
        
    def study(): List[(DenseVector[Double],className)] = { 
        List(
            ( DenseVector(1.0,2.2,3.4,4.7) , "classA" ),
            ( DenseVector(14.0,32.2,3.324,4.47) , "classB" )
        )
    }
    
    def classify(
        subject: DenseVector[Double], 
        studyWeight: List[(DenseVector[Double],className)]
    ): (Double,className) = {  
        
        //重みベクトル
        val weightVectors = studyWeight
        
        //識別関数群    
        val discriminants: List[(Double,className)] = 
            weightVectors
                .map( vector_class => 
                     ( norm(vector_class._1 :* subject) , vector_class._2 )
                    ) 
                    
        discriminants.max
            

        
    
    } 
    
}