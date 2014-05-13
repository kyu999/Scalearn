package scalearn.classification.supervised

import scalearn.statistics.Tools

case class KNN(trainData: Vector[(String , Vector[Double])] ){

    type className = String
        
    def classify(subject: Vector[Double], numberOfMajority: Int)
        : (Int , className) = {
                 
        val neighbors = 
            trainData.map{ class_vector => 
            (class_vector._1 , 
             Tools.euclidean(subject, class_vector._2)
             ) } 
                     .sortBy( class_vector => class_vector._2 ) 
                     .reverse
                     .take(numberOfMajority)
                     
        println("neighbors : "+neighbors)
                     
        val mostMajority = 
            neighbors    
                 .groupBy(x=>x._1)
                 .map(elt=>(elt._2.length,elt._1))
                 .max
                     
        mostMajority
        
    }

}
