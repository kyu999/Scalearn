package scalearn.classification.supervised

import scalearn.statistics.Stats

case class KNN(trainData: Vector[(String , Vector[Double])] ){

    type className = String
        
    def getNeighbors(subject: Vector[Double], numberOfNeighbors: Int): Map[Int, String] = 
          trainData
            .map{ data => (data._1 , Stats.euclidean(subject, data._2)) } 
            .toVector
            .sortBy( class_vector => class_vector._2 ) 
            .takeRight(numberOfNeighbors)
            .groupBy(x=>x._1)
            .map(elt=>(elt._2.length,elt._1))
        
        
    def classify(subject: Vector[Double], numberOfNeighbors: Int)
        : (Int , className) 
    
    = getNeighbors(subject, numberOfNeighbors).max
                             

            
    
    /*K-NN法；すべての学習データと手元のデータとの距離をそれぞれもとめ、
      　　　　 距離が近い上位ｋ個の学習データを取り出し、その中で多数派であるクラスを
            　推定クラスとする方法
    */
}