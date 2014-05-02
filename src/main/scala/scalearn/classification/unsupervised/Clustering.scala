package scalearn.classification.unsupervised

import scalearn.preprocessing._
import scala.collection.mutable.ArrayBuffer
import scala.util.Random._
import scalearn.statistics.Tools


object Clustering {
	
	def kmeans(
	    numberOfPrototype:Int,
	    vectors:ArrayBuffer[Vector[Double]],
	    max_iteration:Int = 100 
	    ):IndexedSeq[VectorCluster] = {
	    	
	  val size = vectors.length

		//k個の代表ベクトルを無作為に決め,事例集から削除しk個のクラスターを作成
		
		val initial_clusters:IndexedSeq[VectorCluster] =
		
		    ( 1 to numberOfPrototype ).map {  elt =>
		  
		        val pop_place:Int = nextInt(size-1)
		        val pop_value:Vector[Double] = vectors(pop_place)
		        
		        vectors.remove(pop_place)
		        VectorCluster(ArrayBuffer(pop_value))
		        
		        }
        		
		var previous_clustered:IndexedSeq[VectorCluster] = 
		    reclustering( vectors,initial_clusters )
            
		var after_clustered:IndexedSeq[VectorCluster] = 
		    reclustering( 
		    	vectors ,
		    	previous_clustered
		    	    .map( elt => VectorCluster(ArrayBuffer(elt.center)) ) 
		    	)
		
		var current_iteration = 1
		var keep_iteration = true
        
		while( (previous_clustered != after_clustered) && keep_iteration){
		    
		    if(current_iteration >= max_iteration) keep_iteration = false
		    
		    previous_clustered = after_clustered
		    
		    after_clustered = 
		        reclustering(
		            vectors,
		            previous_clustered
		                .map( elt => VectorCluster(ArrayBuffer(elt.center)) )
		            )
		            
		   current_iteration += 1
		}
		
		after_clustered
		
	}	
	
	//clusterとvectorのコレクションを受け取り新たなclusterを作成する
	def reclustering(
	    vectors:ArrayBuffer[Vector[Double]],
	    clusters:IndexedSeq[VectorCluster]
	    ) = {
		
		//残りの全てのvectorと各clusterとの類似度を求めて最も類似しているclusterにvectorを含める
		
		vectors.foreach{ vect =>
		    
		    var cluster_index = 0
		    var inserting_place = 0
		    
		    var maxsim:(VectorCluster,Double) = 
		        (clusters.head , Tools.pearRaw(clusters.head.center,vect))
		    
		    clusters.foreach{ cluster =>
		        val sim:(VectorCluster,Double) = 
		            (cluster , Tools.pearRaw(cluster.center,vect))
		        
		        if(sim._2 > maxsim._2){
		            inserting_place = cluster_index
		            maxsim = sim
		            }
		        
		        cluster_index += 1
		        
		        }
		
		//一致するclusterにmaxsimのvectorを突っ込む    
		    clusters(inserting_place) <+ vect
		    
		    } 
		
		clusters
		
		}
  
}
