package nlp

import preprocessing._
import scala.collection.mutable.ArrayBuffer
import scala.util.Random._
import datafactory._
import Converter._

object Clustering {

	
	def kmeans(k:Int,vectors:ArrayBuffer[Vector[Double]])={
		  		
		val size=vectors.length

		//k個の代表ベクトルを無作為に決め,事例集から削除しk個のクラスターを作成
		
		val first_clusters:IndexedSeq[VectorCluster] = ( 1 to k ).map{  elt=>
		  
		  val insert_place:Int=nextInt(size-1)
		  val insert_value:Vector[Double] = vectors(insert_place)
		  println("centers : "+insert_value)
		  vectors.remove(insert_place)
		  VectorCluster(ArrayBuffer(insert_value))
		 
		}
		
		def helper(clusters:IndexedSeq[VectorCluster]) = {
		
		println(vectors)
		
		//残りの全てのvectorと各clusterとの類似度を求めて最も類似しているclusterにvectorを含める
		
		vectors.foreach{ fv =>
		    
		    var cluster_index=0
		    var inserting_place=0
		    
			var maxsim:(VectorCluster,Double) = (clusters.head , dase(clusters.head.center.toVector.toda,fv.toda).pears.head )
			
			clusters.foreach{	cluster =>
			  
			  val sim:(VectorCluster,Double) = (cluster , dase(cluster.center.toVector.toda,fv.toda).pears.head )
			  
			  if(sim._2>maxsim._2){
			    inserting_place=cluster_index
			    maxsim=sim			    
			  }
			  
			  cluster_index+=1
  
			}
		//一致するclusterにmaxsimのvectorを突っ込む
		    
        //println("next vector : ")
		clusters(inserting_place)+fv
		
		} 
		
		clusters
		
		}
		
		var previous_clustered:IndexedSeq[VectorCluster] = helper(first_clusters)
		var after_clustered:IndexedSeq[VectorCluster] = helper( previous_clustered.map(elt=>VectorCluster(ArrayBuffer(elt.center))) )
		
		while(previous_clustered!=after_clustered){
			 previous_clustered=after_clustered
			 after_clustered=helper( previous_clustered.map(elt=>VectorCluster(ArrayBuffer(elt.center))) )
		}
		
		after_clustered
		
	}	
  
}