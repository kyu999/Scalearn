package scalearn.classification.supervised

import scala.math.log
import java.io.Serializable
import scala.collection.mutable.ListBuffer

import org.apache.spark.rdd._
import org.apache.spark.SparkContext._

import scalearn.io.read
import scalearn.parallel.SparkInstance
import org.apache.spark.SparkContext
     
/**
input : ListBuffer[ (the class name the document belong to , path to document ) ]
file_paths & docs are mutable !! Be careful. they are subject to side effect.
**/

case class NaiveBayes( 
    docs: ListBuffer[(String ,RDD[(String,Int)])] ) extends Serializable 
{
	
	def wholeClass: Map[String,ListBuffer[(String,RDD[(String,Int)])]] = docs.groupBy(elt=>elt._1)
	//全てのクラスとドキュメントの集合：Map( 各クラス-> Vector( ( 各クラス,ドキュメントの集合 ) ) )
	
	def allClassNames: List[String] = wholeClass.map(elt=>elt._1).toList
	//全classのリスト作成
	
	def eachNumDocs: Map[String,Int] = wholeClass.map(elt=>(elt._1,elt._2.length))
	//each == at each class
    //Nc：各クラスに置けるdocument数、のMap : Map(class->number of docs)
	
	def sumNumDocs: Int = docs.size
	//ΣNc：総document数
	
	def eachNumDocsAtWord(word: String, class_name: String ):Int = {
	
	    var doc_count = 0
	    
	    wholeClass(class_name).foreach{ 
	        class_rdd =>  // == (class,rdd)
			val filtered = class_rdd._2.filter{ word_occur => word_occur._1 == word }
				
			if (filtered.count != 0) doc_count += 1
		  }
		  doc_count
	}
	//N(w,c)：各クラスに置ける特定のwordが出現するdocument数
	
	def eachProbWord(word: String, class_name: String, alpha: Int = 2): Double = {
		
	    val Nwc = eachNumDocsAtWord(word , class_name).toDouble
	    val Nc = eachNumDocs(class_name).toDouble
	    
	    log( ( Nwc + (alpha-1) ) / ( Nc + 2*(alpha-1) ) )
	}
	//log(Pw,c)
	//alpha is the parameter to decide how much we gonna make the data flat
	
	def eachProbClass(class_name: String): Double = {
		
	    val Nc = eachNumDocs(class_name).toDouble
	    
	    log( ( Nc+1 ) / ( sumNumDocs + numClass ) )

	}
	
	val numClass :Int = wholeClass.size
	//|C|：クラスの種類数

	def classify(document: RDD[(String,Int)], alpha: Int = 2 ):(Double,String) = {
	
	    val array_word_freq: Array[(String,Int)] = document.collect
	    
	    val probPerClass: List[(Double,String)] = 
	        allClassNames.map{ class_name =>
	            val each_prob: Array[Double] =
	                array_word_freq.map { word_freq =>
	                    eachProbWord(word_freq._1 , class_name , alpha) * word_freq._2
	                    }
	            ( each_prob.sum + eachProbClass(class_name) , class_name )
	            
	            }
	            
	    println("probability of each class : " + probPerClass)
	    
	    val estimate_class: (Double,String) = probPerClass.max
	    	    
	    docs += Pair(estimate_class._2,document)
	    
	    estimate_class
	    
	    }
	
	/**
		cached_rddの各要素：(word,frequency) => probability of the word * frequency in the document 
		各クラスに置ける各単語の頻度を返したい：Vector[RDD] ※ただしrddは単語ではなく確率を返す
		rddに含まれる全ての単語を、分類器を使って各クラス各単語出現確率(log)へ変換 
		reduceで各確率を足し合わせ、log事前確率P(c)を足したものをクラスごとに作成
		最も高いクラスを推定クラスとして(String,rdd)という形で既存の分類器にデータを追加
	**/
		
}

object DoParallelNaive extends App{

  	val file_paths = 
  		ListBuffer( 
            ("plus","resource/doc1.txt"),
            ("plus","resource/doc2.txt"),
  		    ("plus","resource/doc3.txt"),
  		    ("minus","resource/doc4.txt"),
  		    ("minus","resource/doc5.txt"),
  		    ("minus","resource/doc6.txt")
  			  )
        
    val docs = 
        file_paths
        .map( class_path => ( class_path._1 , read.document(class_path._2) ) ) 
  	
  	val nb = NaiveBayes(docs)
             
    val new_doc = read.document("resource/examine.txt")
  	
    nb.classify(new_doc)
  	    
}