package scalearn.classification.supervised

import org.apache.spark.rdd._
import scala.math.log
import scalearn.interface.read
import java.io.Serializable
import scala.collection.mutable.ListBuffer


/**
input : ListBuffer[ (the class name the document belong to , path to document ) ]
file_paths & docs are mutable !! Be careful. it has side effect.
**/

case class ParallelNaive( 
	file_paths : ListBuffer[(String,String)] , 
	spark_context:SparkContext = SparkInstance.default) extends Serializable 
{
	
	val docs:ListBuffer[(String ,RDD[(String,Int)])] = file_paths.map( class_path => ( class_path._1 , read.rdds(class_path._2,false,spark_context) ) )
	
	def wholeClass :Map[String,ListBuffer[(String,RDD[(String,Int)])]] = docs.groupBy(elt=>elt._1)
	//全てのクラスとドキュメントの集合：Map( 各クラス-> Vector( ( 各クラス,ドキュメントの集合 ) ) )
	
	def allClassNames:List[String] = wholeClass.map(elt=>elt._1).toList
	//全classのリスト作成
	
	def eachNumDocs:Map[String,Int] = wholeClass.map(elt=>(elt._1,elt._2.length))
	//Nc：各クラスに置けるdocument数、のMap : Map(class->number of docs)
	
	def sumNumDocs:Int = docs.size
	//ΣNc：総document数
	
	def eachNumWord(word:String , class_name:String ):Int = {

		println(" eachNumWord---------------------------------------------")
				
		var doc_count = 0

		wholeClass(class_name).foreach{
			class_rdd =>  // == (class,rdd)
				val filtered = class_rdd._2.filter{word_occur=> word_occur._1==word}
				
				if(filtered.count != 0) doc_count += 1
		}
		
		println("eachNumWord Done!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
		doc_count
	}
	//N(w,c)：各クラスに置ける各単語の出現回数、のMap。できれば。もし未知語が来たらNoneではなく0を返す
	
	def eachProbWord(word:String , class_name:String , alpha:Int = 2):Double={
		println(" eachProbWord---------------------------------------------")
		
		val Nwc = eachNumWord(word , class_name).toDouble
		val Nc = eachNumDocs(class_name).toDouble
		
		println("Nwc : "+Nwc)
		println("Nc : "+Nc)
		
		log( ( Nwc+(alpha-1) ) / ( Nc + 2*(alpha-1) ) )
	}
	//log(Pw,c)
	//alpha is the parameter to decide how much we gonna make the data flat
	
	def eachProbClass(class_name:String):Double={
		println(" eachProbClass---------------------------------------------")
		
		val Nc = eachNumDocs(class_name).toDouble
		
		log( ( Nc+1 ) / ( sumNumDocs + NumClass ) )

	}
	
	val NumClass = wholeClass.size
	//|C|：クラスの種類数
	
	def temp(doc_path:String) ={
	
		val cached_rdd = read.rdds(doc_path)
		
		println ( "cached_rdd : "+cached_rdd.collect() ) 	
		println ( "cached_rdd : "+cached_rdd.collect() ) 	//okならactionの回数は問題ではない
		
		val each_prob : List[RDD[Double]] =
			allClassNames.map{
				class_name => 	
						cached_rdd
							.map { elt => eachProbWord(elt._1 , class_name ) * elt._2 }
			}
					
		each_prob.map(_.take(1))

//					val sum_prob  = each_prob.reduce{ (a,b) => a+b } 
			
	}
	

	def classify(doc_path:String , alpha:Int = 2 ) = {

        val new_rdd = read.rdds(doc_path)
        
		val array_word_freq:Array[(String,Int)] = new_rdd.collect	
								
		val ProbPerClass:List[(Double,String)] = 
			allClassNames.map{
				class_name => 					
										
					val each_prob :Array[Double] = 
						array_word_freq.map { word_freq => eachProbWord(word_freq._1 , class_name , alpha) * word_freq._2 }
															
					( each_prob.sum + eachProbClass(class_name) , class_name )
				}	

		println("ProbPerClass : "+ProbPerClass)
		
		val estimate_class : (Double,String) = ProbPerClass.max
		
        file_paths += Pair(estimate_class._2,doc_path)
        
        docs += Pair(estimate_class._2,new_rdd)
        
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

object DoNaive extends App{

  	val file_paths = 
  		ListBuffer( ("plus","resource/doc1.txt"),
  			    ("plus","resource/doc2.txt"),
  			    ("plus","resource/doc3.txt"),
  			    ("minus","resource/doc4.txt"),
  			    ("minus","resource/doc5.txt"),
  			    ("minus","resource/doc6.txt")
  			  )
  	
  	val pn = ParallelNaive(file_paths)  	

/*
 	val cached_rdd = read.rdds("resource/examine.txt")	
 	     
 	val each_prob : List[RDD[String]] =
		pn.allClassNames.map{	
			class_name => 	
					cached_rdd
						.map { elt => ( pn.eachProbWord(elt._1 , class_name ) * elt._2 ).toString }
			}
     
	val head_prob = each_prob.head
	
//	println("head_prob : "+head_prob.take(1).head)  <= does not work. please try!
	
    println(pn.docs.map(elt=>elt._2.take(1).head))
    
    //this works!! why???
    
*/
    pn.classify("resource/examine.txt")
    
    file_paths.foreach(println)
    
    pn.docs.foreach(println)
    
    pn.classify("resource/examine.txt")
    
    
}