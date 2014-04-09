package scalearn.classification.supervised

import org.apache.spark.rdd
import scala.math.log
import scalearn.interface.read
import scala.collection.mutable.ListBuffer

/**
input : Vector[ (the class name the document belong to , path to document ) ]
only the file_paths is mutable variable, the other things are just functions
**/


case class NaiveBayes( file_paths : ListBuffer[(String,String)] )
{	
	val docs:ListBuffer[(String ,Array[(String,Int)])] = 
		file_paths
			.map( class_path => ( class_path._1 , read.rdds(class_path._2,false).collect ) )
		
	def wholeClass :Map[String,ListBuffer[(String,Array[(String,Int)])]] = docs.groupBy(elt=>elt._1)
	//全てのクラスとドキュメントの集合：Map( 各クラス-> List( ( 各クラス,ドキュメントの集合 ) ) )
	
	def allClassNames:List[String] = wholeClass.map(elt=>elt._1).toList
	//全classのリスト作成
	
	def eachNumDocs:Map[String,Int] = wholeClass.map(elt=>(elt._1,elt._2.length))
	//Nc：各クラスに置けるdocument数、のMap : Map(class->number of docs)
	
	def sumNumDocs:Int = docs.size
	//ΣNc：総document数
	
	def eachNumWord(word:String , class_name:String ):Int = {
				
		var doc_count = 0

		wholeClass(class_name).foreach{
			class_array =>  // == (class,array)
				val filtered = class_array._2.filter{word_occur=> word_occur._1==word}

                if(!filtered.isEmpty) doc_count += 1
		}
		
		doc_count
	}
	//N(w,c)：各クラスに置ける各単語の出現回数、のMap。できれば。もし未知語が来たらNoneではなく0を返す
	
	def eachProbWord(word:String , class_name:String , alpha:Int = 2):Double = {
		
		val Nwc = eachNumWord(word , class_name).toDouble
		val Nc = eachNumDocs(class_name).toDouble
				
		log( ( Nwc+(alpha-1) ) / ( Nc + 2*(alpha-1) ) )
	}
	//log(Pw,c)
	//alpha is the parameter to decide how much we gonna make the data flat
	
	def eachProbClass(class_name:String):Double = {
		
		val Nc = eachNumDocs(class_name).toDouble
		
		log( ( Nc+1 ) / ( sumNumDocs + NumClass ) )

	}
	
	def NumClass = wholeClass.size
	//|C|：クラスの種類数
	

	def classify(doc_path:String , alpha:Int = 2 ):(Double,String) = {

		val arrayWord = read.rdds(doc_path).collect	//何度も使うのでcache化
								
		val ProbPerClass = 
			allClassNames.map{
				class_name => 					
										
					val each_prob = 
						arrayWord.map { word_freq => eachProbWord(word_freq._1 , class_name , alpha) * word_freq._2 }
																				
					( each_prob.sum + eachProbClass(class_name) , class_name )
				}	
		//list of probability that this document would belong to
		
        println("ProbPerClass : "+ProbPerClass)
        
		val estimate_class:(Double,String) = ProbPerClass.max
        
        file_paths += Pair(estimate_class._2,doc_path)
        
        docs += Pair(estimate_class._2,arrayWord)
        
        estimate_class
		//推定クラスを返す
		
		
		/**
		cached_rddの各要素：(word,frequency) => probability of the word * frequency in the document 
		各クラスに置ける各単語の頻度を返したい：Vector[RDD] ※ただしrddは単語ではなく確率を返す
		rddに含まれる全ての単語を、分類器を使って各クラス各単語出現確率(log)へ変換 
		reduceで各確率を足し合わせ、log事前確率P(c)を足したものをクラスごとに作成
		最も高いクラスを推定クラスとして(String,rdd)という形で既存の分類器にデータを追加
		
		if we need to classify in fast, just divide study phase and classify phase definitely 
			1. use "val" instead "def" above
			2. create Map instead function like eachNumWord , eachProbWord, eachProbClass
		**/
	}
    
		
}

object DoNaiveBayes extends App{

  	val file_paths = 
  		ListBuffer( ("plus","resource/doc1.txt"),
  			    ("plus","resource/doc2.txt"),
  			    ("plus","resource/doc3.txt"),
  			    ("minus","resource/doc4.txt"),
  			    ("minus","resource/doc5.txt"),
  			    ("minus","resource/doc6.txt")
  			  )
  	      
  	val pn = NaiveBayes(file_paths)  	
  	  	
 	println("classify : " + pn.classify("resource/examine.txt") ) 

    pn.classify("resource/examine2.txt")

    pn.classify("resource/examine3.txt")

    pn.classify("resource/examine4.txt")

    pn.classify("resource/examine5.txt")

    pn.classify("resource/doc2.txt")

    pn.classify("resource/doc1.txt")    
     	     
}