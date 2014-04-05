package supervised

import org.apache.spark.rdd._
import scala.math.log

/**
input : Vector[ (the class name the document belong to , RDD of document) ]
引数の文書群は学習データ。前処理は済ましておく
雑念とした各文書を既に Set( (word,frequency) )　としてRDD化した上で用いる
**/
class ParallelNaive( var docs:Vector[ ( String , RDD[(String,Int)] ) ] )
{
	/** both strings as inputs above stand for classes here **/
	
	def wholeClass :Map[String,Vector[(String,RDD[(String,Int)])]] = docs.groupBy(elt=>elt._1)
	//全てのクラスとドキュメントの集合：Map( 各クラス-> Vector( ( 各クラス,ドキュメントの集合 ) ) )
	
	def allClassNames:Vector[String] = wholeClass.map(elt=>elt._1).toVector
	//全classのリスト作成
	
	def eachNumDocs:Map[String,Int] = wholeClass.map(elt=>(elt._1,elt._2.length))
	//Nc：各クラスに置けるdocument数、のMap : Map(class->number of docs)
	
	def sumNumDocs:Int = docs.size
	//ΣNc：総document数
	
	def eachNumWord(word:String)(class_name:String):Int = {
		var word_count = 0
		wholeClass(class_name).foreach{
			class_rdd =>  // == (class,rdd)
				word_count += 
					class_rdd._2.filter(word_occur=>word_occur._1==word)	//filter pair contains the word
					.take(1)(0)._2											//get frequency of the word 	
		}
		word_count
	}
	//N(w,c)：各クラスに置ける各単語の出現回数、のMap。できれば。もし未知語が来たらNoneではなく0を返す
	
	def eachProbWord(word:String)(class_name:String)(alpha:Int):Double={
		
		val Nwc = eachNumWord(word)(class_name).toDouble
		val Nc = eachNumDocs(class_name).toDouble
		
		log( ( Nwc+(alpha-1) ) / ( Nc + 2*(alpha-1) ) )
	}
	//log(Pw,c)
	//alpha is the parameter to decide how much we gonna make the data flat
	
	def eachProbClass(class_name:String):Double={
		
		val Nc = eachNumDocs(class_name).toDouble
		
		log( ( Nc+1 ) / ( sumNumDocs + NumClass ) )

	}
	
	def NumClass = wholeClass.size
	//|C|：クラスの種類数
		
		
	//前処理が済んでいるRDDを扱う。ただしcache化はしていないと仮定
	def classify(rdd:RDD[(String,Int)])(alpha:Int) = {
	
		val cached_rdd = rdd.cache()	//何度も使うのでcache化
	
		val ProbPerClass = 
			allClassNames.map{
				class_name => 
					cached_rdd
						.map { elt => eachProbWord(elt._1)(class_name)(alpha) * elt._2 }
						.reduce( (a,b) => a+b ) + eachProbClass(class_name)
				}	
		//list of probability that this document would belong to
		
		val max_class : (Double,Int) = ProbPerClass.zipWithIndex.max
		// ( probability , index of the class )
		
		allClassNames(max_class._2)
		//推定クラスを返す
		
		
		/**
		cached_rddの各要素：(word,frequency) => probability of the word * frequency in the document 
		各クラスに置ける各単語の頻度を返したい：Vector[RDD] ※ただしrddは単語ではなく確率を返す
		rddに含まれる全ての単語を、分類器を使って各クラス各単語出現確率(log)へ変換 
		reduceで各確率を足し合わせ、log事前確率P(c)を足したものをクラスごとに作成
		最も高いクラスを推定クラスとして(String,rdd)という形で既存の分類器にデータを追加
		**/
	}
}