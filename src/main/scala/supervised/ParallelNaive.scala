package supervised

import org.apache.spark.rdd._

//input : Vector[ (the class name the document belong to , RDD of document) ]

//雑念とした各文書を既に Set( (word,#occur) )　としてRDD化した上で用いる
class ParallelNaive( var docs:Vector[ ( String , RDD[(String,Int)] ) ] )
{
	/** both string stands for classes here **/

	def wholeWordTypes = {
		val new_rdd = docs.map(class_rdd=>class_rdd._2).reduce((a,b)=>a++b)
				//class_nameを削除したVectorに変換: Vector(:RDD[(String,Int)] )
				//全てのRDDを足し合わせて新しいRDD作成:RDD[(String,Int)] 
//		new_rdd.reduceByKey(_ + _)					//同一の単語をまとめる::RDD[(String,Int)] 
	}

	
	def wholeClass :Map[String,Vector[(String,RDD[(String,Int)])]] = docs.groupBy(elt=>elt._1)
	//全てのクラスとドキュメントの集合：Map( 各クラス-> Vector( ( 各クラス,ドキュメントの集合 ) ) )
	
	def eachNumDocs:Vector[Int] = wholeClass.map(elt=>elt._2.length).toVector
	//Nc：各クラスに置けるdocument数、のVector
	
	def sumNumDocs:Int = eachNumDocs.sum
	//ΣNc：総document数
	
	def eachNumWord(word:String)(class_name:String):Int = {
		var word_count = 0
		wholeClass(class_name).foreach{
			class_rdd =>  // == (class,rdd)
				word_count += 
					class_rdd._2.filter(word_occur=>word_occur._1==word)	// == (word,#occur)	
					.take(1)(0)._2											//take #occur 	
		}
		word_count
	}
	//N(w,c)：各クラスに置ける各単語の出現回数、のMap。できれば。もし未知語が来たらNoneではなく0を返す
	
	def NumClass = wholeClass.size
	//|C|：クラスの種類数
	
	//学習によって分類器の作成
	def study = {}
	
	def classify(rdd:RDD[String]) = {
		//rddに含まれる全ての単語を、分類器を使って各単語出現確率(log)へ変換
		//reduceで各確率を足し合わせる
		//最後にlog事前確率P(c)を足す
		//最も高いクラスを推定クラスとして(String,rdd)という形で既存の分類器にデータを追加
	}
}