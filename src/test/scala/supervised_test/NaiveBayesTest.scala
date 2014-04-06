package supervised_test
import org.scalatest.FunSuite
import org.apache.spark.rdd._

import supervised._
import io.read

class NaiveBayesTest extends FunSuite
{
  
  val t=NaiveBayes.time _
//  val f=NaiveBayes.examine _  //オブジェクトのメソッドは関数ではないので関数化するには　メソッド名+ _　 
//  VirtualData.examlist.map(x=>f(x))
  
  	val file_pathes = 
  		Vector( ("plus","resource/doc1.txt"),
  			    ("plus","resource/doc2.txt"),
  			    ("plus","resource/doc3.txt"),
  			    ("minus","resource/doc4.txt"),
  			    ("minus","resource/doc5.txt"),
  			    ("minus","resource/doc6.txt")
  			  )
  	
  	val examine_file = "resource/examine.txt"
  	
  	//documentに不備はない

	val vector_rdd : Vector[(String,RDD[(String,Int)])] = file_pathes.map( class_path => ( class_path._1 , read.rdds(class_path._2,false) ) )
/*	
		var word_count = 0

		val class_name = "minus"
		val word = "悪い"
		
		println("-------------------------------------------------problem part start")
		
		val mapped_rdds = vector_rdd.map{
			class_rdd =>  
				println(class_name)
				class_rdd._2.filter{word_occur=> word_occur._1==word}//.take(1)(0)._2
//				println(" take successfully!!!!!!!!!!!!!!!!!!!!!")
//				word_count += frequency
		}

//		println(mapped_rdds.map(elt=>elt.take(1))) //(0)._2)
		
		println("-------------------------------------------------problem part end")
		
		
		
		println (" word_count : "+ word_count )
*/
	
	val pn = ParallelNaive(file_pathes)
	println("eachNumWord (良い,plus): "+pn.eachNumWord("良い","plus"))

	println("result : "+pn.temp(examine_file) )
	
	println("wholeClass : "+pn.wholeClass)
	println("allClassNames : "+pn.allClassNames)
	println("eachNumDocs : "+pn.eachNumDocs)
	println("NumClass : "+pn.NumClass)
	println("sumNumDocs : "+pn.sumNumDocs)
	println("eachProbClass plus : "+pn.eachProbClass("plus"))
	println("eachProbClass minus: "+pn.eachProbClass("minus"))
	println("eachProbWord (良い,plus): "+pn.eachProbWord("良い","plus"))
	println("eachProbWord (良い,minus): "+pn.eachProbWord("良い","minus"))
	println("eachProbWord (悪い,plus): "+pn.eachProbWord("悪い","plus"))
	println("eachProbWord (おい,minus): "+pn.eachProbWord("悪い","minus"))

	
	
	

  
}