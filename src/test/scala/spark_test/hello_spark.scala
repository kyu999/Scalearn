
import org.scalatest.FunSuite

import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import scalearn.parallel.SparkInstance
import org.atilika.kuromoji._
import scalearn.preprocessing.tokenfactory.JP

/*
class hello_spark// extends FunSuite
{

    val sc = SparkInstance.context
 
    val myFile = sc.textFile("resource/doc1.txt")
    
    val counts = myFile.flatMap{line =>JP.mkToken(line).map(elt=>elt.getBaseForm)}
                        .map(word => (word, 1))
                        .reduceByKey(_ + _)
                        .take(1)//(0)._2
                        //take the first one ; it automatically convert into array, so take the first one again alghough it is singleton ; and take the second value = frequency
                        
     
     println("counts : "+counts+"--------------------------------------------------------")  
     counts.foreach(println)      
     val second = myFile.flatMap{line =>JP.mkToken(line).map(elt=>elt.getBaseForm)}
                        .map(word => (word, 1))
                        .reduceByKey(_ + _)
                        .take(1)(0)._2     
                        
     println("second : "+second)
    /**
    val double_counts=counts.join(counts)
    これをすると("word",(1,1))みたいな形になる
    println(double_counts.takeSample(false,1,2)+"--------------------------------------------")//.saveAsTextFile("resource/double.txt")
    println(counts.takeSample(false,1,2)+"--------------------------------------------")
    counts.filter(elt=>elt._1=="好く").foreach(println)//
 	**/   
    //counts.saveAsTextFile("resource/single.txt")
	
	println("""
	*********************
	*   Success Spark   *
	*********************""")
}

*/