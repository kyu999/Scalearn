import org.scalatest.FunSuite

import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._

class hello_spark extends FunSuite{

    val spark = new SparkContext("local", "SparkTest")
 
    val myFile = spark.textFile("resource/test.txt")
    
    val counts = myFile.flatMap(line => line.split(" "))
                        .map(word => (word, 1))
                        .reduceByKey(_ + _)
 
    counts.saveAsTextFile("out.txt")
 
	println("success spark!!")
}