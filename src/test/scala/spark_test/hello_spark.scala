import org.scalatest.FunSuite
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import parallel.SparkInstance
import org.atilika.kuromoji._
import tokenfactory.JP

class hello_spark extends FunSuite
{

    val sc = SparkInstance.context
 
    val myFile = sc.textFile("resource/doc1.txt")
    
    val counts = myFile.flatMap{line =>JP.mkToken(line).map(elt=>elt.getBaseForm)}
                        .map(word => (word, 1))
                        .reduceByKey(_ + _)
                        .cache() 
                   
    /**
    val double_counts=counts.join(counts)
    これをすると("word",(1,1))みたいな形になる
    println(double_counts.takeSample(false,1,2)+"--------------------------------------------")//.saveAsTextFile("resource/double.txt")
    println(counts.takeSample(false,1,2)+"--------------------------------------------")
    counts.filter(elt=>elt._1=="好く").foreach(println)//
 	**/   
    counts.saveAsTextFile("resource/single.txt")
	
	println("""
	*********************
	*   Success Spark   *
	*********************""")
}