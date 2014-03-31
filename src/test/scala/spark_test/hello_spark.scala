import org.scalatest.FunSuite
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.atilika.kuromoji._
import tokenfactory.JP

class hello_spark// extends FunSuite
{

    val spark = new SparkContext("local", "SparkTest")
 
    val myFile = spark.textFile("resource/test.txt")
    
    val counts = myFile.flatMap{line => println(line);JP.mkToken(line).map(elt=>elt.getBaseForm)}
                        .map(word => (word, 1))
                        .reduceByKey(_ + _)
 
    counts.saveAsTextFile("resource/out.txt")
 
	println("""
	*********************
	*   success spark   *
	*********************""")
}