package parallel

import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._

/** singleton spark instance **/

object SparkInstance{
	
	val context = new SparkContext("local", "Scalearn")
	
	val config = ""
	
}