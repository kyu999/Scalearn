package scalearn.parallel

import org.apache.spark.SparkConf
import org.apache.spark.SparkContext._
import org.apache.spark.SparkContext

/** singleton spark instance **/

object SparkInstance{
	
	val default = new SparkContext("local", "Scalearn")
		
	def generate(config:SparkConf) = new SparkContext(config)
	
}