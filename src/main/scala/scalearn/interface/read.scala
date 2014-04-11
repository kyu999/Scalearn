package scalearn.interface

import scala.io._
import scala.Iterator._

import scalearn.statistics._
import scalearn.statistics.Converter

import org.atilika.kuromoji._
import scalearn.preprocessing.tokenfactory.JP
import scalearn.parallel.SparkInstance

import org.apache.spark.rdd._
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._



object read {
  
    def retrieving(path:String,header:Boolean):Vector[String]={
      
      val source=Source.fromFile(path)
      
      try{
      
        source.getLines.map(_.toString).toVector
      
      }finally{
      
        source.close()
      
      } 
    }    
    
	def csv(path:String)(header:Boolean)={
	  
	  val resource=retrieving(path,header).map(_.split(","))
	  
	  if(header){
	  
	    val head=resource.head
	    val content=resource.tail.map(_.map(matching(_)).toVector)
	    content
	  
	  }else{
	  
	    resource.tail.map(_.map(a=>matching(a)).toVector)
	  
	  }
	}
	
	def rdds( 
		path:String ,
		cache_it:Boolean = true , 
		spark_context:SparkContext = SparkInstance.default
	):RDD[(String,Int)] = {
					
		val myfile =
            spark_context.textFile(path)
				.flatMap{ ( line:String ) => JP.mkToken(line).map(elt=>elt.getBaseForm) }
				.map(word => (word, 1))
				.reduceByKey(_ + _)
                                        
        if(cache_it) myfile.cache()
        else myfile
		
	}
	
	//cannot use like this ; rdds("document.txt",MySparkContext)
	//specify the parameter instead ; rdds("document.txt",spark_context = MySparkContext)	
	
	def matching(in:String)={
	  try{
	      in.toDouble
	  }catch{
	      case e: NumberFormatException => in  
	  }
	}
	
	
	def table={
      
    }
 

	def db={}
}


