package scalearn.io

import scala.io._
import scala.Iterator._

import scalearn.statistics._
import scalearn.statistics.Converter

import org.atilika.kuromoji._
import scalearn.preprocessing.tokenfactory.JP
import scalearn.parallel.SparkInstance
import scalearn.general.ImplicitConverter._

import org.apache.spark.rdd._
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
    



object read {
    
    type Closable = { def close():Unit }

    def using[A <: Closable,B](resource: A)(f: A => B) = 
        
        try{
          f(resource)
        }finally{
          resource.close
        }
    
  
    def retrieving(path: String): Iterator[String]={
      
      val source = Source.fromFile(path)
      
      try{
      
        source.getLines
      
      }finally{
      
        source.close()
      
      } 
    }    
    
    
    
    def csv(path: String)(header: Boolean) = {
    
        val lines: Iterator[String] = retrieving(path)
        
        if(header){
        
            val head = lines.next.split(",")
            
            val body = lines.map(_.split(","))
            
            (head,body)
            
        }else lines.map(_.split(","))
        
    }
	
    def document( 
    	path: String ,
    	cache_it: Boolean = false , 
    	spark_context: SparkContext = SparkInstance.default
    	): RDD[(String,Int)] = {
    	
    	val myfile =
	    spark_context.textFile(path)
	        .flatMap{ (line:String) => JP.mkToken(line).map(elt=>elt.getBaseForm) }
	        .map(word => (word, 1))
	        .reduceByKey(_ + _)
                                        
        if(cache_it) myfile.cache()
        
        myfile
		
	}
    
    def event(
        path: String
    ) = {
        
    }
	
	//cannot use like this ; rdds("document.txt",MySparkContext)
	//specify the parameter instead ; rdds("document.txt",spark_context = MySparkContext)	
		
}


