package io

import scala.io._
import scala.Iterator._

import datafactory._
import Converter._

import org.atilika.kuromoji._
import tokenfactory.JP
import parallel.SparkInstance

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
	
	def rdds(path:String,cache_or_not:Boolean = true,lang:String = "jp"):RDD[(String,Int)] = {
					
		val myfile = SparkInstance.context.textFile(path)
							.flatMap{ ( line:String ) => JP.mkToken(line).map(elt=>elt.getBaseForm) }
							.map(word => (word, 1))
							.reduceByKey(_ + _)
                                        
        if(cache_or_not) myfile.cache()
        else myfile
		
	}
	
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


