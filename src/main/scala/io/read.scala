package io

import scala.io._
import scala.Iterator._
import datafactory._
import Converter._

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
//	    content.naming(head)
	    content
	  }else{
	    resource.tail.map(_.map(a=>matching(a)).toVector)
	  }
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