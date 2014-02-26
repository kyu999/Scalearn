package io

import scala.io.Source._
import scala.Iterator._

object read {
  
    def retrieving:Vector[Any]={
      continually(readLine).takeWhile(_ != null).toVector
    } 
    
    def table={
      
    }
    
	def csv(path:String)(header:Boolean)={
	  if(header==true){
	    
	  }else{
	    
	  }
	}
	def csv(path:String)={
	  
	}
	
	def db={}
}