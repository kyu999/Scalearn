package clustering

import scala.io.Source
import scala.xml._
import scala.collection.mutable._
import java.net.URL

class NaiveCluster {
  
	def readRSS(url:String)={
	  
       //read url
	   val source = Source.fromURL(url) 
	   
	   //get feeds
	   val feeds:Elem=//XML.loadString(source.getLines.mkString)
	   XML.load(new URL(url))
	   
       val title=(feeds \\ "title").head.text	
       
       val summary=(feeds \\ "summary")
	   
	   val descrip=if (summary.size>0) summary else (feeds \\ "description")
	 
	   
	   val store=List(//"feeds : "+feeds,
	       "title : "+title,"description : "+descrip.text)
	   
	   store.foreach(println)
	   	   
	   source.close
	   
	   descrip

	}
	
	 def getwords(html:String):List[String] = {
	   
      "<[^>]+>".r.replaceAllIn(html, "").split("[^A-Z^a-z]+").
      map(_.toLowerCase).toList
        
	  }
	def getCount()={
	  
	}
}

object ClusterTest extends App{
  
	val nc=new NaiveCluster
	val html=nc.readRSS("http://feeds.feedburner.com/37signals/beMH")
	println(nc.getwords(html.toString))
	
}