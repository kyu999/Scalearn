package unsupervised

import scala.io.Source
import scala.xml._
import scala.collection.mutable.Map
import java.net.URL

class NaiveCluster {
  
	def readRSS(url:String):(String,xml.NodeSeq)={
	  
       //read url
	   val source = Source.fromURL(url) 
	   
	   //get feeds
	   val feeds:Elem=//XML.loadString(source.getLines.mkString)
	   XML.load(new URL(url))
	   
       val title=(feeds \\ "title").head.text	
       
       val summary=(feeds \\ "summary")
	   
	   val descriptions=if (summary.size>0) summary else (feeds \\ "description")
	 
	   
	   val store=List(//"feeds : "+feeds,
	       "title : "+title,"description : "+descriptions.text)
	   
	   store.foreach(println)
	   	   
	   source.close
	   
	   (title,descriptions)

	}
	
	def getwords(html:String):List[String] = {
	   
      "<[^>]+>".r.replaceAllIn(html, "").split("[^A-Z^a-z]+").
      map(_.toLowerCase).toList
        
	  }
	 
	def counting(title:String,descriptions:xml.NodeSeq):(String,Map[String, Int])={
	  // 取得したRSSフィードごとに単語頻出度をカウントアップします
    
	var wc = Map.empty[String, Int]
    
    for (description <- descriptions){
      for(word <- getwords(title + ' ' + description.text)){
        if(!wc.isDefinedAt(word)) wc(word) = 0
        wc(word) += 1
      } 
    }
    // タプルの形式で値を返します 
    (title, wc) 
    
	}
	
	def getwordcounts(url:String)={
	  val context=readRSS(url)
	  counting(context._1,context._2)
	}
	
	 // RSSフィードの読み込み集計処理です
  def parseFeed(datafile:String="resource/feedlist.txt"):(Int, 
    Map[String, Int], Map[String, Map[String, Int]]) = {
    
    // URL一覧のリストファイルを取得します
    val source = Source.fromFile(datafile)
    val feedlist = source.getLines().toList
    source.close
    
    // 集計用のミュータブルな変数を初期化します
    val apcount = Map.empty[String, Int]
    val wordcounts = Map.empty[String, Map[String, Int]]
     
    // URLごとに取得処理をします
    for(feedurl <- feedlist){
      try{
        // 単語頻出度を取得します
        var (title, wc) = getwordcounts(feedurl)
        // 集計を行います
        wordcounts(title) = wc
        // 全ブログの単語頻出度を記録します
        for(w <- wc){
          if(!apcount.isDefinedAt(w._1)) apcount(w._1) = 0
          if(1 < w._2) apcount(w._1) += 1
        }
        println("Success to parse feed " + feedurl)
      }catch{
        case e:Exception => println("Failed to parse feed " + feedurl)
      }
    }
    // 値をセットで返します
    return (feedlist.size, apcount, wordcounts)
  }
	
}

object ClusterTest extends App{
  
	val nc=new NaiveCluster
	val html=nc.readRSS("http://feeds.feedburner.com/37signals/beMH")
//	println(nc.getwords(html.toString))
	println(nc.parseFeed("resource/feedlist.txt"))
	
}