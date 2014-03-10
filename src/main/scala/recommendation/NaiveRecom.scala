package recommendation

import scala.math._
import datafactory._
import Converter._
import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer

object NaiveRecom {
  
	val data=
	  
Map("Lisa Rose" -> Map("Lady in the Water" -> 2.5,
      "Snakes on a Plane" -> 3.5, "Just My Luck" -> 3.0,
      "Superman Returns" -> 3.5, "You Me and Dupree" -> 2.5,
      "The Night Listener" -> 3.0),
    "Gene Seymour" -> Map("Lady in the Water" -> 3.0, "Snakes on a Plane" -> 3.5,
      "Just My Luck" -> 1.5, "Superman Returns" -> 5.0, "You Me and Dupree" -> 3.5,
      "The Night Listener" -> 3.0),
    "Michael Phillips" -> Map("Lady in the Water" -> 2.5, "Snakes on a Plane" -> 3.0,
      "Superman Returns" -> 3.5, "The Night Listener" -> 4.0),
    "Claudia Puig" -> Map("Snakes on a Plane" -> 3.5, "Just My Luck" -> 3.0,
      "Superman Returns" -> 4.0, "You Me and Dupree" -> 2.5,
      "The Night Listener" -> 4.5),
    "Mick LaSalle" -> Map("Lady in the Water" -> 3.0, "Snakes on a Plane" -> 4.0,
      "Just My Luck" -> 2.0, "Superman Returns" -> 3.0, "You Me and Dupree" -> 2.0,
      "The Night Listener" -> 3.0),
    "Jack Matthews" -> Map("Lady in the Water" -> 3.0, "Snakes on a Plane" -> 4.0,
      "Superman Returns" -> 5.0, "You Me and Dupree" -> 3.5,
      "The Night Listener" -> 3.0),
    "Toby" -> Map("Snakes on a Plane" -> 4.5, "Superman Returns" -> 4.0,
      "You Me and Dupree" -> 1.0))  
 		
 	//1. check whether item is in this list or not
 		//if true => ignore
 		//if false => take it
 	def itemlistup(prefs:Map[String,Map[String,Double]])={
	  
		var entity=ListBuffer("")
		
		prefs.foreach{in=>in._2.map{a=>
		   if(!entity.contains(a._1)) a._1+=:entity
		   }
		}
		entity-=""
	}
 		
 	def euclisim(prefs:Map[String,Map[String,Double]],person1:String,person2:String)={
		
		val p1list=prefs(person1).keys.toVector
		val overlapped=p1list.filter(a=>prefs(person2).get(a)!=None)

		val commons=p1list.map{a=>
		  if( prefs(person2).get(a)==None ) (a,0)
		  else (a,1)
		    }
		
//		println("commons : "+commons)
//		println("overlapped : "+overlapped)
		
/*		if(
		   commons.exists(_._2!=1)
		    ){ 0 }		//ここがバグ
		
		
		else	 {
		* 
		*/
		  
//		  val m1val=commons.map(a=>prefs(person1)(a._1))
//		  val m2val=commons.map(a=>prefs(person2)(a._1))			//ここでバグ  
		  //ここでエラー発生
		  
		 val m1val=overlapped.map(a=>prefs(person1)(a))
		 val m2val=overlapped.map(a=>prefs(person2)(a))
		 
		  val gapsquared=m1val.zip(m2val).map{
		    a=>pow(a._1-a._2,2)
		  }
		  		  
		  val sum=gapsquared.sum
		  		  
		  1/(1+sqrt(sum))
//		}
	}
	
 	//バグあり
	 def pearsim(prefs:Map[String,Map[String,Double]],person1:String,person2:String):Double={
		
		val p1list=prefs(person1).keys.toVector
		val commons=p1list.map{a=>
		  if( prefs(person2).get(a)==None ) (a,0)
		  else (a,1)
		    }.filter(freq=>freq._2>1)
		
//		if(commons.exists(_._2!=1)){ 0 }
		
//		else	 {
		  val m1val=commons.map(a=>prefs(person1)(a._1))
		  val m2val=commons.map(a=>prefs(person2)(a._1))		  
		  		  		  
		  val pear=dase(m1val.toda,m2val.toda).pears(0)
		  //dataset化してpearsonを求める
		  
		  pear
		  
		}
		
//	 }
	
	
	 //バグあり
	def topMatches(prefs:Map[String,Map[String,Double]],me:String,n:Int,similarity:(Map[String,Map[String,Double]],String,String)=>Double)={
	  
	  val people=prefs.keys.filter(a=>a!=me).toList
	  val similar=people.map{person=>
	    println("")
	    println("person : "+person+", each similarity : "+similarity(prefs,me,person))
	    (similarity(prefs,me,person),person)}.toList
	    println("")
	    println("similarity : "+similar)
	  
	  similar.sorted.reverse.take(n)
	  
	}
	
	
	//transformは問題なし
	def transform(prefs:Map[String,Map[String,Double]])={

	  var result=Map("item"->Map("name"->0.0))
	  
	  prefs.foreach{person=>
	    val fullname=person._1
	    val items=person._2
	    items.foreach{
	    		item=>
	    		  val itemname=item._1
	    		  val value=item._2
	    		  result.get(itemname) match{
	    		    case Some(x)=>x.put(fullname,value)
	    		    case None=>result.put(itemname,Map(fullname->value))
	    		    }
	    		}
	    }
	  
	  result.remove("item")
	  result
	  
	}
	
	def getRecommendations(prefs:Map[String,Map[String,Double]],person:String,similarity:(Map[String,Map[String,Double]],String,String)=>Double)={

	  
	  

	}
	
	
}
