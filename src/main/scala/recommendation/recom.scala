package recommendation

import scala.math._
import datafactory._

class recom {
  
	val data=
	  
	  Map(
	    "Lisa Rose"->
	  		Map("Lady in the Water"->2.5, "Snakes on a Plane"->3.5,
 "Just My Luck"->3.0, "Superman Returns"->3.5, "You, Me and Dupree"->2.5, 
 "The Night Listener"-> 3.0),
 	    "Gene Seymour"-> 
	  		Map("Lady in the Water"-> 3.0, "Snakes on a Plane"-> 3.5, 
 "Just My Luck"-> 1.5, "Superman Returns"-> 5.0, "The Night Listener"-> 3.0, 
 "You, Me and Dupree"-> 3.5), 
 		"Michael Phillips"-> Map("Lady in the Water"-> 2.5, "Snakes on a Plane"-> 3.0,
 "Superman Returns"-> 3.5, "The Night Listener"-> 4.0),
 		"Claudia Puig"-> Map("Snakes on a Plane"-> 3.5, "Just My Luck"-> 3.0,
 "The Night Listener"-> 4.5, "Superman Returns"-> 4.0, "You, Me and Dupree"-> 2.5),
 		"Mick LaSalle"-> Map("Lady in the Water"-> 3.0, "Snakes on a Plane"-> 4.0, 
 "Just My Luck"-> 2.0, "Superman Returns"-> 3.0, "The Night Listener"-> 3.0,
 "You, Me and Dupree"-> 2.0), 
 		"Jack Matthews"-> Map("Lady in the Water"-> 3.0, "Snakes on a Plane"-> 4.0,
 "The Night Listener"-> 3.0, "Superman Returns"-> 5.0, "You, Me and Dupree"-> 3.5),
 		"Toby"-> Map("Snakes on a Plane"->4.5,"You, Me and Dupree"->1.0,"Superman Returns"->4.0))
 		
 	def sim_distance(prefs:Map[String,Map[String,Double]],man1:String,man2:String)={
		
		val p1list=prefs(man1).keys.toVector
		val bothlist=p1list.map{a=>
		  if( prefs(man2).get(a)==None ) (a,0)
		  else (a,1)
		    }
		
		println("p1list : "+p1list)
		println("bothlist : "+bothlist)
		
		if(bothlist.exists(_._2!=1)){ 0 }
		
		else	 {
		  val m1val=bothlist.map(a=>prefs(man1)(a._1))
		  val m2val=bothlist.map(a=>prefs(man2)(a._1))		  
		  		  
		  println("m1val : "+m1val)
		  println("m2val : "+m2val)
		  
		  val gapsquared=m1val.zip(m2val).map{
		    a=>pow(a._1-a._2,2)
		  }
		  
		  println("gapsquared : "+gapsquared)
		  
		  val sum=gapsquared.sum
		  
		  println("sum : "+sum)
		  
		  1/(1+sqrt(sum))
		}
	}
	
	
}

object recotest extends App{
  val r=new recom
  println(r.data("Lisa Rose"))
  println(r.data("Gene Seymour"))
  println(r.sim_distance(r.data, "Lisa Rose", "Gene Seymour"))
}