package preprocessing

import tokenfactory.JP._
import org.atilika.kuromoji._
import scala.collection.mutable.ArrayBuffer


object NLP {
  
	def toMathVector(document:String,allwordtype:String)={
	  
	  	FrequencyVector(
	  	    mkToken(document).toVector , mkToken(allwordtype).toSet
	  	    ).values
	  			

	}
}