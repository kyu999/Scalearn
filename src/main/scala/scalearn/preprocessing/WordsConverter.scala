package scalearn.preprocessing

import scalearn.preprocessing.tokenfactory.JP._
import org.atilika.kuromoji._
import scala.collection.mutable.ArrayBuffer


object WordsConverter {
  
    def toMathVector(document:String,all_docuemnts:String) = 
        FrequencyVector(
            mkToken(document).toVector , 
            mkToken(all_docuemnts).toSet
	  	).values
	  			
	
}