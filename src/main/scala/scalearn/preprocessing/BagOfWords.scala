package scalearn.preprocessing

import scalearn.preprocessing.tokenfactory.JP._
import org.atilika.kuromoji._


trait BagOfWords// extends TokenVector
{
	
	val tokens:Vector[Token]
	val allwordtype:Set[Token]
	//↑２つとも引数
	
	val types:Set[String] = tokens.map(elt=>elt.getBaseForm).toSet
	
	val values:Vector[Double]

//allwordtypeはドキュメント全体で出現する全ての単語タイプ
    
}

case class BinaryVector(tokens:Vector[Token],allwordtype:Set[Token]) extends BagOfWords{
   
	
    val values = 
        allwordtype
            .toVector
            .map{ elt =>
                if (types.contains(elt.getBaseForm)) 1.0
	            else 0.0
	        }
    
    
}

case class FrequencyVector(tokens:Vector[Token],allwordtype:Set[Token])  extends BagOfWords{
/*全てのDocumentにおける単語タイプと、捜索すべき単語トークンを取得
 * allwordtypeを変形してreturnする
 * 
 */ 	   
	val freqs:Map[String,Double] 
        = tokens
            .groupBy( (elt=>elt.getBaseForm) )
            .toMap
            .map( elt => (elt._1,elt._2.length.toDouble) )
	//改善の余地あり
	 
	val values
        = allwordtype
            .toVector
            .map{ elt=>
	            freqs.get(elt.getBaseForm) match{
	                case Some(x)=>x
	                case None=>0
	            }
	        }

}