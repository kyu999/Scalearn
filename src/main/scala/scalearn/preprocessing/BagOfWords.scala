package scalearn.preprocessing

import scalearn.preprocessing.tokenfactory.JP._
import org.atilika.kuromoji._


trait BagOfWords// extends TokenVector
{
	
	val tokens: Vector[Token]
	val allwordtype: Set[Token]
	//↑２つとも引数
	
	val types: Set[String] = tokens.map(elt=>elt.getBaseForm).toSet
	
	val values: Vector[Double]

//allwordtypeはドキュメント全体で出現する全ての単語タイプ
    
}

case class BinaryVector(tokens:Vector[Token],allwordtype:Set[Token]) 
    extends BagOfWords{
   
	
    val values = 
        allwordtype
            .toVector
            .map{ elt =>
                if (types.contains(elt.getBaseForm)) 1.0
	            else 0.0
	        }
    
    
}

case class FrequencyVector(tokens: Vector[Token],allwordtype: Set[Token])
    extends BagOfWords{
/*全てのDocumentにおける単語タイプと、捜索すべき単語トークンを取得
 * allwordtypeを変形してreturnする
 */ 	   
	val frequency: Map[String,Double] 
        = tokens
            .groupBy( (elt=>elt.getBaseForm) )
            .toMap
            .map( elt => (elt._1,elt._2.length.toDouble) )
	//改善の余地あり
	 
	val values         //単語の出現頻度を元に作ったベクトル。
        = allwordtype
            .toVector
            .map{ elt =>
	            frequency.get(elt.getBaseForm) match{
	                case Some(x) => x+1    
                        //本来は x だが、未知語対策のため1を足している
	                case None => 1         
                        //本来は 0 だが、未知語対策のため1にしている
	            }
	        }

}