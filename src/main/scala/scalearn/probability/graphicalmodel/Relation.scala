package scalearn.probability.graphicalmodel


    trait Relation{
    
        val startFactor : String
    
        val endFactor : String
        
    }


    case class DirectedRelation(startFactor:String,endFactor:String) extends Relation{

       override def equals(that:Any):Boolean = {
            that match {
                case dr : DirectedRelation =>
                    if(startFactor == dr.startFactor && endFactor == dr.endFactor)
                        true
                    else false
                case _ => false
                }
        }
 
    
    }

    trait UndirectedRelation extends Relation {
 
        override def equals(that:Any):Boolean = {
            that match {
                case udr : UndirectedRelation =>
                    if( (startFactor == udr.startFactor && endFactor == udr.endFactor)
                        |
                        (startFactor == udr.endFactor && endFactor == udr.startFactor)
                    ) true 
                    else false
                case _ => false
                }
        }               
    
        
    }

    case class PairwiseRelation(startFactor:String,endFactor:String) 
        extends UndirectedRelation{
        
    }        