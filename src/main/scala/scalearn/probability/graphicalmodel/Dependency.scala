package scalearn.probability.graphicalmodel


    trait Dependency{
    
        val startFactor : String
    
        val endFactor : String
        
    }


    case class DirectedDependency(startFactor:String,endFactor:String) extends Dependency{

       override def equals(that:Any):Boolean = {
            that match {
                case dr : DirectedDependency =>
                    if(startFactor == dr.startFactor && endFactor == dr.endFactor)
                        true
                    else false
                case _ => false
                }
        }
 
    
    }

    trait UndirectedDependency extends Dependency {
 
        override def equals(that:Any):Boolean = {
            that match {
                case udr : UndirectedDependency =>
                    if( (startFactor == udr.startFactor && endFactor == udr.endFactor)
                        |
                        (startFactor == udr.endFactor && endFactor == udr.startFactor)
                    ) true 
                    else false
                case _ => false
                }
        }               
    
        
    }

    case class PairwiseDependency(startFactor:String,endFactor:String) 
        extends UndirectedDependency{
        
    }        