package probability
        
import scala.collection.mutable.Set
import scala.collection.mutable.Map
    
import scalearn.general._

    
    case class Event(situation:String, probability:Double)

    trait Factor{
        
        val name : String
        
        val events : Vector[Event]
  
        def indepenentWith(factor:Factor) = { }
        //Factor同士の関係性をデータから得る関数も実装する必要あり

    }

    case class SimpleFactor(name:String,events:Vector[Event]) extends Factor{

        val cpd : Vector[(String,Double)] = 
            events
                .combinations(2)
                .map{ 
                    eventPair => 
                        ( eventPair(0).situation + "_" + eventPair(1).situation , 
                        eventPair(0).probability * eventPair(1).probability )
                }
                .toVector
        //cpd == Conditional Probability Distribution ; event同士は独立であると仮定する
     
    }


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

    case class UndirectedRelation(startFactor:String,endFactor:String) extends Relation {
 
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

        
    trait Network[ Re<:Relation , Fa<:Factor ] {
                        
        val relations : Set[Re]
        //mutable Set

        val factors : Set[Fa] 
            
        val factorReference :Map[String,Fa] = //mutable Map
            Map() ++
            ( factors
                .map{ factor => ( factor.name , factor ) }
                .toMap )
        
              
        def add(factor:Fa) = {
            factors += factor
            factorReference += factor.name -> factor
        }
        
        def add(relation:Re) = 
            relations += relation

        def remove(factor:Fa) = {
            factors -= factor
            factorReference -= factor.name
        }
                
        def remove(relation:Re) = 
            relations -= relation
            
        
            
    }


    trait BayesianNetwork[Fa<:Factor] extends Network[DirectedRelation,Fa] {
            
        val factors:Set[Fa]
                
        val relations:Set[DirectedRelation]
                
    }
           

    case class SimpleBayesianNetwork(
        
        factors:Set[SimpleFactor],
        relations:Set[DirectedRelation]
        
    ) extends BayesianNetwork[SimpleFactor]

        
    trait MarkovNetwork[Fa<:Factor] extends Network[UndirectedRelation,Fa] {
            
        val factors:Set[Fa]
                
        val relations:Set[UndirectedRelation]
                
    }

        
   case class SimpleMarkovNetwork(
       
        factors:Set[SimpleFactor],
        relations:Set[UndirectedRelation]
       
    ) extends MarkovNetwork[SimpleFactor]

        
