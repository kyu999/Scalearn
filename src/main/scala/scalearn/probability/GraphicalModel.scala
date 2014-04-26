package probability
        
import scala.collection.mutable.Set
import scala.collection.mutable.Map

    
    
    case class Event(situation:String, probability:Double)

    trait Player{
        
        val name : String
        
        val events : Vector[Event]
  
        def indepenentWith(player:Player) = { }
        //player同士の関係性をデータから得る関数も実装する必要あり

    }

    case class SimplePlayer(name:String,events:Vector[Event]) extends Player{

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
    
        val startPlayer : String
    
        val endPlayer : String
        
    }


    case class DirectedRelation(startPlayer:String,endPlayer:String) extends Relation

    case class UndirectedRelation(startPlayer:String,endPlayer:String) extends Relation

        
    trait Network[ Re<:Relation , Pl<:Player ] {
                        
        val relations : Set[Re]
        //mutable Set

        val players : Set[Pl] 
            
        val playerReference = 
            players
                .map{ player => ( player.name , player ) }
                .toMap  //immutable Map
        
              
        def addPlayer(player:Pl) = 
            players += player
                    
        def addRelation(relation:Re) = 
            relations += relation
        //side effect 
            
    }


    trait BayesianNetwork[Pl<:Player] extends Network[DirectedRelation,Pl] {
            
        val players:Set[Pl]
                
        val relations:Set[DirectedRelation]
                
    }
           

    case class SimpleBayesianNetwork(
        
        players:Set[SimplePlayer],
        relations:Set[DirectedRelation]
        
    ) extends BayesianNetwork[SimplePlayer]

        
    trait MarkovNetwork[Pl<:Player] extends Network[UndirectedRelation,Pl] {
            
        val players:Set[Pl]
                
        val relations:Set[UndirectedRelation]
                
    }

        
   case class SimpleMarkovNetwork(
       
        players:Set[SimplePlayer],
        relations:Set[UndirectedRelation]
       
    ) extends MarkovNetwork[SimplePlayer]

        
