package probability
        
import scala.collection.mutable.Set
       
    
    case class Event(situation:String, probability:Double)

    case class Player(events:Vector[Event]){

        val cpd : Vector[(String,Double)] = 
            events
                .combinations(2)
                .map{ 
                    eventPair => 
                        ( eventPair(0).situation + " && " + eventPair(1).situation , 
                        eventPair(0).probability * eventPair(1).probability )
                }
                .toVector
        //cpd == Conditional Probability Distribution ; event同士は独立であると仮定する
 
        def indepenentWith(player:Player) = { }
        //player同士の関係性をデータから得る関数も実装する必要あり
    
    }


    trait Relation{
    
        val start : Player
    
        val end : Player
        
    }

    case class DirectedRelation(start:Player,end:Player) extends Relation

    case class UndirectedRelation(start:Player,end:Player) extends Relation

        
    trait Network {
            
        val relations : Set[Relation]
        //mutable Set
            
        def addRelation(relation:Relation) = 
            relations += relation
        //side effect 
            
    }

    case class BayesianNetwork(relations:Set[Relation]) extends Network

    case class MarkovNetwork(relations:Set[Relation]) extends Network

        
