package probability
        
import scala.collection.mutable.Set
import scala.collection.mutable.Map
    
import scalearn.general._

    
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


    case class DirectedRelation(startPlayer:String,endPlayer:String) extends Relation{

       override def equals(that:Any):Boolean = {
            that match {
                case dr : DirectedRelation =>
                    if(startPlayer == dr.startPlayer && endPlayer == dr.endPlayer)
                        true
                    else false
                case _ => false
                }
        }
 
    
    }

    case class UndirectedRelation(startPlayer:String,endPlayer:String) extends Relation {
 
       override def equals(that:Any):Boolean = {
            that match {
                case udr : UndirectedRelation =>
                    if( (startPlayer == udr.startPlayer && endPlayer == udr.endPlayer)
                        |
                        (startPlayer == udr.endPlayer && endPlayer == udr.startPlayer)
                    ) true 
                    else false
                case _ => false
                }
        }               
    
        
    }

        
    trait Network[ Re<:Relation , Pl<:Player ] {
                        
        val relations : Set[Re]
        //mutable Set

        val players : Set[Pl] 
            
        val playerReference :Map[String,Pl] = //mutable Map
            Map() ++
            ( players
                .map{ player => ( player.name , player ) }
                .toMap )
        
              
        def add(player:Pl) = {
            players += player
            playerReference += player.name -> player
        }
        
        def add(relation:Re) = 
            relations += relation

        def remove(player:Pl) = {
            players -= player
            playerReference -= player.name
        }
                
        def remove(relation:Re) = 
            relations -= relation
            
        
            
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

        
