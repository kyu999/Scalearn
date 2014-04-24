package probability
    
package object probability{
    
import scala.collection.mutable.Map
    
    type Situation = String
     
    type Name = String
       
        
    case class Event(situation:Situation, probability:Double)

    case class Player(events:Vector[Event], children:Vector[Player]){

        def indepenentWith(player:Player) = { }
        //player同士の関係性をデータから得る関数も実装する必要あり
    }


    trait Edge{
    
        val start : Player
    
        val end : Player
        
    }

    case class DirectedEdge(start:Player,end:Player,toChild:Boolean) extends Edge

    case class UndirectedEdge(start:Player,end:Player) extends Edge

        
    trait Network {
            
        val players : Map[Name,Player]
            
        def addPlayer(name:Name, player:Player) = 
            players +=  name -> player

    }

    case class BayesianNetwork(players:Map[Name,Player]) extends Network

    case class MarkovNetwork(players:Map[Name,Player]) extends Network

        
}