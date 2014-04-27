package probability
        
import scala.collection.mutable.Set
import scala.collection.mutable.Map
import scala.math._
    
import scalearn.general.Checking
    
    case class Event(situation:String, probability:Double)

    trait Factor{
        
        val name : String
        
        val events : Vector[Event]
        
        val sumProbability :Double = events.map( event => event.probability ).sum
                        
        Checking.requireDouble(
            sumProbability,
            1.0,
            "Sum of probabilities should be 1. Error here => " + name )
            
        def sortBySituation :Vector[Event] = 
            events.sortWith( (a,b) => a.situation < b.situation )
            
        def sortByProbability : Vector[Event] = 
            events.sortWith( (a,b) => a.probability < b.probability )

            
        def indepenentWith(factor:Factor) = { }
        //Factor同士の関係性をデータから得る関数も実装する必要あり

    }

    case class TestingFactor(name:String,events:Vector[Event]) extends Factor{}

    case class SimpleFactor(name:String,events:Vector[Event]) extends Factor{

        val cpd : Vector[( (String,String) , Double )] = 
            events
                .combinations(2)
                .map{ 
                    eventPair => 
                        ( ( eventPair(0).situation , eventPair(1).situation ) , 
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
        
              
        def addFactor(factor:Fa) = {
            factors += factor
            factorReference += factor.name -> factor
        }
        
        def addRelation(relation:Re) = 
            relations += relation

        def removeFactor(factor:Fa) = {
            factors -= factor
            factorReference -= factor.name
        }
                
        def removeRelation(relation:Re) = 
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
        
    //この実装では各Factorに対応したNetworkを選ばなければいけない；異なるFactorを同じNetworkで使えない

        
    trait MarkovNetwork[Fa<:Factor] extends Network[UndirectedRelation,Fa] {
            
        val factors:Set[Fa]
                
        val relations:Set[UndirectedRelation]
                
    }

        
   case class SimpleMarkovNetwork(
       
        factors:Set[SimpleFactor],
        relations:Set[UndirectedRelation]
       
    ) extends MarkovNetwork[SimpleFactor]

        
