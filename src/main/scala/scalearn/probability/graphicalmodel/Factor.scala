package scalearn.probability.graphicalmodel

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

    case class NaiveFactor(name:String,events:Vector[Event]) extends Factor
    //Markov Network用のFactor ; 内部にCPDは持たない。計算の主体はRelationに任せる。

    case class CpdFactor(name:String,events:Vector[Event]) extends Factor{

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
    //単純なBaysian Network用のFactor ; 内部にCPDを持ち、計算の主体となる

        