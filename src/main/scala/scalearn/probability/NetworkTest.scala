package probability
    
import scala.collection.mutable.Set
        
object NetworkTest extends App{

  
    val event1 = Event("get A",0.3)
    val event2 = Event("graduate successfully",0.2)
    val event3 = Event("Bad score for SAT",0.1)
        
    val eventsA = Vector(event1,event2,event3)
        
    val factor1 = SimpleFactor("first factor",eventsA)
        
//    player1.cpd.foreach(println)

    val event4 = Event("get C",0.2)
    val event5 = Event("fail to graduate",0.4)
    val event6 = Event("best score ever!!",0.3)
        
    val eventsB = Vector(event4,event5,event6)
      
    val factor2 = SimpleFactor("second factor",eventsB)
    
    println("")
        
//    player2.cpd.foreach(println)
        
    val factors = Set(factor1,factor2)
            
    val relation1 = DirectedRelation(factor1.name,factor2.name)
        
    val relations = Set(relation1)
        
    val bn = SimpleBayesianNetwork(factors,relations)
        
    println("bn : "+bn)
        
    bn.relations.foreach(println)
        
    println("relations : "+relations)
        
    println("")
        
    println("playerReference : "+bn.factorReference)

        
    val relation2 = UndirectedRelation(factor1.name,factor2.name)
        
    val mn = SimpleMarkovNetwork(factors,Set(relation2))

    println("Origine Markov Network : ")
        
    println(mn.factorReference)
        
    val factor3 = SimpleFactor("third factor",eventsB)
    
    mn.addFactor(factor3)
        
    println("After add factor : ")
        
    println(mn.factorReference)
        
    val dif_factor = TestingFactor("test factor",eventsB)
        
//    mn.addFactor(dif_factor) <= これがうまくいくようにしたい！

    mn.removeFactor(factor3)
        
    println("After delete factor : ")
        
    println(mn.factorReference)
        
    val relation3 = UndirectedRelation(factor3.name,factor1.name)
        
    mn.addRelation(relation3)
        
    println("After add relation : ")
        
    println(mn.relations)
        
    println( mn.relations.contains( relation3 ) )

    println( relation3 == UndirectedRelation(factor1.name,factor3.name) )
        
}