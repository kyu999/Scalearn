package probability
    
import scala.collection.mutable.Set
        
object NetworkTest extends App{
    
    val event1 = Event("get A",0.3)
    val event2 = Event("graduate successfully",0.2)
    val event3 = Event("Bad score for SAT",0.1)
        
    val eventsA = Vector(event1,event2,event3)
        
    val player1 = Player(eventsA)
        
    player1.cpd.foreach(println)

    val event4 = Event("get C",0.2)
    val event5 = Event("fail to graduate",0.4)
    val event6 = Event("best score ever!!",0.3)
        
    val eventsB = Vector(event4,event5,event6)
      
    val player2 = Player(eventsB)
    
    println("")
        
    player2.cpd.foreach(println)
            
    val relation1 = DirectedRelation(player1,player2)
        
    val bn = BayesianNetwork(Set(relation1)) 
    
    println("BN : "+bn)
            
    bn.relations.foreach(println)
}