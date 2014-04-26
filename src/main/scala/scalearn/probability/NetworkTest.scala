package probability
    
import scala.collection.mutable.Set
        
object NetworkTest extends App{

  
    val event1 = Event("get A",0.3)
    val event2 = Event("graduate successfully",0.2)
    val event3 = Event("Bad score for SAT",0.1)
        
    val eventsA = Vector(event1,event2,event3)
        
    val player1 = SimplePlayer("first player",eventsA)
        
//    player1.cpd.foreach(println)

    val event4 = Event("get C",0.2)
    val event5 = Event("fail to graduate",0.4)
    val event6 = Event("best score ever!!",0.3)
        
    val eventsB = Vector(event4,event5,event6)
      
    val player2 = SimplePlayer("second player",eventsB)
    
    println("")
        
//    player2.cpd.foreach(println)
        
    val players = Set(player1,player2)
            
    val relation1 = DirectedRelation(player1.name,player2.name)
        
    val relations = Set(relation1)
        
    val bn = SimpleBayesianNetwork(players,relations)
        
    println("bn : "+bn)
        
    bn.relations.foreach(println)
        
    println("relations : "+relations)
        
    println("")
        
    println("playerReference : "+bn.playerReference)

        
    val relation2 = UndirectedRelation(player1.name,player2.name)
        
    val mn = SimpleMarkovNetwork(players,Set(relation2))

    println("Origine Markov Network : ")
        
    println(mn.playerReference)
        
    val player3 = SimplePlayer("third player",eventsB)
    
    mn.add(player3)
        
    println("After add player : ")
        
    println(mn.playerReference)

    mn.remove(player3)
        
    println("After delete player : ")
        
    println(mn.playerReference)
        
    val relation3 = UndirectedRelation(player3.name,player1.name)
        
    mn.add(relation3)
        
    println("After add relation : ")
        
    println(mn.relations)
        
    println( mn.relations.contains( relation3 ) )

    println( relation3 == UndirectedRelation(player1.name,player3.name) )
        
}