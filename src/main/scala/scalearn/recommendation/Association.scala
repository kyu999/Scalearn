package scalearn.recommendation
    
    
case class item(name: String, price: Double)
    
object Association{
    
    val sample = 
        Vector(
          Set("banana", "apple"), 
          Set("apple", "guava", "orange"), 
          Set("banana", "orange", "guava"), 
          Set("banana", "orange"), 
          Set("apple","guava"), 
          Set("apple","guava", "orange"), 
          Set("apple","guava", "orange")
    )

    def counting[I](buskets: Vector[Set[I]], items: Set[I]): Double = {
                  
      val items_size = items.size
          
      var frequency = 0.0
          
      buskets.foreach{ busket =>
       if( (busket & items).size == items_size ) frequency += 1
      }
        
      frequency / buskets.length
          
      }
        
    def findRules[I](minimumSupport: Double, minimumConfidence: Double, 
                     buskets: Vector[Set[I]]) = {
        
      val findSupports = { candidates: Vector[Set[I]] =>  
            candidates.map{ item => 
             ( counting[I](buskets, item) , item ) }
        }
        
      val filtering = { candidates: Vector[Set[I]] =>  
            
         val supports = findSupports(candidates)
            
         supports
           .filter( prob_item => prob_item._1 >= minimumSupport)
           .map( prob_item => prob_item._2 ) 
       }
        
      val addKinds = { (candidates: Vector[Set[I]], size: Int) => 
        val flatted = candidates.flatten.toSet.toVector
          
        flatted.combinations(size).toVector.map(elt=>elt.toSet)
      }
          
        
      var pre_candidates: Vector[Set[I]] = 
          buskets.flatten.toSet.toVector.map{ item: I => Set(item) }
        
      var post_candidates: Vector[Set[I]] = filtering(pre_candidates)
          
      var satisfySupport = if(post_candidates.isEmpty) false else true
        
      var size = 1
          
      var result = post_candidates
          
      //either items satisfy minimumSupport
      while(satisfySupport){
 
          if(pre_candidates.isEmpty) {
              satisfySupport = false 
              result = post_candidates
          }
              
          else {
              pre_candidates = addKinds(post_candidates, size)
                  
              if(pre_candidates.isEmpty) {
                  satisfySupport = false
                  result = post_candidates
              }
              
              else post_candidates = filtering(pre_candidates)
                    
              size += 1
              
          }
      }

      result
                
    }
    
    /*
    minimumSupport == minimum probability the set would appear on 
    minimumConfidence == support(X && Y) / support(X) on rule(X->Y)
    rule(X->Y) == when find X, Y also appear, and the confidence is the confidence that would likely occur
    
     Vector(
        Set(item("banana", 120.0), item("apple", 50)), 
        Set(item("apple", 70), item("guava", 30)), 
        Set(item("banana", 130), item("orange", 60), item("guava", 20)), 
        Set(item("banana", 150), item("orange", 40)) )
     
    */

}

object TestAssociation extends App{
    
    /*
    println(
        Association.counting[String](
            Association.sample, Set("banana", "guava")) 
    )
    */
    
    println(
        Association.findRules[String](0.2, 0.7, Association.sample)
    )
        
        
}