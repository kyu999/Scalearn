package scalearn.recommendation
    
        
trait Association[I]{

    def counting(buskets: Vector[Set[I]], items: Set[I]): Double = {
                  
      val items_size = items.size
          
      var frequency = 0.0
          
      buskets.foreach{ busket =>
       if( (busket & items).size == items_size ) frequency += 1
      }
        
      frequency / buskets.length
          
      }
    
    
    def uniqueFlatten(candidates: Vector[Set[I]]): Vector[I] = candidates.flatten.distinct        
        
        
    def regenerate(candidates: Vector[Set[I]], size: Int): Vector[Set[I]] = { 
        val flatted = uniqueFlatten(candidates)

        flatted.combinations(size).toVector.map(items => items.toSet)
      }
        
    
    def supportFilter(buskets: Vector[Set[I]], minimumSupport: Double): Vector[Set[I]]  = {
                          
      val filtering = { candidates: Vector[Set[I]] =>  
            
         val supports = candidates.map{ items => 
             ( counting(buskets, items) , items ) }
            
         supports
           .filter( prob_item => prob_item._1 >= minimumSupport)
           .map( prob_item => prob_item._2 ) 
       }
                  
      var pre_candidates: Vector[Set[I]] = uniqueFlatten(buskets).map{ item: I => Set(item) }
        
      var post_candidates = filtering(pre_candidates)
          
      var satisfySupport = if(post_candidates.isEmpty) false else true
        
      var size = 2
          
      var result = post_candidates
          
      while(satisfySupport){
 
          println("---- pre_candidates ----") ; pre_candidates.foreach(println)
          println("---- post_candidates ----") ;  post_candidates.foreach(println)
          
          if(post_candidates.isEmpty) { 
              satisfySupport = false 
              result = pre_candidates 
          } //if pre is empty => post also empty
              
          else {
              pre_candidates = regenerate(post_candidates, size)
                  
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
    
    
    
    def findCausality(buskets: Vector[Set[I]], candidates: Vector[Set[I]], 
                      minimumConfidence: Double): Vector[Set[I]] = 
      candidates.map{ items => 
         val candidateSupport = counting(buskets, items)
         items.filter{ item => 
            val confidence = candidateSupport / counting(buskets, Set(item)) 
            println("confidence of [ " + item + " -> " + items + " ] => " + confidence)
            confidence >= minimumConfidence }
                    }

                     
    def findRules(buskets: Vector[Set[I]], minimumSupport: Double, minimumConfidence: Double) = {
      val candidates = supportFilter(buskets, minimumSupport)
      val rules = findCausality(buskets, candidates, minimumConfidence)
      rules
    }
    
}


object StringAssociation extends Association[String]
    
    /*
    minimumSupport == minimum probability the set would appear on 
    minimumConfidence == support(X && Y) / support(X) on rule(X->Y)
    rule(X->Y) == when find X, Y also appear, and the confidence is the confidence that would likely occur
    
↓こう使ったりできる。
case class item(name: String, price: Double)
object ItemAssociation extends Association[item]

ただこの場合、同じ商品のだけど名前が違うインスタンを異なるものとして捉えるためequals methodをoverrideする必要がある。
*/
    
case class item(name: String, price: Double){ 
    
    override def equals(that: Any) = that match{
        case i: item => if(name == i.name) true else false
        case _ => false
        }
    
}
    
object ItemAssociation extends Association[item]
    

    
object TestAssociation extends App{
    
    /*
    println(
        Association.counting[String](
            Association.sample, Set("banana", "guava")) 
    )
    */
    val sample = 
        Vector(
          Set("banana", "apple"), 
          Set("apple", "guava", "orange"), 
          Set("banana", "orange", "guava"), 
          Set("banana", "orange"), 
          Set("apple","guava"), 
          Set("apple","guava", "orange"), 
          Set("apple","guava", "orange"), 
          Set("strawberry"), 
          Set("apple", "mango", "orange", "guava"),
          Set("apple", "mango", "orange", "guava"),
          Set("apple", "mango", "orange", "guava"),
          Set("apple", "mango", "orange", "guava"),
          Set("apple", "mango", "orange", "guava"),
          Set("apple", "mango", "orange", "guava"),
          Set("apple", "mango", "orange", "guava")
    )
        
    println(StringAssociation.findRules(sample, 0.7, 0.7))
        
    val items = 
       Vector(
        Set(item("banana", 120.0), item("apple", 50)),
        Set(item("apple", 70), item("guava", 30)),
        Set(item("banana", 130), item("orange", 60), item("guava", 20)),
        Set(item("banana", 150), item("orange", 40)) )
        
    println(ItemAssociation.findRules(items, 0.3,0.4))
        
        
    val personalities = 
      Vector(
        Set("brave", "considerate"), 
        Set("mean", "weak"), 
        Set("brave", "strong"), 
        Set("considerate", "weak"), 
        Set("curward", "weak","mean", "brave")
    )
        
    println(StringAssociation.findRules(personalities, 0.3, 0.5)) // get idealed result
//  StringAssociation.findRules(0.5, 0.5, personalities) <- in this case, get opposite result, yet it isn't bug. just reasonable result; rule == weak & brave. accordingly, we need to select minimum support carefully. Also, it means this algorithm doesn't fit the goals like we wanna find strong relaitonship between two variables though each of them does not occur often. Of course, if the minimum support is too low to detect patterns, we couldn't distinguish coincidences from facts
    

    /*
    
console at 0.5; 


---- pre_candidates ----
Set(considerate)
Set(weak)
Set(mean)
Set(strong)
Set(curward)
Set(brave)
---- post_candidates ----  !! here is the most important. obviously, we don't want it
Set(weak)   <- what fuck 
Set(brave)  <- lol
---- pre_candidates ----
Set(weak, brave)
---- post_candidates ----


*/
    
}