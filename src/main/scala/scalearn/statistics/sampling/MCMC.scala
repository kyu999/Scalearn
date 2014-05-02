package scalearn.statistics.sampling
    
import scala.util.Random
import scala.collection.mutable.Map
    
class MCMC{
    
    def metropolis(
        f:Double => Double,
        initial_place : Double,
        max_iteration :Int = 1000
    ):Vector[(Double,Double)] = {
        
        var counter = 1
            
        val random = new Random
            
        val holder :Map[Int,(Double,Double)] = Map(0->(0.0,0.0))    
        
        var current_place :Double = initial_place
            
        var next_place : Double = 0.0
        
        while(counter < max_iteration){
            
            val current_value = f(current_place)
                 
            holder += counter -> (current_place,current_value)
            
            val step = random.nextDouble
                
            next_place = current_place + step
                
            val ratio = f(next_place) / current_value
                
            if(ratio >= 1 | ratio >= random.nextDouble)
               current_place = next_place
            
            counter += 1
        }
        
        holder.filter( elt => elt._1 >=100 ).values.toVector
        //バーンイン部分をカットして返す
    }

}