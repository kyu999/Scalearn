package scalearn.statistics.sampling
    
import scala.util.Random
import scala.collection.mutable.Map
    
class MCMC{
    
    def metropolis(
        f: Double => Double,
        initial_place: Double,
        max_iteration: Int = 1000,
        stepScale: Double = 1.0
    ): Vector[(Double,Double)] = {
        
        var counter = 1
            
        val random = new Random
            
        val holder :Map[Int,(Double,Double)] = Map(0->(0.0,0.0))    
        
        var current_place :Double = initial_place
            
        var next_place = 0.0
        
        while(counter < max_iteration){
            
            val current_value = f(current_place)
                 
            holder += counter -> (current_place,current_value)
                
            var direction = 1   
                
            if(random.nextBoolean) direction = -1
            
            val step = random.nextDouble * direction * stepScale
                
            next_place = current_place + step
                
            val ratio = f(next_place) / current_value
                
            println("current_place : "+current_place)
            println("current_value : "+current_value)
            println("step : "+step)
            println("next_place : "+next_place)
            println("next_value : "+f(next_place))
            println("ratio : "+ratio)
                
            if(ratio >= 1 | ratio >= random.nextDouble)
               current_place = next_place
            
            counter += 1
        }
        
        holder.filter( elt => elt._1 >=100 ).values.toVector
        //バーンイン部分をカットして返す
    }

}


object TestMCMC extends App{
 
    val generator = new MCMC
        
    val function = { (x: Double) => x * x * x * x } 
    
    val result = generator.metropolis(function,0)

}