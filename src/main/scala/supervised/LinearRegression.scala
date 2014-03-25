package supervised
import datafactory.Descriptive
import scala.math._
trait LinearRegression extends Descriptive{

   def BatchGradientDescent(x:Vector[Double],y:Vector[Double],α:Double,expθ0:Double,expθ1:Double):(Double,Double)={
	  
      val m=y.length
      val mean=y.reduce((a,b)=>a+b)/m
	  val zipped=y.zip(x)
	  
      var tempθ0=0.0
	  var tempθ1=0.0
	  var θ0=expθ0
	  var θ1=expθ1
	  var check=true
	  var counter=0
	  
	  while (check){
	    
	    tempθ0=θ0-α*(zipped.map(a=>θ0+θ1*a._1-a._2).reduce((a,b)=>a+b))/m
	    tempθ1=θ1-α*(zipped.map(a=>a._1*(θ0+θ1*a._1-a._2)).reduce((a,b)=>a+b))/m
	    
	    if(abs(θ0-tempθ0)<α*0.01 && abs(θ1-tempθ1)<α*0.01) check=false
	    
	    θ0=tempθ0
	    θ1=tempθ1
	   
	    println("tempθ0 : "+tempθ0+" , tempθ1 : "+tempθ1+" ,  θ0 : "+θ0+" , θ1 : "+θ1)
	    
	    counter=counter+1
	    
	    if(counter>200000) check=false
	    
	  }
      
	  (θ0,θ1)
	  
	}
   //h(θ0,θ1) == θ0+θ1*x
}