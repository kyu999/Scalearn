package scalearn.classification.supervised

import scalearn.statistics.Descriptive
import scala.math._

trait LinearRegression extends Descriptive{

   //最急降下法
   def BatchGradientDescent(
       x:Vector[Double],
       y:Vector[Double],
       alpha:Double,
       inferedTheta0:Double,
       inferedTheta1:Double
   ):(Double,Double)={
	  
      val size = y.length
	  val zipped = y.zip(x)
	  
      var tempTheta0 = 0.0
	  var tempTheta1 = 0.0
          
	  var theta0 = inferedTheta0
	  var theta1 = inferedTheta1
          
	  var check = true
	  var counter = 0
	  
	  while (check){
	    
	    tempTheta0 = 
            theta0 - alpha * (zipped.map(a=>theta0+theta1*a._1-a._2).sum)/size
	
        tempTheta1 = 
            theta1 - alpha * (zipped.map(a=>a._1*(theta0+theta1*a._1-a._2)).sum)/size
	    
	    if( abs(theta0-tempTheta0) < alpha*0.01 && abs(theta1-tempTheta1) < alpha*0.01)
            check = false
	    
	    theta0 = tempTheta0
	    theta1 = tempTheta1
	   
	    println(
            "tempTheta0 : " + tempTheta0
            + " , tempTheta1 : " + tempTheta1 
            + " ,  Theta0 : " + theta0
            + " , Theta1 : "+theta1
        )
	    
	    counter = counter+1
	    
	    if(counter>200000) check = false
	    
	  }
      
	  (theta0,theta1)
	  
	}
   //h(Theta0,Theta1) == Theta0+Theta1*x
}